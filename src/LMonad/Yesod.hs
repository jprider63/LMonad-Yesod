-- TODO: Some licensing stuff (http://hackage.haskell.org/package/yesod-core-1.4.3/docs/src/Yesod-Core-Class-Yesod.html)



{-# LANGUAGE FlexibleContexts, TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies #-}
module LMonad.Yesod where

import Control.Monad (forM)
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Monoid (Last(..), mempty)
import Data.Text (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad
import Text.Blaze ( customAttribute, textTag, toValue, (!))
import qualified Text.Blaze.Html5 as TBH
import Text.Julius
import Yesod.Core
import Yesod.Core.Types
import qualified Yesod.Core.Widget as Yesod

widgetToPageContent :: (Label l, LMonad (HandlerT site IO), LMonad (WidgetT site IO), Yesod site) => LMonadT l (WidgetT site IO) () -> LMonadT l (HandlerT site IO) (PageContent (Route site))
widgetToPageContent = swapBase $ \w -> do
    ( res, ((),new)) <- widgetToPageContent' w
    return (res, new)
    
    where
        widgetToPageContent' :: ((Eq (Route site)), (Yesod site)) => WidgetT site IO a -> HandlerT site IO (PageContent (Route site), a)
        widgetToPageContent' w = do
            master <- getYesod
            hd <- HandlerT return
            (res, GWData (Body body) (Last mTitle) scripts' stylesheets' style jscript (Head head')) <- lift $ unWidgetT w hd
            let title = maybe mempty unTitle mTitle
                scripts = runUniqueList scripts'
                stylesheets = runUniqueList stylesheets'

            render <- getUrlRenderParams
            let renderLoc x =
                    case x of
                        Nothing -> Nothing
                        Just (Left s) -> Just s
                        Just (Right (u, p)) -> Just $ render u p
            css <- forM (Map.toList style) $ \(mmedia, content) -> do
                let rendered = toLazyText $ content render
                x <- addStaticContent "css" "text/css; charset=utf-8"
                   $ encodeUtf8 rendered
                return (mmedia,
                    case x of
                        Nothing -> Left $ preEscapedToMarkup rendered
                        Just y -> Right $ either id (uncurry render) y)
            jsLoc <-
                case jscript of
                    Nothing -> return Nothing
                    Just s -> do
                        x <- addStaticContent "js" "text/javascript; charset=utf-8"
                           $ encodeUtf8 $ renderJavascriptUrl render s
                        return $ renderLoc x

            -- modernizr should be at the end of the <head> http://www.modernizr.com/docs/#installing
            -- the asynchronous loader means your page doesn't have to wait for all the js to load
            let (mcomplete, asyncScripts) = asyncHelper render scripts jscript jsLoc
                regularScriptLoad = [hamlet|
                    $newline never
                    $forall s <- scripts
                        ^{mkScriptTag s}
                    $maybe j <- jscript
                        $maybe s <- jsLoc
                            <script src="#{s}">
                        $nothing
                            <script>^{jelper j}
                |]

                headAll = [hamlet|
                    $newline never
                    \^{head'}
                    $forall s <- stylesheets
                        ^{mkLinkTag s}
                    $forall s <- css
                        $maybe t <- right $ snd s
                            $maybe media <- fst s
                                <link rel=stylesheet media=#{media} href=#{t}>
                            $nothing
                                <link rel=stylesheet href=#{t}>
                        $maybe content <- left $ snd s
                            $maybe media <- fst s
                                <style media=#{media}>#{content}
                            $nothing
                                <style>#{content}
                    $case jsLoader master
                      $of BottomOfBody
                      $of BottomOfHeadAsync asyncJsLoader
                          ^{asyncJsLoader asyncScripts mcomplete}
                      $of BottomOfHeadBlocking
                          ^{regularScriptLoad}
                |]
            let bodyScript = [hamlet|
                    $newline never
                    ^{body}
                    ^{regularScriptLoad}
                |]

            return (( PageContent title headAll $
                    case jsLoader master of
                        BottomOfBody -> bodyScript
                        _ -> body
                ), res)

        renderLoc' render' (Local url) = render' url []
        renderLoc' _ (Remote s) = s

        addAttr x (y, z) = x ! customAttribute (textTag y) (toValue z)
        mkScriptTag (Script loc attrs) render' =
            foldl' addAttr TBH.script (("src", renderLoc' render' loc) : attrs) $ return ()
        mkLinkTag (Stylesheet loc attrs) render' =
            foldl' addAttr TBH.link
                ( ("rel", "stylesheet")
                : ("href", renderLoc' render' loc)
                : attrs
                )

        runUniqueList :: Eq x => UniqueList x -> [x]
        runUniqueList (UniqueList x) = nub $ x []

        jelper :: JavascriptUrl url -> HtmlUrl url
        jelper = fmap jsToHtml

        jsToHtml :: Javascript -> Html
        jsToHtml (Javascript b) = preEscapedToMarkup $ toLazyText b
        
        left :: Either a b -> Maybe a
        left (Left x) = Just x
        left _ = Nothing
        
        right :: Either a b -> Maybe b
        right (Right x) = Just x
        right _ = Nothing

        asyncHelper :: (url -> [x] -> Text)
                 -> [Script (url)]
                 -> Maybe (JavascriptUrl (url))
                 -> Maybe Text
                 -> (Maybe (HtmlUrl url), [Text])
        asyncHelper render scripts jscript jsLoc =
            (mcomplete, scripts'')
          where
            scripts' = map goScript scripts
            scripts'' =
                case jsLoc of
                    Just s -> scripts' ++ [s]
                    Nothing -> scripts'
            goScript (Script (Local url) _) = render url []
            goScript (Script (Remote s) _) = s
            mcomplete =
                case jsLoc of
                    Just{} -> Nothing
                    Nothing ->
                        case jscript of
                            Nothing -> Nothing
                            Just j -> Just $ jelper j

handlerToWidget :: (Label l, LMonad (HandlerT site IO), LMonad (WidgetT site IO)) => LMonadT l (HandlerT site IO) a -> LMonadT l (WidgetT site IO) a
handlerToWidget = swapBase Yesod.handlerToWidget

whamlet = QuasiQuoter { quoteExp = \s -> quoteExp Yesod.whamlet s >>= return . (AppE (VarE 'lLift)) }

extractWidget :: (Label l, LMonad (WidgetT site IO)) => LMonadT l (WidgetT site IO) () -> LMonadT l (WidgetT site IO) (WidgetT site IO ())
extractWidget = swapBase f
    where
        f :: (WidgetT site IO ((), l)) -> WidgetT site IO (WidgetT site IO (), l)
        -- f (WidgetT w) = WidgetT $ \h -> do
        --     (((), s),g) <- w h
        --     return ((WidgetT (\i -> do
        --             (((),_),h) <- w i
        --             return ((),mappend h g)
        --         ), s), g)
        f (WidgetT w) = WidgetT $ \h -> do
            (((), s),g) <- w h
            return ((WidgetT (\_ -> do
                    -- (((),_),g) <- w i
                    return ((),g)
                ), s), mempty)

instance (MonadResource m, Label l, LMonad m) => MonadResource (LMonadT l m) where
    liftResourceT = lLift . liftResourceT

instance (MonadHandler m, Label l, LMonad m) => MonadHandler (LMonadT l m) where
    type HandlerSite (LMonadT l m) = HandlerSite m
    liftHandlerT = lLift . liftHandlerT

instance (MonadWidget m, Label l, LMonad m) => MonadWidget (LMonadT l m) where
    liftWidgetT = lLift . liftWidgetT

