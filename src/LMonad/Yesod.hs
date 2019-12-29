-- TODO: Some licensing stuff (http://hackage.haskell.org/package/yesod-core-1.4.3/docs/src/Yesod-Core-Class-Yesod.html)


{-# LANGUAGE FlexibleContexts, TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies #-}
module LMonad.Yesod where

import Control.Monad (forM)
import Control.Monad.Logger
import Data.IORef
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Monoid (Last(..), mempty)
import Data.Text (Text)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder as TLB
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import LMonad
import LMonad.TCB
import Text.Blaze ( customAttribute, textTag, toValue, (!))
import qualified Text.Blaze.Html5 as TBH
import Text.Julius
import qualified Text.Hamlet as TH
import Yesod.Core
import Yesod.Core.Types
import qualified Yesod.Core.Widget as Yesod

widgetToPageContent :: (Label l, LMonad (HandlerT site IO), LMonad (WidgetT site IO), Yesod site) => LMonadT l (WidgetT site IO) () -> LMonadT l (HandlerT site IO) (PageContent (Route site))
widgetToPageContent = swapBase $ \w -> do
    ( res, ((),new)) <- widgetToPageContent' w
    return (res, new)
    
    where
        widgetToPageContent' w = do
          jsAttrs <- jsAttributesHandler
          HandlerFor $ \hd -> do
          master <- unHandlerFor getYesod hd
          ref <- newIORef mempty
          res <- unWidgetFor w WidgetData
            { wdRef = ref
            , wdHandler = hd
            }
          GWData (Body body) (Last mTitle) scripts' stylesheets' style jscript (Head head') <- readIORef ref
          let title = maybe mempty unTitle mTitle
              scripts = runUniqueList scripts'
              stylesheets = runUniqueList stylesheets'
        
          flip unHandlerFor hd $ do
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
                            <script src="#{s}" *{jsAttrs}>
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
        
            return ( PageContent title headAll $
                case jsLoader master of
                    BottomOfBody -> bodyScript
                    _ -> body
              , res)
          where
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
        -- widgetToPageContent' :: ((Eq (Route site)), (Yesod site)) => WidgetT site IO a -> HandlerT site IO (PageContent (Route site), a)
        -- widgetToPageContent' w = do
        --     master <- getYesod
        --     -- hd <- HandlerFor return
        --     (WidgetData wdRef _) <- unWidgetFor w
        --     (res, GWData (Body body) (Last mTitle) scripts' stylesheets' style jscript (Head head')) <- lift $ unWidgetFor w
        --     let title = maybe mempty unTitle mTitle
        --         scripts = runUniqueList scripts'
        --         stylesheets = runUniqueList stylesheets'

        --     render <- getUrlRenderParams
        --     let renderLoc x =
        --             case x of
        --                 Nothing -> Nothing
        --                 Just (Left s) -> Just s
        --                 Just (Right (u, p)) -> Just $ render u p
        --     css <- forM (Map.toList style) $ \(mmedia, content) -> do
        --         let rendered = toLazyText $ content render
        --         x <- addStaticContent "css" "text/css; charset=utf-8"
        --            $ encodeUtf8 rendered
        --         return (mmedia,
        --             case x of
        --                 Nothing -> Left $ preEscapedToMarkup rendered
        --                 Just y -> Right $ either id (uncurry render) y)
        --     jsLoc <-
        --         case jscript of
        --             Nothing -> return Nothing
        --             Just s -> do
        --                 x <- addStaticContent "js" "text/javascript; charset=utf-8"
        --                    $ encodeUtf8 $ renderJavascriptUrl render s
        --                 return $ renderLoc x

        --     -- modernizr should be at the end of the <head> http://www.modernizr.com/docs/#installing
        --     -- the asynchronous loader means your page doesn't have to wait for all the js to load
        --     let (mcomplete, asyncScripts) = asyncHelper render scripts jscript jsLoc
        --         regularScriptLoad = [Y.hamlet|
        --             $newline never
        --             $forall s <- scripts
        --                 ^{mkScriptTag s}
        --             $maybe j <- jscript
        --                 $maybe s <- jsLoc
        --                     <script src="#{s}">
        --                 $nothing
        --                     <script>^{jelper j}
        --         |]

        --         headAll = [Y.hamlet|
        --             $newline never
        --             \^{head'}
        --             $forall s <- stylesheets
        --                 ^{mkLinkTag s}
        --             $forall s <- css
        --                 $maybe t <- right $ snd s
        --                     $maybe media <- fst s
        --                         <link rel=stylesheet media=#{media} href=#{t}>
        --                     $nothing
        --                         <link rel=stylesheet href=#{t}>
        --                 $maybe content <- left $ snd s
        --                     $maybe media <- fst s
        --                         <style media=#{media}>#{content}
        --                     $nothing
        --                         <style>#{content}
        --             $case jsLoader master
        --               $of BottomOfBody
        --               $of BottomOfHeadAsync asyncJsLoader
        --                   ^{asyncJsLoader asyncScripts mcomplete}
        --               $of BottomOfHeadBlocking
        --                   ^{regularScriptLoad}
        --         |]
        --     let bodyScript = [Y.hamlet|
        --             $newline never
        --             ^{body}
        --             ^{regularScriptLoad}
        --         |]

        --     return (( PageContent title headAll $
        --             case jsLoader master of
        --                 BottomOfBody -> bodyScript
        --                 _ -> body
        --         ), res)

        -- renderLoc' render' (Local url) = render' url []
        -- renderLoc' _ (Remote s) = s

        -- addAttr x (y, z) = x ! customAttribute (textTag y) (toValue z)
        -- mkScriptTag (Script loc attrs) render' =
        --     foldl' addAttr TBH.script (("src", renderLoc' render' loc) : attrs) $ return ()
        -- mkLinkTag (Stylesheet loc attrs) render' =
        --     foldl' addAttr TBH.link
        --         ( ("rel", "stylesheet")
        --         : ("href", renderLoc' render' loc)
        --         : attrs
        --         )

        -- runUniqueList :: Eq x => UniqueList x -> [x]
        -- runUniqueList (UniqueList x) = nub $ x []

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



class ToLWidget l site a where
    toLWidget :: a -> LMonadT l (WidgetT site IO) ()

instance ToLWidget l site (LMonadT l (WidgetT site IO) ()) where
    toLWidget = id

instance (Label l, LMonad (WidgetT site IO)) => ToLWidget l site (WidgetT site IO ()) where
    toLWidget = lLift

instance (Label l, LMonad (WidgetT site IO)) => ToLWidget l site TLB.Builder where
    toLWidget = LMonadT . lift . toWidget . toHtml

instance (Label l, LMonad (WidgetT site IO)) => ToLWidget l site Html where
    toLWidget = LMonadT . lift . toWidget

-- instance (Label l, LMonad (WidgetT site IO)) => ToLWidget l site (HtmlUrl (Route site)) where
--     toLWidget = LMonadT . lift . toWidget

-- instance (Label l, LMonad (WidgetT site IO), ToWidget site a) => ToLWidget l site a where
--     toLWidget = lLift . toWidget

-- whamlet = QuasiQuoter { quoteExp = \s -> quoteExp Yesod.whamlet s >>= return . (AppE (VarE 'lLift)) }
whamlet = TH.hamletWithSettings rules TH.defaultHamletSettings

asLWidgetT :: LMonadT l (WidgetT site IO) () -> LMonadT l (WidgetT site IO) ()
asLWidgetT = id

-- From: https://hackage.haskell.org/package/yesod-core-1.6.17/docs/src/Yesod.Core.Widget.html#whamlet
rules :: Q TH.HamletRules
rules = do
    -- ah <- [|asWidgetT . toWidget|]
    ah <- [|asLWidgetT . toLWidget|]
    let helper qg f = do
            x <- newName "urender"
            e <- f $ VarE x
            let e' = LamE [VarP x] e
            g <- qg
            bind <- [|(>>=)|]
            return $ InfixE (Just g) bind (Just e')
    let ur f = do
            let env = TH.Env
                    (Just $ helper [|getUrlRenderParams|])
                    (Just $ helper [|fmap (toHtml .) getMessageRender|])
            f env
    return $ TH.HamletRules ah ur $ \_ b -> return $ ah `AppE` b

-- -- TODO: This seems wrong?
-- extractWidget :: (Label l, LMonad (WidgetT site IO)) => LMonadT l (WidgetT site IO) () -> LMonadT l (WidgetT site IO) (WidgetT site IO ())
-- extractWidget = swapBase f
--     where
--         -- f :: (WidgetT site IO ((), l)) -> WidgetT site IO (WidgetT site IO (), l)
--         -- -- f (WidgetT w) = WidgetT $ \h -> do
--         -- --     (((), s),g) <- w h
--         -- --     return ((WidgetT (\i -> do
--         -- --             (((),_),h) <- w i
--         -- --             return ((),mappend h g)
--         -- --         ), s), g)
--         -- f (WidgetFor w) = WidgetFor $ \h -> do
--         --     (((), s),g) <- w h
--         --     return ((WidgetFor (\_ -> do
--         --             -- (((),_),g) <- w i
--         --             return ((),g)
--         --         ), s), mempty)
--         f :: (WidgetT site IO ((), l)) -> WidgetT site IO (WidgetT site IO (), l)
--         f (WidgetFor w) = WidgetFor $ \h -> do
--             ((), g) <- w h
--             return (WidgetFor (\_ -> do
--                     -- (((),_),g) <- w i
--                     return ()
--                 ), g)

instance (MonadResource m, Label l, LMonad m) => MonadResource (LMonadT l m) where
    liftResourceT = lLift . liftResourceT

instance (MonadLogger m, Label l, LMonad m) => MonadLogger (LMonadT l m) where
    monadLoggerLog a b c d = lLift $ monadLoggerLog a b c d

instance (MonadHandler m, Label l, LMonad m) => MonadHandler (LMonadT l m) where
    type HandlerSite (LMonadT l m) = HandlerSite m
    liftHandler = lLift . liftHandler

instance (MonadWidget m, Label l, LMonad m) => MonadWidget (LMonadT l m) where
    liftWidget = lLift . liftWidget

