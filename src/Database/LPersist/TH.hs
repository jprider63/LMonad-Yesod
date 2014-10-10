{-# LANGUAGE CPP #-}

-- Reference:
--  https://github.com/jprider63/persistent/blob/master/persistent-template/Database/Persist/TH.hs
--
-- TODO: licensing things... XXX









module Database.LPersist.TH where

import Data.Monoid
import Language.Haskell.TH.Syntax

import Database.LPersist.Quasi
import Database.LPersist.Types

lShare :: [[LEntityDef] -> Q [Dec]] -> [LEntityDef] -> Q [Dec]
lShare lFs x = fmap mconcat $ mapM ($ x) lFs

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
lPersistWith :: PersistSettings -> QuasiQuoter
lPersistWith ps = QuasiQuoter
    { quoteExp = lParseReferences ps . pack
    }

-- | Apply 'lPersistWith' to 'upperCaseSettings'.
lPersistUpperCase :: QuasiQuoter
lPersistUpperCase = lPersistWith upperCaseSettings

-- | Apply 'lPersistWith' to 'lowerCaseSettings'.
lPersistLowerCase :: QuasiQuoter
lPersistLowerCase = lPersistWith lowerCaseSettings

-- | Same as 'lPersistWith', but uses an external file instead of a
-- quasiquotation.
lPersistFileWith :: PersistSettings -> FilePath -> Q Exp
lPersistFileWith ps fp = do
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    h <- qRunIO $ SIO.openFile fp SIO.ReadMode
    qRunIO $ SIO.hSetEncoding h SIO.utf8_bom
    s <- qRunIO $ TIO.hGetContents h
    lParseReferences ps s

-- calls parse to Quasi.parse individual entities in isolation
-- afterwards, sets references to other entities
lParseReferences :: PersistSettings -> Text -> Q Exp
lParseReferences ps s = lift $
     map (mkEntityDefSqlTypeExp entityMap) entsWithEmbeds
  where
    -- every EntityDef could reference each-other (as an EmbedRef)
    -- let Haskell tie the knot
    entityMap = M.fromList $ map (\ent -> (lEntityHaskell ent, toEmbedEntityDef ent)) entsWithEmbeds
    entsWithEmbeds = map setEmbedEntity rawEnts
    setEmbedEntity ent = ent
      { entityFields = map (setEmbedField entityMap) $ entityFields ent
      }
    rawEnts = lParse ps s
