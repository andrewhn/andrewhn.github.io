-- | This module allows specification of javascript requirements in metadata.
-- It assumes javascript files live in folder called "js" in the root site
-- directory.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Requires
    ( getRequires
    , requiresField
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                   (forM)
import           Data.List                       (intercalate, intersperse,
                                                  sortBy)
import qualified Data.Map                        as M
import           Data.Monoid                     (mconcat)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Rules
import           Hakyll.Core.Util.String
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Html

--------------------------------------------------------------------------------
getRequires :: MonadMetadata m => Identifier -> m [String]
getRequires identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup "requires" metadata

requiresField :: String -> Context a
requiresField key = field key $ \item -> do
  reqs' <- getRequires $ itemIdentifier item
  scripts <- forM reqs' $ \req -> do
    return $ renderScript req
  return $ renderHtml $ mconcat $ scripts

renderScript :: String -> H.Html
renderScript req =
  H.script ! A.type_ ("text/javascript")
           ! A.src (toValue $ toUrl $ "/js/" ++ req) $ toHtml ("" :: String)

