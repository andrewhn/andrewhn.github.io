{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll
import qualified Data.Map as M
import Data.String (fromString)
import System.FilePath.Posix ((</>), takeFileName)
import Data.String.Utils (replace)
import Control.Monad (mapM)
import Requires (requiresField)

main :: IO ()
main = hakyll $ do

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**.js" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*.es6" $ do
        route   $ setExtension "js"
        -- run es6 through babel on the fly
        compile $ getResourceString >>= withItemBody (unixFilter "babel" ["--presets=es2015"])

    match "data/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        -- recompile clay css on the fly
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "other.rst"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route postRoute
        let fullPostContext = postContext <> requiresField "reqs"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" fullPostContext
            >>= loadAndApplyTemplate "templates/default.html" fullPostContext
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveContext =
                    listField "posts" postContext (return posts) <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                >>= loadAndApplyTemplate "templates/default.html" archiveContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexContext =
                    listField "posts" teaserContext (return $ take 5 posts) <>
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postRoute :: Routes
postRoute = composeRoutes (setExtension "html") (metadataRoute build) where
  build :: Metadata -> Routes
  build md = customRoute (\fp -> "posts" </> getDate md </> getFile fp)
  getFile = takeFileName . toFilePath
  getDate :: Metadata -> String
  getDate md = replace "-" "/" $ M.findWithDefault "unknown" "date" md

postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext

teaserContext :: Context String
-- TODO: maybe change default separator to be rst friendly
teaserContext = teaserFieldWithSeparator "<!--more-->" "teaser" "content" <>
                postContext

