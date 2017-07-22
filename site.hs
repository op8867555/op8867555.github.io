--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

import System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main =
    hakyll $ do
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler
        match "js/*" $ do
            route idRoute
            compile copyFileCompiler
        match "css/*" $ do
            route idRoute
            compile compressCssCompiler
        match (fromList ["about.rst", "contact.markdown"]) $ do
            route $ setExtension "html"
            compile $
                pandocCompiler >>=
                loadAndApplyTemplate "templates/default.html" defaultContext >>=
                relativizeUrls
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")
        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        constField "title" "Archives" `mappend`
                        defaultContext
                makeItem "" >>=
                    loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
                    loadAndApplyTemplate "templates/default.html" archiveCtx >>=
                    relativizeUrls
        let tagRuleFn tag pattern = do
                let title = "Posts tagged \"" ++ tag ++ "\""
                route idRoute
                compile $ do
                    posts <- recentFirst =<< loadAll pattern
                    let ctx =
                            constField "title" title `mappend`
                            listField "posts" (postCtx tags) (return posts) `mappend`
                            defaultContext
                    makeItem "" >>=
                        loadAndApplyTemplate "templates/tag.html" ctx >>=
                        loadAndApplyTemplate "templates/default.html" ctx >>=
                        relativizeUrls
        tagsRules tags tagRuleFn
        match "posts/*" $ do
            route $ setExtension "html"
            compile $
                pandocCompiler >>=
                loadAndApplyTemplate "templates/post.html" (postCtx tags) >>=
                loadAndApplyTemplate "templates/default.html" (postCtx tags) >>=
                relativizeUrls
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- fmap (take 3) $ recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        constField "title" "Home" `mappend`
                        tagCloudField "tagcloud" 50 200 tags `mappend`
                        defaultContext
                getResourceBody >>= applyAsTemplate indexCtx >>=
                    loadAndApplyTemplate "templates/default.html" indexCtx >>=
                    relativizeUrls
        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
filenameField =
    field "filename" $ return . takeFileName . toFilePath . itemIdentifier

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags `mappend` dateField "date" "%B %e, %Y" `mappend`
    filenameField `mappend`
    defaultContext
