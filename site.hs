--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import qualified Data.Set as S
import Control.Monad

import Hakyll

import System.FilePath
import Text.Pandoc.Options
import Text.Pandoc.Shared (eastAsianLineBreakFilter)
import Text.Pandoc.SideNote (usingSideNotes)

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

        forM [ "vendor/tufte-css/et-book/**"
             , "vendor/tufte-css/*.css" ] $ \pat -> do
              match pat $ do
                route $ customRoute $ \path ->
                    "css" </> (joinPath . drop 2 . splitPath . toFilePath $ path)
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
                myCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*"
                tagslist <- renderTagList tags
                let indexCtx = mconcat [ listField "posts" (postCtx tags) (return posts)
                                       , constField "title" "Home"
                                       , constField "taglist" tagslist
                                       , defaultContext
                                       ]
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= renderPandocWith myReaderOpts myWriterOpts
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls
        match "templates/*" $ compile templateBodyCompiler
        -- RSS Feed
        create ["feed.xml"] $ do
            route idRoute
            compile $ do
                posts <-
                    fmap (take 10) $
                        recentFirst =<< loadAllSnapshots "posts/*" "content"
                renderRss feedConfig feedCtx posts

myReaderOpts = defaultHakyllReaderOptions
myWriterOpts = defaultHakyllWriterOptions { writerHTMLMathMethod = KaTeX "", writerSectionDivs = True}

myCompiler =
    pandocCompilerWithTransform
        myReaderOpts
        myWriterOpts
        (eastAsianLineBreakFilter . usingSideNotes)

feedConfig =
    FeedConfiguration
    { feedTitle = "Alex Lu's Blog Feed"
    , feedDescription = "Alex Lu's Blog Feed"
    , feedAuthorName = "Alex Lu"
    , feedAuthorEmail = "b3A4ODY3NTU1K2Jsb2dAZ21haWwuY29t"
    , feedRoot = "https://op8867555.github.io"
    }

feedCtx :: Context String
feedCtx = mconcat [bodyField "description", defaultContext]

--------------------------------------------------------------------------------
filenameField =
    field "filename" $ return . takeFileName . toFilePath . itemIdentifier

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags `mappend` dateField "date" "%B %e, %Y" `mappend`
    filenameField `mappend`
    defaultContext
