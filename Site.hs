{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration
import Data.Functor
import Data.Maybe
import Data.Monoid (mempty, mappend)
import Hakyll
import Hakyll.Web.Sass
import System.FilePath
import System.Process
import Text.Regex (mkRegex, subRegex)

toIndex :: String -> Identifier -> FilePath
toIndex extension ident =
  let originalPath = toFilePath ident
      newDirectory = takeWhile (/= '.') originalPath
  in newDirectory ++ "/index." ++ extension

siteRoute :: String -> String -> Routes
siteRoute remove extension =
  composeRoutes (customRoute $ toIndex extension) (gsubRoute remove (const ""))

blogRoute :: String -> Routes
blogRoute extension =
  let dropDate identifier = subRegex (mkRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}-")
                                     (toFilePath identifier) ""
  in composeRoutes (customRoute $ toIndex extension) (customRoute dropDate)

urlDirectoryField :: String -> Context a
urlDirectoryField key =
  field key $ fmap (maybe mempty toUrl) . fmap (fmap takeDirectory) . getRoute .
              itemIdentifier

siteContext :: Context String
siteContext = urlDirectoryField "urlDirectory" `mappend` defaultContext

blogContext :: Context String
blogContext = dateField "date" " %e %B %Y" `mappend` siteContext

-- Used from https://github.com/jaspervdj/jaspervdj
xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
  TmpFile texPath <- newTmpFile "xelatex.tex"
  let resDir = dropExtension $ toFilePath $ itemIdentifier item
      tmpDir = takeDirectory texPath
      pdfPath = replaceExtension texPath "pdf"
  unsafeCompiler $ do
    writeFile texPath $ itemBody item
    _ <- system $ unwords ["xelatex", "-halt-on-error", "-output-directory",
                           tmpDir, texPath, ">/dev/null", "2>&1"]
    return ()
  makeItem $ TmpFile pdfPath

coffeeScriptCompiler :: Compiler (Item String)
coffeeScriptCompiler = do
  input <- itemBody <$> getResourceBody
  output <- unsafeCompiler $ readCreateProcess (proc "coffee" ["-cs"]) input
  makeItem output

compileFixedHeight :: Integer -> Bool -> Compiler (Item TmpFile)
compileFixedHeight height isTransparent = do
  originalPath <- toFilePath <$> itemIdentifier <$> getResourceBody
  extension <- takeExtension <$> fromMaybe ".png" <$>
               (getUnderlying >>= getRoute)
  TmpFile resultPath <- newTmpFile ("imagemagick" ++ extension)
  unsafeCompiler $ do
    _ <- system $ unwords (["convert", "-geometry", "x75"] ++
                           (if isTransparent then ["-background", "transparent"]
                                             else []) ++
                           [originalPath, resultPath])
    return ()
  makeItem $ TmpFile resultPath

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/tools/*.svg" $ do
    route $ setExtension "png"
    compile $ compileFixedHeight 300 True

  match "docs/*.pdf" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/*.sass" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler

  match "js/*.coffee" $ do
    route $ setExtension "js"
    compile coffeeScriptCompiler

  match "templates/*" $ compile templateCompiler

  match "pages/**.md" $ do
    route $ siteRoute "pages/" "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" siteContext
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls

  match "blog/*.md" $ do
    route $ blogRoute "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" blogContext
      >>= loadAndApplyTemplate "templates/default.html" blogContext
      >>= relativizeUrls

  create ["blog/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "blog/*.md"
      let archiveCtx = listField "posts" blogContext (return posts) `mappend`
                       constField "title" "Archives" `mappend`
                       siteContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/index.html" siteContext
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls

  let cvWithLanguage language = do
        route $ composeRoutes (gsubRoute "cv" (const ("cv-" ++ language)))
                              (setExtension "pdf")
        let setLanguage language' body =
              fmap (fmap (\content -> "\\providecommand\\locale{" ++
                                      language' ++ "}" ++ content)) body
        compile (setLanguage language getResourceBody >>= xelatex)

  match "docs/cv.tex" $ version "en" $ cvWithLanguage "en"
  match "docs/cv.tex" $ version "de" $ cvWithLanguage "de"
