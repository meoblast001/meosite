{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Data.Monoid (mempty, mappend)
import Hakyll
import System.FilePath
import System.Process (system)
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
    _ <- system $ unwords ["xelatex", "-halt-on-error",
        "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
    return ()
  makeItem $ TmpFile pdfPath

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "docs/*.pdf" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  match "pages/**.md" $ do
    route $ siteRoute "pages/" "html"
    compile $ pandocCompiler
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

  match "index.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
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
