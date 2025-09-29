{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Void
import Fields
import GHC.IO.Encoding
import Hakyll
import Hakyll.Images
  ( compressJpgCompiler,
    ensureFitCompiler,
    loadImage,
  )
import Hakyll.Web.Sass
import Replace.Megaparsec
import System.Environment (lookupEnv)
import System.FilePath.Posix
  ( takeBaseName,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Pandoc.Options
  ( Extension
      ( Ext_latex_macros,
        Ext_raw_html,
        Ext_tex_math_dollars,
        Ext_tex_math_double_backslash
      ),
    HTMLMathMethod (MathJax),
    ReaderOptions (readerExtensions),
    WriterOptions (writerExtensions, writerHTMLMathMethod),
    enableExtension,
  )
import Text.Sass.Options
  ( SassOptions (..),
    SassOutputStyle (..),
    defaultSassOptions,
  )

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "./out",
      previewPort = 5000,
      providerDirectory = "./"
    }

recentPostsCount :: Int
recentPostsCount = 5

feedItemsCount :: Int
feedItemsCount = 10

sassOptions :: Maybe FilePath -> SassOptions
sassOptions distPath =
  defaultSassOptions
    { sassSourceMapEmbed = True,
      sassOutputStyle = SassStyleCompact,
      sassIncludePaths = fmap (: []) distPath
    }

main :: IO ()
main = do
  setLocaleEncoding utf8
  sassCompiler <- fmap (sassCompilerWith . sassOptions) (lookupEnv "THIRDPARTY")
  compilerEnv <- lookupEnv "HAKYLL_ENV"

  hakyllWith config $ do
    tags <- buildTags "posts/**.md" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pat -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      let ctx = postContext tags

      route tagRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat

        let tagsCtx =
              constField "title" title
                <> listField "posts" ctx (return posts)
                <> constField "tag" tag
                <> constField "language" "en"
                <> baseContext

        makeItem ""
          >>= loadAndApplyTemplate "generator/templates/tag.html" tagsCtx
          >>= loadAndApplyTemplate "generator/templates/default.html" tagsCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    match "generator/templates/*" $
      compile templateBodyCompiler

    match "posts/**.md" $ do
      route postRoute
      compile $
        customCompiler
          >>= saveSnapshot "posts-content"
          >>= applyFilter embedYoutube
          >>= applyFilter embedVideo
          >>= applyFilter embedAsciinema
          >>= loadAndApplyTemplate "generator/templates/post.html" (postContext tags)
          >>= loadAndApplyTemplate "generator/templates/default.html" (postContext tags)
          >>= relativizeUrls

      depends <- makePatternDependency "generator/css/**.scss"
      rulesExtraDependencies [depends] $ do
        match (fromRegex "^generator/css/custom.scss") $ do
          route $ stripRoute "generator/" `composeRoutes` setExtension "css"
          compile sassCompiler

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**.md"
        let ctx = postContext tags
        let archiveCtx =
              listField "posts" ctx (return posts)
                <> publishedGroupField "years" posts ctx
                <> constField "archive" "true"
                <> constField "language" "en"
                <> constField "title" "Archive"
                <> baseContext

        makeItem ""
          >>= loadAndApplyTemplate "generator/templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "generator/templates/default.html" archiveCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    match "pages/home.md" $ do
      route $ constRoute "index.html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**.md"
        let ctx = postContext tags
        let indexCtx =
              listField "posts" ctx (return $ take recentPostsCount posts)
                <> constField "home" "true"
                <> constField "title" "Home"
                <> constField "language" "en"
                <> baseContext

        customCompiler
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "generator/templates/home.html" indexCtx
          >>= loadAndApplyTemplate "generator/templates/default.html" indexCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    match "pages/contacts.md" $ do
      route $ constRoute "contacts/index.html"
      compile $ do
        let ctx =
              constField "title" "Contacts"
                <> constField "contacts" "true"
                <> constField "language" "en"
                <> baseContext
        customCompiler
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "generator/templates/default.html" ctx
          >>= relativizeUrls
          >>= cleanIndexUrls

    match "generator/js/custom.js" $ do
      route $ stripRoute "generator/"
      compile copyFileCompiler

    match ("generator/thirdparty/**" .&&. complement "**.md") $ do
      route $ stripRoute "generator/"
      compile copyFileCompiler

    match "assets/custom/**" $ do
      route idRoute
      compile copyFileCompiler

    match "assets/images/**.jpg" $
      version "original" $ do
        route $ stripRoute "assets/"
        compile copyFileCompiler

    match ("assets/images/**" .&&. complement "**.jpg") $ do
      route $ stripRoute "assets/"
      compile copyFileCompiler

    match "assets/casts/**.cast" $ do
      route $ stripRoute "assets/"
      compile copyFileCompiler

    match "assets/videos/**" $ do
      route $ stripRoute "assets/"
      compile copyFileCompiler

    create ["rss/rss.xml"] $ do
      route idRoute
      compile (feedCompiler renderRss)

    create ["atom/atom.xml"] $ do
      route idRoute
      compile (feedCompiler renderAtom)

domain :: String
domain = "blog.aciceri.dev"

root :: String
root = "https://" ++ domain

postContext :: Tags -> Context String
postContext tags =
  dateField "date" "%Y-%m-%d"
    <> allTagsField "tags" tags
    <> constField "item-type" "post"
    <> teaserField "teaser" "posts-content"
    <> peekField 50 "peek" "posts-content"
    <> readTimeField "read-time" "posts-content"
    <> pathField "sourcefile"
    <> baseContext

baseContext :: Context String
baseContext =
  envField "nix-hash" "NIX_HASH"
    <> constField "item-type" "default"
    <> concatField "concat"
    <> constField "root" root
    <> defaultContext

prefixRoute :: String -> Routes
prefixRoute prefix = customRoute makePrefixRoute
  where
    makePrefixRoute ident = parentDir </> prefixed
      where
        p = toFilePath ident
        parentDir = takeDirectory p
        baseName = takeBaseName p
        ext = takeExtension p
        prefixed = prefix ++ "-" ++ baseName ++ ext

stripRoute :: String -> Routes
stripRoute txt = gsubRoute txt (const "")

postRoute :: Routes
postRoute = gsubRoute ".md" (const "/index.html")

tagRoute :: Routes
tagRoute = gsubRoute ".html" (const "/index.html")

metadatasToStr :: [String] -> String
metadatasToStr = ("----------\n" ++) . (++ "----------\n") . unlines

customCompiler :: Compiler (Item String)
customCompiler =
  let mathExtensions =
        [ Ext_tex_math_dollars,
          Ext_tex_math_double_backslash,
          Ext_latex_macros
        ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr enableExtension defaultExtensions mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = MathJax ""
          }
      readerOptions =
        defaultHakyllReaderOptions
          { readerExtensions = enableExtension Ext_raw_html (readerExtensions defaultHakyllReaderOptions)
          }
   in pandocCompilerWith readerOptions writerOptions

applyFilter :: (String -> String) -> Item String -> Compiler (Item String)
applyFilter f item = return $ fmap f item

createEmbedFilter :: String -> Parsec Void String String -> (String -> String) -> String -> String
createEmbedFilter prefix parser = streamEdit macro
  where
    macro = do
      string ("{" ++ prefix ++ ":")
      content <- parser
      char '}'
      return content

youtubeParser :: Parsec Void String String
youtubeParser = many alphaNumChar

videoParser :: Parsec Void String String
videoParser = many (alphaNumChar <|> char '-' <|> char '.')

asciinemaParser :: Parsec Void String String
asciinemaParser = many (alphaNumChar <|> char '-')

embedYoutube :: String -> String
embedYoutube = createEmbedFilter "youtube" youtubeParser youtubeEmbedder
  where
    youtubeEmbedder id = "<div class='youtube-wrapper'><iframe allowfullscreen='true' src='https://www.youtube.com/embed/" ++ id ++ "'></iframe></div>"

embedVideo :: String -> String
embedVideo = createEmbedFilter "video" videoParser videoEmbedder
  where
    videoEmbedder filename = "<video controls src='/videos/" ++ filename ++ "'>Sorry, this browser doesn't support embedded videos</video>"

embedAsciinema :: String -> String
embedAsciinema = createEmbedFilter "asciinema" asciinemaParser asciinemaEmbedder
  where
    asciinemaEmbedder name = "\n\n<div class='asciinema-container'>\n<div id='cast-" ++ name ++ "'></div>\n</div>\n\n"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Andrea Ciceri's blog",
      feedDescription = "Personal blog with a bit of everything",
      feedAuthorName = "Andrea Ciceri",
      feedAuthorEmail = "andrea.ciceri@autistici.org",
      feedRoot = "https://blog.aciceri.dev"
    }

feedContext :: Context String
feedContext =
  bodyField "description"
    <> dateField "date" "%Y-%m-%d"
    <> baseContext

type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedContext
    =<< fmap (take feedItemsCount) . recentFirst
    =<< loadAllSnapshots "posts/**.md" "posts-content"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls =
  let cleanIndex :: String -> String
      cleanIndex url
        | isSuffixOf idx url = take (length url - length idx) url
        | otherwise = url
        where
          idx = "index.html"
   in return . fmap (withUrls cleanIndex)
