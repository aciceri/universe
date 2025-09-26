{-# LANGUAGE OverloadedStrings #-}

module Fields
  ( peekField,
    envField,
    readTimeField,
    readTimeFieldWith,
    publishedGroupField,
    concatField,
    allTagsField,
  )
where

import Control.Applicative (empty)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.List
  ( dropWhileEnd,
    groupBy,
    isPrefixOf,
  )
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock (UTCTime (..))
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Typeable
import Hakyll
import System.Environment (lookupEnv)

-- Peek Field
--------------------------------------------------------------------------------

peekField ::
  -- | length to peak
  Int ->
  -- | Key to use
  String ->
  -- | Snapshot to load
  Snapshot ->
  -- | Resulting context
  Context String
peekField length key snapshot = field key $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  return (peek body)
  where
    peek = T.unpack . T.unwords . take length . T.words . T.pack

-- Environment variable field
--------------------------------------------------------------------------------
envField :: String -> String -> Context String
envField name envVar = field name $ \_ -> unsafeCompiler $ do
  maybeValue <- lookupEnv envVar
  return $ fromMaybe "" maybeValue

-- Default reading speed in words per minute
defaultReadingSpeed :: Int
defaultReadingSpeed = 200

-- Field that naïvely determines the reading time
-- by assuming an average reading velocity and
-- dividing the actual number of words by this average
readTimeField :: String -> Snapshot -> Context String
readTimeField = readTimeFieldWith defaultReadingSpeed

readTimeFieldWith :: Int -> String -> Snapshot -> Context String
readTimeFieldWith wordsPerMinute name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack $ body)
  return $ show $ div words wordsPerMinute

publishedGroupField ::
  String -> -- name
  [Item String] -> -- posts
  Context String -> -- Post context
  Context String -- output context
publishedGroupField name posts postContext = listField name groupCtx $ do
  tuples <- traverse extractYear posts
  let grouped = groupByYear tuples
  let merged = merge <$> grouped
  let itemized = makeItem <$> merged

  sequence itemized
  where
    groupCtx =
      field "year" (return . show . fst . itemBody)
        <> listFieldWith "posts" postContext (return . snd . itemBody)

    merge :: [(Integer, [Item String])] -> (Integer, [Item String])
    merge [] = (0, [])
    merge [g] = g
    merge (g : gs) =
      let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
       in foldr conv g gs

    groupByYear = groupBy (\(y, _) (y', _) -> y == y')

    extractYear :: Item a -> Compiler (Integer, [Item a])
    extractYear item = do
      time <- getItemUTC defaultTimeLocale (itemIdentifier item)
      let (year, _, _) = (toGregorian . utctDay) time
      return (year, [item])

concatField :: String -> Context String
concatField name = functionField name (\args item -> return $ concat args)

allTagsField :: String -> Tags -> Context String
allTagsField name tags = listFieldWith name (tagCtx tags) mkPostTags
  where
    tagCtx :: Tags -> Context String
    tagCtx tags =
      field "name" (return . itemBody)
        <> field "url" mkTagUrl

    mkTagUrl :: -- unwrap the maybe that gets returned when
    -- seaching the route of the id of the tag
    -- (or something like that)

      -- \| a tag name
      Item String ->
      -- \| corresponding tag page url
      Compiler String
    mkTagUrl item = do
      maybeRoute <- getRoute . tagsMakeId tags . itemBody $ item
      return $ toUrl $ fromMaybe "#" maybeRoute

    mkPostTags :: -- resolve the item's tags
    -- if it has tags apply them to makeItem (phrasing?)
    -- else return empty to suppress rendering
      Item String -> -- a post
      Compiler [Item String] -- the tags for a given post
    mkPostTags item =
      (getTags . itemIdentifier $ item)
        >>= \tags' ->
          if null tags'
            then empty
            else mapM makeItem tags'
