-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main
where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Random
import Data.Array
import qualified Data.ByteString as BS
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Options.Applicative
import qualified Text.ParserCombinators.ReadP as RP

import Block
import Maze

import Paths_pokemap

data Spec = Spec
  { mapSize    :: (Int, Int)
  , blockSpec  :: FilePath
  , outputFile :: FilePath
  }

instance Show Spec where
  show (Spec {..}) =
    "--size " ++ show (fst mapSize) ++ "x" ++ show (snd mapSize) ++ " " ++
    "--blocks " ++ show blockSpec ++ " " ++
    "--output " ++ show outputFile

readSize :: String -> Maybe (Int, Int)
readSize sizeStr = fmap fst $ listToMaybe $ flip RP.readP_to_S sizeStr $ do
  w <- RP.munch1 isDigit
  _ <- RP.char 'x'
  h <- RP.munch1 isDigit
  RP.eof
  return (read w, read h)

specParser :: Parser Spec
specParser = Spec
  <$> option ( long "size"   <> short 's' <> metavar "WIDTHxHEIGHT" <> reader readSize <> value (20, 20)      <> help "The size of map to generate, in blocks" )
  <*> option ( long "blocks" <> short 'b' <> metavar "FILE"         <> reader str      <> value "test.blocks" <> help "The block specification file" )
  <*> option ( long "output" <> short 'o' <> metavar "FILE"         <> reader str      <> value "out.map"     <> help "Path to store the generated map" )

searchFile :: FilePath -> IO BS.ByteString
searchFile fp = catch (BS.readFile fp) $ \(_ :: IOException) -> getDataFileName fp >>= BS.readFile

duplicates :: (Eq a) => [a] -> [a]
duplicates []                       =     []
duplicates [_]                      =     []
duplicates (x:xs@(y:_)) | x == y    = x : duplicates xs
                        | otherwise =     duplicates xs

main :: IO ()
main = do

  spec@(Spec {..}) <- execParser $ ParserInfo specParser True "Generates random maps for Pokemon games, based on Wang tiles." "" "" 1
  putStrLn $ "pokemap " ++ show version
  putStrLn "Map Generator"
  putStrLn $ "Options: " ++ show spec

  putStrLn "Loading block specification..."
  blockData <- searchFile blockSpec

  case parseBlockFile blockSpec blockData of
    Left e -> do

      putStrLn $ "Error parsing block specification from " ++ show blockSpec ++ "!"
      putStrLn $ show e

    Right blocks -> do

      let
        bs      = sortBy (compare `on` blockIndex) blocks
        dups    = nub $ duplicates $ map blockIndex bs
        colors  = nub $ sort $ concatMap (concat . elems . sideColors) bs
        initMap = listArray ((1,1),mapSize) $ repeat Nothing

      putStrLn $ show (length bs) ++ " blocks loaded."
      when (not $ null dups) $ putStrLn $ "Warning: Duplicate block indexes: " ++ intercalate ", " (map show dups)
      putStrLn $ show (length colors) ++ " colors defined: " ++ intercalate ", " colors

      g <- newStdGen

      putStrLn "Starting map generation..."

      mm <- force <$> evalRandT (buildMaze initMap (\_ _ -> []) bs) g

      case mm of
        Nothing -> putStrLn "Failed to find a solution. Not sure how that happened, I should've just run forever trying. Probably a bug."
        Just m -> do

          putStrLn $ "Saving result to " ++ show outputFile ++ "."
          BS.writeFile outputFile $ BS.pack [ blockIndex $ m ! (x,y) | y <- [1..snd mapSize], x <- [1..fst mapSize] ]
