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
import Data.Array
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.Monoid
import Graphics.Rendering.Cairo hiding
  ( version
  )
import Options.Applicative
import qualified Text.ParserCombinators.ReadP as RP

import Paths_pokemap

data Spec = Spec
  { mapSize    :: (Int, Int)
  , tilesFile  :: FilePath
  , blocksFile :: FilePath
  , outputFile :: FilePath
  , mapFile    :: FilePath
  }

instance Show Spec where
  show (Spec {..}) =
    "--size " ++ show (fst mapSize) ++ "x" ++ show (snd mapSize) ++ " " ++
    "--tiles " ++ show tilesFile ++ " " ++
    "--blocks " ++ show blocksFile ++ " " ++
    "--output " ++ show outputFile ++ " " ++
    show mapFile

readSize :: String -> Maybe (Int, Int)
readSize sizeStr = fmap fst $ listToMaybe $ flip RP.readP_to_S sizeStr $ do
  w <- RP.munch1 isDigit
  _ <- RP.char 'x'
  h <- RP.munch1 isDigit
  RP.eof
  return (read w, read h)

specParser :: Parser Spec
specParser = Spec
  <$> option ( long "size"   <> short 's' <> metavar "WIDTHxHEIGHT" <> reader readSize <> value (20, 20)         <> help "The size of the map to render, in blocks" )
  <*> option ( long "tiles"  <> short 't' <> metavar "FILE"         <> reader str      <> value "test.png"       <> help "The image file containing 8x8 tiles" )
  <*> option ( long "blocks" <> short 'b' <> metavar "FILE"         <> reader str      <> value "test.tiles.bin" <> help "The binary file containing the blockset definition" )
  <*> option ( long "output" <> short 'o' <> metavar "FILE"         <> reader str      <> value "out.png"        <> help "Path to store the rendered map" )
  <*> argument str                         ( metavar "FILE"         <>                                              help "The binary file containing the map definition" )

searchFile :: FilePath -> IO BS.ByteString
searchFile fp = catch (BS.readFile fp) $ \(_ :: IOException) -> getDataFileName fp >>= BS.readFile

searchSurface :: FilePath -> IO Surface
searchSurface fp = catch (imageSurfaceCreateFromPNG fp) $ \(_ :: IOException) -> getDataFileName fp >>= imageSurfaceCreateFromPNG

main :: IO ()
main = do

  spec@(Spec {..}) <- execParser $ ParserInfo specParser True "Renders maps for Pokemon games." "" "" 1
  putStrLn $ "pokemap " ++ show version
  putStrLn "Map Renderer"
  putStrLn $ "Options: " ++ show spec

  let (width, height) = mapSize

  putStrLn "Loading map..."
  mapData <- searchFile mapFile

  when (BS.length mapData /= width * height) $ fail $ "Incorrect map size! Expected " ++ show (width * height) ++ " bytes (" ++ show width ++ "x" ++ show height ++ " blocks), but map is " ++ show (BS.length mapData) ++ " bytes."

  let mapArray = array ((0, 0), (width-1, height-1)) [ ((x, y), mapData `BS.index` (x + y * width)) | x <- [0..width-1], y <- [0..width-1] ]
  mapArray `deepseq` return ()

  putStrLn $ "Loading blocks from " ++ show blocksFile ++ "..."
  blockData <- searchFile blocksFile

  let blockArray = array (0, BS.length blockData `div` 16 - 1) [ (i, array ((0, 0), (3, 3)) [ ((x, y), blockData `BS.index` (i * 16 + x + y * 4)) | x <- [0..3], y <- [0..3] ]) | i <- [0..BS.length blockData `div` 16 - 1] ]
  blockArray `deepseq` return ()

  putStrLn $ "Loading tiles from " ++ show tilesFile ++ "..."
  tileSurface <- searchSurface tilesFile

  tileWidth <- imageSurfaceGetWidth tileSurface

  let tilesPerLine = tileWidth `div` 8
  putStrLn $ "Looks like this image has " ++ show tilesPerLine ++ " tiles per line."

  fmt <- imageSurfaceGetFormat tileSurface

  putStrLn "Creating output surface..."
  withImageSurface fmt (width * 4 * 8) (height * 4 * 8) $ \outSurface -> do

    putStrLn "Rendering the map..."

    renderWith outSurface $ do

      forM_ (assocs mapArray) $ \((x, y), b) -> do

        let thisBlock = blockArray ! (fromIntegral b)

        forM_ (assocs thisBlock) $ \((x', y'), t) -> do

          let (tileNY, tileNX) = (fromIntegral t) `divMod` tilesPerLine
          let tileX = 8 * tileNX
          let tileY = 8 * tileNY
          let outX = (x * 4 + x') * 8
          let outY = (y * 4 + y') * 8

          newPath
          rectangle (fromIntegral outX) (fromIntegral outY) 8 8

          setSourceSurface tileSurface (fromIntegral $ outX - tileX) (fromIntegral $ outY - tileY)

          fill

    putStrLn $ "Saving rendered map to " ++ show outputFile ++ "..."

    surfaceWriteToPNG outSurface outputFile
