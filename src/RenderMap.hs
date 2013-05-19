-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.DeepSeq
import Control.Monad
import Data.Array
import qualified Data.ByteString as BS
import Graphics.Rendering.Cairo
import System.Environment

main :: IO ()
main = do

  args <- getArgs

  (mapFile, width, height, blockFile, tileFile, outFile) <- case args of
    [mapFile, width, height, blockFile, tileFile, outFile] -> return (mapFile,   read width, read height, blockFile,        tileFile,   outFile)
    [mapFile, width, height, blockFile, tileFile]          -> return (mapFile,   read width, read height, blockFile,        tileFile,   "out.png")
    [mapFile, width, height, blockFile]                    -> return (mapFile,   read width, read height, blockFile,        "test.png", "out.png")
    [mapFile, width, height]                               -> return (mapFile,   read width, read height, "test.tiles.bin", "test.png", "out.png")
    [mapFile]                                              -> return (mapFile,   20,         20,          "test.tiles.bin", "test.png", "out.png")
    []                                                     -> return ("out.map", 20,         20,          "test.tiles.bin", "test.png", "out.png")
    _                                                      -> fail "Usage: rendermap [mapFile [width height [blockfile [tileset [output]]]]]"

  putStrLn $ "Loading map from " ++ show mapFile ++ "..."
  mapData <- BS.readFile mapFile

  when (BS.length mapData /= width * height) $ fail $ "Incorrect map size! Expected " ++ show (width * height) ++ " bytes (" ++ show width ++ "x" ++ show height ++ " blocks), but map is " ++ show (BS.length mapData) ++ " bytes."

  let mapArray = array ((0, 0), (width-1, height-1)) [ ((x, y), mapData `BS.index` (x + y * width)) | x <- [0..width-1], y <- [0..width-1] ]
  mapArray `deepseq` return ()

  putStrLn $ "Loading blocks from " ++ show blockFile ++ "..."
  blockData <- BS.readFile blockFile

  let blockArray = array (0, BS.length blockData `div` 16 - 1) [ (i, array ((0, 0), (3, 3)) [ ((x, y), blockData `BS.index` (i * 16 + x + y * 4)) | x <- [0..3], y <- [0..3] ]) | i <- [0..BS.length blockData `div` 16 - 1] ]
  blockArray `deepseq` return ()

  putStrLn $ "Loading tiles from " ++ show tileFile ++ "..."
  withImageSurfaceFromPNG tileFile $ \tileSurface -> do

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

      putStrLn $ "Saving rendered map to " ++ show outFile ++ "..."

      surfaceWriteToPNG outSurface outFile
