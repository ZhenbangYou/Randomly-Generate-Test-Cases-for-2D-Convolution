{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (forM_, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Array (Array)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (Ix, MArray (newArray), freeze, readArray, writeArray)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.Random (randomIO)
import System.Random.Stateful (Random)

type RandomNumberType = Float

randMatrix ::
  ( MArray a e m,
    Ix i,
    Num i,
    Num e,
    Enum i,
    Random e,
    MonadIO m
  ) =>
  i ->
  i ->
  m (a i e)
randMatrix !height !width = do
  arr <- newArray (0, height * width - 1) 0
  forM_ [0 .. height * width - 1] $ \ix ->
    ( do
        num <- randomIO
        writeArray arr ix num
    )
  return arr

convolution ::
  IOUArray Int RandomNumberType ->
  (Int, Int) ->
  IOUArray Int RandomNumberType ->
  (Int, Int) ->
  IO (IOUArray Int RandomNumberType)
convolution !input (!inputHeight, !inputWidth) !weight (!weightHeight, !weightWidth) = do
  let (!outputHeight, !outputWidth) = (inputHeight - weightHeight + 1, inputWidth - weightWidth + 1)
  !output <- newArray (0, outputHeight * outputWidth - 1) 0
  forM_
    [0 .. inputHeight - weightHeight]
    ( \oh ->
        forM_
          [0 .. inputWidth - weightWidth]
          ( \ow ->
              forM_
                [0 .. weightHeight - 1]
                ( \wh ->
                    forM_
                      [0 .. weightWidth - 1]
                      ( \ww -> do
                          let (ih, iw) = (oh + wh, ow + ww)
                          srcI <- readArray input (ih * inputWidth + iw)
                          srcW <- readArray weight (wh * weightWidth + ww)
                          modifyArray output (oh * outputWidth + ow) (+ (srcI * srcW))
                      )
                )
          )
    )
  return $! output
  where
    modifyArray !arr !ix !f = do
      origin <- readArray arr ix
      writeArray arr ix (f origin)

outputOneCase :: (Int, Int, Int, Int) -> FilePath -> IO ()
outputOneCase (!inputHeight, !inputWidth, !weightHeight, !weightWidth) !path = do
  !input <- randMatrix inputHeight inputWidth
  !weight <- randMatrix weightHeight weightWidth
  !output <- convolution input (inputHeight, inputWidth) weight (weightHeight, weightWidth)

  !input <- freeze input :: IO (Array Int RandomNumberType)
  !weight <- freeze weight :: IO (Array Int RandomNumberType)
  !output <- freeze output :: IO (Array Int RandomNumberType)

  let !outputDirPath = path </> [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]
      !inputFile = outputDirPath </> "input" <.> "txt"
      !weightFile = outputDirPath </> "weight" <.> "txt"
      !outputFile = outputDirPath </> "output" <.> "txt"

  createDirectoryIfMissing True outputDirPath
  
  writeFile inputFile [i|#{inputHeight} #{inputWidth}\n|]
  writeFile weightFile [i|#{weightHeight} #{weightWidth}\n|]
  writeFile outputFile [i|#{inputHeight-weightHeight+1} #{inputWidth-weightWidth+1}\n|]

  appendFile inputFile $ show input
  appendFile weightFile $ show weight
  appendFile outputFile $ show output

outputCases :: [(Int, Int, Int, Int)] -> FilePath -> IO ()
outputCases !shapeList !path = do
  let !numJobs = length shapeList
  !wg <- newEmptyMVar
  mapM_
    ( \x ->
        forkFinally
          (outputOneCase x path)
          ( \case
              Right _ -> putMVar wg ()
              Left _ -> error "an error occured"
          )
    )
    shapeList
  replicateM_ numJobs $! takeMVar wg

main :: IO ()
main = do
  start <- getCurrentTime
  let shapeList =
        [ (2, 2, 2, 2),
          (128, 128, 8, 8),
          (128, 128, 16, 16),
          (128, 128, 32, 32),
          (128, 128, 64, 64),
          (256, 256, 8, 8),
          (256, 256, 16, 16),
          (256, 256, 32, 32),
          (256, 256, 64, 64)
        ] ::
          [(Int, Int, Int, Int)]
  outputCases shapeList "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
