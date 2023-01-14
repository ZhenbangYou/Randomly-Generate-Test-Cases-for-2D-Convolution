{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (Ix, MArray (newArray), freeze, readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Foldable (for_)
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
  for_ [0 .. height * width - 1] $ \ix ->
    ( do
        num <- randomIO
        writeArray arr ix num
    )
  return arr

convolution ::
  ( Ix i,
    Num i,
    Num t,
    Enum i,
    MArray a t m
  ) =>
  a i t ->
  (i, i) ->
  a i t ->
  (i, i) ->
  m (a i t)
convolution !input (!inputHeight, !inputWidth) !weight (!weightHeight, !weightWidth) = do
  let (!outputHeight, !outputWidth) = (inputHeight - weightHeight + 1, inputWidth - weightWidth + 1)
  !output <- newArray (0, outputHeight * outputWidth - 1) 0
  for_
    [0 .. inputHeight - weightHeight]
    ( \oh ->
        for_
          [0 .. inputWidth - weightWidth]
          ( \ow ->
              for_
                [0 .. weightHeight - 1]
                ( \wh ->
                    for_
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
  !input <- randMatrix inputHeight inputWidth :: IO (IOUArray Int RandomNumberType)
  !weight <- randMatrix weightHeight weightWidth
  !output <- convolution input (inputHeight, inputWidth) weight (weightHeight, weightWidth)

  !inputImmutable <- freeze input :: IO (UArray Int RandomNumberType)
  !weightImmutable <- freeze weight :: IO (UArray Int RandomNumberType)
  !outputImmutable <- freeze output :: IO (UArray Int RandomNumberType)

  let !outputDirPath = path </> [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]
      !inputFile = outputDirPath </> "input" <.> "txt"
      !weightFile = outputDirPath </> "weight" <.> "txt"
      !outputFile = outputDirPath </> "output" <.> "txt"

  createDirectoryIfMissing True outputDirPath

  writeFile inputFile [i|#{inputHeight} #{inputWidth}\n|]
  writeFile weightFile [i|#{weightHeight} #{weightWidth}\n|]
  writeFile outputFile [i|#{inputHeight-weightHeight+1} #{inputWidth-weightWidth+1}\n|]

  appendFile inputFile $ show inputImmutable
  appendFile weightFile $ show weightImmutable
  appendFile outputFile $ show outputImmutable

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
        [ (256, 256, 8, 8),
          (256, 256, 16, 16),
          (256, 256, 32, 32),
          (256, 256, 64, 64)
        ]
  outputCases shapeList "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
