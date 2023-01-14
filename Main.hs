{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (replicateM_)
import Control.Monad.State.Strict (MonadState, runState, state)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (Ix, MArray (newArray), freeze, readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

type RandomNumberType = Float

rng :: MonadState Int64 m => m Int64
rng =
  state
    ( \s ->
        let next = s * 1103515245 + 12345
         in ((next `div` 65536) `mod` 32768, next)
    )

randMatrix :: (Num e, Num t, Ix t, MArray a e m) => t -> t -> Int64 -> m (a t e)
randMatrix !height !width !seed = do
  arr <- newArray (0, height * width - 1) 0
  let fill !ix !seed =
        ( if ix >= height * width
            then return arr
            else
              let (int, nextSeed) = runState rng seed
               in ( do
                      writeArray arr ix (fromIntegral (int `div` 128) - 128)
                      fill (ix + 1) nextSeed
                  )
        )
  fill 0 seed

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
  let (!outputHeight, !outputWidth) =
        (inputHeight - weightHeight + 1, inputWidth - weightWidth + 1)
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

outputOneCase ::
  (Int, Int, Int, Int) ->
  FilePath ->
  (Int64, Int64) ->
  IO ()
outputOneCase
  ( !inputHeight,
    !inputWidth,
    !weightHeight,
    !weightWidth
    )
  !path
  (!seed1, !seed2) = do
    !input <- randMatrix inputHeight inputWidth seed1 :: IO (IOUArray Int RandomNumberType)
    !weight <- randMatrix weightHeight weightWidth seed2
    !output <-
      convolution
        input
        (inputHeight, inputWidth)
        weight
        (weightHeight, weightWidth)

    !inputImmutable <- freeze input :: IO (UArray Int RandomNumberType)
    !weightImmutable <- freeze weight :: IO (UArray Int RandomNumberType)
    !outputImmutable <- freeze output :: IO (UArray Int RandomNumberType)

    let !outputDirPath =
          path
            </> [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]
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
  for_
    (zip [1 ..] shapeList)
    ( \(!ix, !shape) ->
        forkFinally
          (outputOneCase shape path (ix * 2, ix * 2 + 1))
          ( \case
              Right _ -> putMVar wg ()
              Left _ -> error "an error occured"
          )
    )
  replicateM_ numJobs $! takeMVar wg

main :: IO ()
main = do
  start <- getCurrentTime
  let shapeList =
        [ (1024, 1024, 8, 8),
          (1024, 1024, 16, 16),
          (1024, 1024, 32, 32),
          (1024, 1024, 64, 64)
        ]
  outputCases shapeList "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
