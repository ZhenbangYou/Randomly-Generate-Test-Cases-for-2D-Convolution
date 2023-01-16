{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (replicateM_)
import Control.Monad.State.Strict (State, runState, state)
import Data.Array.MArray (Ix, MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (IArray, UArray, (!))
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

type RandomNumberType = Float

rng :: State Int64 Int64
rng =
  state
    ( \s ->
        let !next = s * 1103515245 + 12345
         in ((next `div` 65536) `mod` 32768, next)
    )

randMatrix :: (MArray a e m, Ix t, Num t, Num e) => t -> t -> Int64 -> m (a t e)
randMatrix !height !width !seed = do
  arr <- newArray (0, height * width - 1) 0
  let fill !ix !s =
        ( if ix >= height * width
            then return arr
            else
              let castInt int = fromIntegral (int `div` 128) - 128
                  !(float, nextSeed) = runState (rng <&> castInt) s
               in ( do
                      writeArray arr ix float
                      fill (ix + 1) nextSeed
                  )
        )
  fill 0 seed

convolution ::
  ( MArray a1 t m,
    Ix i,
    Num i,
    Num t,
    Enum i,
    IArray a2 t,
    IArray a3 t
  ) =>
  a2 i t ->
  (i, i) ->
  a3 i t ->
  (i, i) ->
  m (a1 i t)
convolution !input (!inputHeight, !inputWidth) !weight (!weightHeight, !weightWidth) = do
  let !(outputHeight, outputWidth) =
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
                          let !(ih, iw) = (oh + wh, ow + ww)
                          let !srcI = input ! (ih * inputWidth + iw)
                          let !srcW = weight ! (wh * weightWidth + ww)
                          modifyArray output (oh * outputWidth + ow) (+ (srcI * srcW))
                      )
                )
          )
    )
  return $! output
  where
    modifyArray !arr !ix !f = do
      !origin <- readArray arr ix
      writeArray arr ix $! f origin

genOneCase ::
  (Ix a, Num a, Enum a) =>
  (a, a, a, a) ->
  (Int64, Int64) ->
  ( UArray a RandomNumberType,
    UArray a RandomNumberType,
    UArray a RandomNumberType
  )
genOneCase
  ( !inputHeight,
    !inputWidth,
    !weightHeight,
    !weightWidth
    )
  (!seed1, !seed2) =
    let !input = runSTUArray $! randMatrix inputHeight inputWidth seed1
        !weight = runSTUArray $! randMatrix weightHeight weightWidth seed2
        !output =
          runSTUArray $!
            convolution
              input
              (inputHeight, inputWidth)
              weight
              (weightHeight, weightWidth)
     in (input, weight, output)

outputMatrix :: UArray Int RandomNumberType -> (Int, Int) -> FilePath -> String -> IO ()
outputMatrix !matrix (!height, !width) !dir !name = do
  let !path = dir </> name <.> "txt"
  writeFile path [i|#{height} #{width}\n|]
  appendFile path $! show matrix

outputOneCase :: (Int, Int, Int, Int) -> FilePath -> (Int64, Int64) -> IO ()
outputOneCase
  ( !inputHeight,
    !inputWidth,
    !weightHeight,
    !weightWidth
    )
  !path
  (!seed1, !seed2) = do
    let !(input, weight, output) =
          genOneCase (inputHeight, inputWidth, weightHeight, weightWidth) (seed1, seed2)

    let !outputDirPath =
          path
            </> [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]

    createDirectoryIfMissing True outputDirPath

    let !(outputHeight, outputWidth) =
          (inputHeight - weightHeight + 1, inputWidth - weightWidth + 1)

    outputMatrix input (inputHeight, inputWidth) outputDirPath "input"
    outputMatrix weight (weightHeight, weightWidth) outputDirPath "weight"
    outputMatrix output (outputHeight, outputWidth) outputDirPath "output"

outputCases :: [(Int, Int, Int, Int)] -> FilePath -> IO ()
outputCases !shapeList !path = do
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
  replicateM_ (length shapeList) $! takeMVar wg

main :: IO ()
main = do
  start <- getCurrentTime
  let shapeList =
        [ (512, 512, 8, 8),
          (512, 512, 16, 16),
          (512, 512, 32, 32),
          (512, 512, 64, 64)
        ]
  outputCases shapeList "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
