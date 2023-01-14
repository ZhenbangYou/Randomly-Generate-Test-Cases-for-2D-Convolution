{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (replicateM_)
import Control.Monad.ST.Strict (ST)
import Control.Monad.State.Strict (StateT, runState, state)
import Data.Array.MArray (Ix, MArray (newArray), readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.Unboxed (IArray, UArray, (!))
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

type RandomNumberType = Float

rng :: StateT Int64 Identity Int64
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
              let !(int, nextSeed) = runState rng s
               in ( do
                      writeArray arr ix (fromIntegral (int `div` 128) - 128)
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
      writeArray arr ix (f origin)

genOneCase ::
  (Int, Int, Int, Int) ->
  (Int64, Int64) ->
  ( UArray Int RandomNumberType,
    UArray Int RandomNumberType,
    UArray Int RandomNumberType
  )
genOneCase
  ( !inputHeight,
    !inputWidth,
    !weightHeight,
    !weightWidth
    )
  (!seed1, !seed2) =
    let !input =
          runSTUArray
            ( randMatrix inputHeight inputWidth seed1 ::
                (forall s. ST s (STUArray s Int RandomNumberType))
            )
        !weight =
          runSTUArray
            ( randMatrix weightHeight weightWidth seed2 ::
                (forall s. ST s (STUArray s Int RandomNumberType))
            )
        !output =
          runSTUArray
            ( convolution input (inputHeight, inputWidth) weight (weightHeight, weightWidth) ::
                (forall s. ST s (STUArray s Int RandomNumberType))
            )
     in (input, weight, output)

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
