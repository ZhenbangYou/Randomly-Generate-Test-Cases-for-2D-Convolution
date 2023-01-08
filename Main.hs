{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (forkFinally, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Array (Array)
import Data.Array.IArray (IArray, amap, bounds, elems, listArray, (!))
import Data.Ix (Ix)
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.Random (randomIO)
import System.Random.Stateful (Random)

type RandomNumberType = Float

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

randList :: (Random a, MonadIO m) => Int -> m [a]
randList !size = replicateM size randomIO

randMatrix :: (Random e, MonadIO m, IArray a1 e, IArray a2 (a1 Int e)) => Int -> Int -> m (a2 Int (a1 Int e))
randMatrix !height !width = do
  !ls <- randList (height * width)
  let !ls2d = chunksOf width ls
      !lsArr = map (listArray (0, width - 1)) ls2d
  return $! listArray (0, height - 1) lsArr

convolution ::
  ( IArray a1 e,
    IArray a2 (a1 Int e),
    IArray a3 e,
    IArray a4 (a3 Int e),
    Num e
  ) =>
  a4 Int (a3 Int e) ->
  a2 Int (a1 Int e) ->
  [[e]]
convolution !input !weight =
  let !hi = snd $ bounds input
      !hw = snd $ bounds weight
      !wi = snd $ bounds $ input ! 0
      !ww = snd $ bounds $ weight ! 0
      !res4dFlat = do
        !rowIx <- [0 .. hi - hw]
        !colIx <- [0 .. wi - ww]
        !hIx <- [0 .. hw]
        !wIx <- [0 .. ww]
        return $! (input ! (rowIx + hIx) ! (colIx + wIx)) * (weight ! hIx ! wIx)
      !res3dFlat = map sum $ chunksOf (ww + 1) res4dFlat
      !res2dFlat = map sum $ chunksOf (hw + 1) res3dFlat
   in chunksOf (wi - ww + 1) res2dFlat

listToString :: Show a => [a] -> String
listToString !xs =
  xs
    |> map show
    |> foldl1' (\a b -> a ++ " " ++ b)

twoDimListToString :: Show a => [[a]] -> String
twoDimListToString !xss =
  xss
    |> map listToString
    |> foldl1' (\a b -> a ++ "\n" ++ b)

twoDimArrayToString ::
  ( IArray a1 (a2 i1 e),
    IArray a1 [e],
    IArray a2 e,
    Ix i2,
    Ix i1,
    Show e
  ) =>
  a1 i2 (a2 i1 e) ->
  String
twoDimArrayToString !xss =
  xss
    |> amap elems
    |> elems
    |> twoDimListToString

outputOneCase :: (Int, Int, Int, Int) -> FilePath -> IO ()
outputOneCase (!inputHeight, !inputWidth, !weightHeight, !weightWidth) !path = do
  !input <- randMatrix inputHeight inputWidth :: IO (Array Int (Array Int RandomNumberType))
  !weight <- randMatrix weightHeight weightWidth :: IO (Array Int (Array Int RandomNumberType))
  let !output = convolution input weight
      !outputDirPath = path </> [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]
      !inputFile = outputDirPath </> "input" <.> "txt"
      !weightFile = outputDirPath </> "weight" <.> "txt"
      !outputFile = outputDirPath </> "output" <.> "txt"
  createDirectoryIfMissing True outputDirPath
  writeFile inputFile [i|#{inputHeight} #{inputWidth}\n|]
  writeFile weightFile [i|#{weightHeight} #{weightWidth}\n|]
  writeFile outputFile [i|#{inputHeight-weightHeight+1} #{inputWidth-weightWidth+1}\n|]
  appendFile inputFile $ twoDimArrayToString input
  appendFile weightFile $ twoDimArrayToString weight
  appendFile outputFile $ twoDimListToString output

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
  outputCases [(128, 128, 8, 8), (128, 128, 16, 16), (128, 128, 32, 32), (128, 128, 64, 64)] "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
