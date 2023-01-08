{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array (Array)
import Data.Array.IArray (IArray, amap, bounds, elems, listArray, (!))
import Data.Ix (Ix)
import Data.List.Split (chunksOf)
import Data.String.Interpolate (i)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Random (randomIO)
import Data.List (foldl1')

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

randList :: Int -> IO [Float]
randList !size = replicateM size (randomIO :: IO Float)

randMatrix :: Int -> Int -> IO (Array Int (Array Int Float))
randMatrix !height !width = do
  ls <- randList (height * width)
  let ls2d = chunksOf width ls
  let lsArr = map (listArray (0, width - 1)) ls2d
  return $! listArray (0, height - 1) lsArr

convolution :: Array Int (Array Int Float) -> Array Int (Array Int Float) -> [[Float]]
convolution !input !weight =
  let hi = snd $ bounds input
      hw = snd $ bounds weight
      wi = snd $ bounds $ input ! 0
      ww = snd $ bounds $ weight ! 0
      res4dFlat = do
        rowIx <- [0 .. hi - hw]
        colIx <- [0 .. wi - ww]
        hIx <- [0 .. hw]
        wIx <- [0 .. ww]
        return $! (input ! (rowIx + hIx) ! (colIx + wIx)) * (weight ! hIx ! wIx)
      res3dFlat = map sum $ chunksOf (ww + 1) res4dFlat
      res2dFlat = map sum $ chunksOf (hw + 1) res3dFlat
   in chunksOf (wi - ww + 1) res2dFlat

listToString :: Show a => [a] -> String
listToString !xs =
  xs
    |> map show
    |> foldl1' (++)

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

outputOneCase :: (Int, Int, Int, Int) -> [Char] -> IO ()
outputOneCase (!inputHeight, !inputWidth, !weightHeight, !weightWidth) !path = do
  input <- randMatrix inputHeight inputWidth
  weight <- randMatrix weightHeight weightWidth
  let output = convolution input weight
      outputDir = [i|#{inputHeight}x#{inputWidth}_#{weightHeight}x#{weightWidth}|]
      inputFile = path ++ outputDir ++ "input.txt"
      weightFile = path ++ outputDir ++ "weight.txt"
      outputFile = path ++ outputDir ++ "output.txt"
  writeFile inputFile [i|#{inputHeight} #{inputWidth}\n|]
  writeFile weightFile [i|#{weightHeight} #{weightWidth}\n|]
  writeFile outputFile [i|#{inputHeight-weightHeight+1} #{inputWidth-weightWidth+1}\n|]
  appendFile inputFile $ twoDimArrayToString input
  appendFile weightFile $ twoDimArrayToString weight
  appendFile outputFile $ twoDimListToString output

outputCases :: [(Int, Int, Int, Int)] -> [Char] -> IO ()
outputCases !shapeList !path = do
  createDirectoryIfMissing True path
  mapM_ (\x -> outputOneCase x path) shapeList

main :: IO ()
main = do
  start <- getCurrentTime
  outputCases [(256, 256, 16, 16)] "./cases"
  end <- getCurrentTime
  putStrLn [i|#{end `diffUTCTime` start} elapsed.|]
