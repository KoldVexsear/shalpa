module Lib (translate) where

import Data.List
import System.Environment
import Data.Int
import Data.Char
import Data.Maybe

import Data.Map (Map, (!))
import qualified Data.Map as Map

--translate :: [Char] -> [Char]
--translate t = unlines $ map (translate_line) (lines t)

translate :: [Char] -> [Char]
translate l = unwords $ map (translate_word) (words l)

translate_word :: String -> String
translate_word w = map (codeToSym) $ fix $ translate_wordb w False

fix = fix_tiend . fix_ends . fix_vols . fix_vols'

fix_tiend :: [Int] -> [Int]
fix_tiend [] = []
fix_tiend (x: []) = [x]
fix_tiend (x:y:xs)
                 | x == 0x281A && (y == 0x288B || y == 0x28AB) = 0x28C0 : fix_tiend xs
                 | otherwise = x : fix_tiend (y:xs)

fix_ends :: [Int] -> [Int]
fix_ends [] = []
fix_ends (x:[]) = [x]
fix_ends (x:y:xs)
                | not (xel bepcodes) && not (yel bepcodes) && (x `mod` 4 == 3 || x `mod` 4 == 0) && (y `mod` 4 == 1 || y `mod` 4 == 2) = x : fix_ends ((y - (y `mod` 4) + 1) : xs)
                | not (xel bepcodes) && not (yel bepcodes) && (x `mod` 4 == 3 || x `mod` 4 == 0) && (y `mod` 4 == 0 || y `mod` 4 == 3) = x : fix_ends ((y - (y `mod` 4)) : xs)
                | x == 0x060B  && not (yel bepcodes) && (y `mod` 4 == 1 || y `mod` 4 == 2) = fix_ends((y - (y `mod` 4) + 1) : xs)
                | x == 0x060B  && not (yel bepcodes) && (y `mod` 4 == 0 || y `mod` 4 == 3) = fix_ends ((y - (y `mod` 4)) : xs)
                | otherwise = x : fix_ends (y:xs)
                where
                   xel t = x `elem` t
                   yel t = y `elem` t

fix_vols :: [Int] -> [Int]
fix_vols [] = []
fix_vols (x:[]) = [x]
fix_vols (x:y:xs)
                | x >= 0x0680 && x <= 0x0693 && y == 0x060E = (x + 20) : (fix_vols xs)
                | otherwise = x : fix_vols (y:xs)


fix_vols' :: [Int] -> [Int]

fix_vols' [] = []
fix_vols' (x:xs)
        | x == 0x0612 = 0x0682 : fix_vols' xs
        | otherwise   = x : fix_vols' xs

translate_wordb :: String -> Bool -> [Int]
translate_wordb w key
               | len >= 3 && key == False = [translate_symbol f First]  ++ (translate_wordb m True) ++ [translate_symbol l Last]
               | len >= 3 && key == True  = [translate_symbol f Middle] ++ (translate_wordb m True) ++ [translate_symbol l Middle]
               | len == 2 && key == False = [translate_symbol f First]  ++ [translate_symbol l Last]
               | len == 2 && key == True  = [translate_symbol f Middle] ++ [translate_symbol l Middle]
               | len == 1 && key == False = [translate_symbol f Separate]
               | len == 1 && key == True  = [translate_symbol f Middle]
               | otherwise                = []
               where
                 f   = head w
                 m   = init $ tail w
                 l   = last w
                 len = length w


data Code = Separate | First | Middle | Last
          deriving (Eq)


translate_symbol :: Char -> Code -> Int

translate_symbol 'а' First  = translate_symbol 'а' Separate

translate_symbol 'g' Middle = translate_symbol 'g' Last
translate_symbol 'j' Middle = translate_symbol 'j' Last

translate_symbol s code
                       | s `elem` barbata = translate_symb_s s
                       | code == Separate = translate_symb_s s
                       | code == First    = translate_symb_f s
                       | code == Middle   = translate_symb_m s
                       | code == Last     = translate_symb_l s

translate_symb_s s = findTablaCode s
translate_symb_f f = findTablaCode f + 1
translate_symb_m m = findTablaCode m + 2
translate_symb_l l = findTablaCode l + 3



albataM = [ 'а', 'б', 'т', 'с'
         , 'л', 'в', 'д', 'з'
         , 'р', 'х', 'м', 'г'
         , 'к', 'н', 'п', 'ф'
         , 'ч', 'ц', 'ш', 'ж'
         , 'g', 'j'
         ]
albataMb = map toUpper albataM
albatas  = albataM ++ albataMb

vals1 = ['а', 'е', 'и', 'у', 'о']
vals2 = ['я', 'э', 'ы', 'ю', 'ё']
valss = vals1 ++ vals2
vabatas  = valss ++ map toUpper valss

barbata = [',', '\'', '.']

numbata = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

sepcodes = [0x2810, 0x2814 .. 0x2868]
vepcodes = [0x2880, 0x2884 .. 0x28A4]
bepcodes = [0x287D, 0x287E,  0x287F]
numcodes = [0x2870, 0x2871 .. 0x2879]

alb = Map.fromList $ zip albatas (sepcodes ++ sepcodes)
vab = Map.fromList $ zip vabatas (vepcodes ++ vepcodes ++ vepcodes ++ vepcodes)

bab = Map.fromList $ zip barbata bepcodes
nup = Map.fromList $ zip numbata numcodes

tabla = zip albataM sepcodes

isInTabla s = True


findTablaCode :: Char -> Int
findTablaCode s | s `elem` al = alb ! s
                | s `elem` ba = bab ! s
                | s `elem` va = vab ! s
                | s `elem` na = nup ! s
                | otherwise = 0x0600
                where
                  cod s t = findIndex (\b -> b == s) t
                  al = Map.keys alb
                  va = Map.keys vab
                  ba = Map.keys bab
                  na = Map.keys nup

codeToSym c = toEnum (read (show c):: Int) :: Char