module NLib ( toSString
            , sUnwords
            , sUnlines
            ) where

import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import System.Environment

import qualified Data.Map as Map

data Pos = Separate
          | First
          | Middle
          | Last
        deriving (Eq, Show)

data SChar = SLit Char Pos | SMLit Char Pos [Mod] | SNum Int | SSpec Char | SOth Char | SLig
    deriving (Eq, Show)
data SLig = SLigTI | SLigTA | SLigMA | SLigPA | SLigGA | SLigJA
    deriving (Eq, Show)
type SString = [SChar]
data Mod = Palat | Long | Diacr
    deriving (Eq, Show)

charSetMods :: SChar -> [Mod] -> SChar
charSetMods (SLit c p)    ms = SMLit c p ms
charSetMods (SMLit c p _) ms = SMLit c p ms
charSetMods x _              = x

charAddMod :: SChar -> Mod -> SChar
charAddMod (SLit c p)      m = SMLit c p [m]
charAddMod (SMLit c p oms) m = SMLit c p (oms ++ [m])
charAddMod x _               = x

sIsSpace (SSpec c) = isSpace c
sIsSpace _ = False

sWords :: SString -> [SString]
sWords s = case dropWhile sIsSpace s of
                [] -> []
                s' -> w : sWords s''
                    where (w, s'') = break sIsSpace s'

sUnwords :: [SString] -> SString
sUnwords [] = []
sUnwords ws = foldr1 (\w s -> w ++ SSpec ' ' : s) ws

sLines :: SString -> [SString]
sLines s = cons (case break (== (SSpec '\n')) s of
                   (l, s') -> (l, case s' of
                                   [] -> []
                                   _:s'' -> sLines s''))
         where cons ~(h, t) = h : t

sUnlines :: [SString] -> SString
sUnlines = concatMap (++ [SSpec '\n'])

toSString :: String -> SString
toSString s = sUnlines $ map lineFrom (lines s)

lineFrom :: String -> SString
lineFrom s = sUnwords (map wordFrom (words s))

wordFrom :: String -> SString
wordFrom s = [(SLit (head s) First)] ++ (map (\c -> SLit c Middle) (init (tail s))) ++ [(SLit (last s) Last)]

preFix :: String -> String
preFix s = (map toLower s)

simpExchange :: String -> String
simpExchange s = simpExch' [] s
simpExch' os [] = os
simpExch' os (x:xs) | x == 'й' = simpExch' os ('и':xs)
                    | x == 'д' && (head xs) == 'ж' = simpExch' os ('g':(tail xs))
                    | x == 'д' && (head xs) == 'з' = simpExch' os ('j':(tail xs))
                    | x == 'п' && (head xs) == 'с' = simpExch' os ('s':(tail xs))
                    | x == 'к' && (head xs) == 'с' = simpExch' os ('X':(tail xs)) --TODO
                    | otherwise = simpExch' (os ++ [x]) xs