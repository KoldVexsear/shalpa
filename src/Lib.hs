module Lib
    ( translate
    , decode
    ) where

import Data.Char
import Data.Int
import Data.List hiding (union)
import Data.Maybe
import System.Environment

import Data.Map (Map, (!), union)
import qualified Data.Map as Map

--TODO: Add Data type of ShalChar, don't translate other symbols
-- -------------------------
fStartCode = 0x2800

--f_start_code = 0x0020
-- -------------------------
--detranslate :: [Char] -> [Char]
--detranslate (translate a) = a
translate :: String -> String
translate l = unwords $ map translateWord (words l)

translateWord :: String -> String
translateWord w = map codeToSym $ fix $ translateWordb (preFix w) False

fix = fixTiend . fixEnds . fixVols

preFix = preFixd . preFixa . preValFix . map toLower

preValFix :: String -> String
preValFix [] = []
preValFix (x:xs)
    | x == 'й' = 'и' : preValFix xs
    | x == 'ь' = '\'' : preValFix xs
    | x == 'ъ' = '-' : preValFix xs
    | x == 'щ' = 'ш' : preValFix xs
    | x `elem` Map.keys vl = vl ! x : preValFix xs
    | otherwise = x : preValFix xs
  where
    vl = Map.fromList (zip vals2 vals1)

preFixa :: String -> String
preFixa [] = []
preFixa [x]
    | x == 'я'   = "а"
    | otherwise = [x]
preFixa (x:y:xs)
    | x == a             = 'а'   : preFix (y : xs)
    | x == ekr && y == ekr =  x    : preFix      xs
    | y == ekr           =  x    : preFix (y : xs)
    | x == ekr && y == a   =  ekra : preFix      xs
    | otherwise         =  x    : preFix (y : xs)
  where
    ekra = 'â'
    ekr  = '\\'
    a    = 'а'

-- â
preFixd :: String -> String
preFixd []   = []
preFixd [x] = [x]
preFixd (x:y:xs)
    | x == d && y == zh = g : preFixd xs
    | x == d && y == z  = j : preFixd xs
    | otherwise      = x : preFixd (y : xs)
  where
    d = 'д'
    zh = 'ж'
    z = 'з'
    g = 'g'
    j = 'j'

-- transform end -ти to ligature code
fixTiend :: [Int] -> [Int]
fixTiend [] = []
fixTiend [x] = [x]
fixTiend (x:y:xs)
    | x == ta && y `elem` ias = ati : fixTiend xs
    | otherwise = x : fixTiend (y : xs)
  where
    ta = fStartCode + 0x001A -- code 'т'
    ias = [fStartCode + 0x008B, fStartCode + 0x00AB] -- code 'и'
    ati = fStartCode + 0x00C0 -- code '-ти'

-- fix breaks after end letters (bya)
fixEnds :: [Int] -> [Int]
fixEnds [] = []
fixEnds [x]
    | isFir x = [toSep x]
    | isMid x = [toEnd x]
    | otherwise = [x]
fixEnds (x:y:xs)
    | not_zap x && not_zap y && (isEnd x || isSep x) && isMid y = x : fixEnds (toFir y : xs)
    | not_zap x && not_zap y && (isEnd x || isSep x) && isMid y = x : fixEnds (toSep y : xs)
    | not_zap x && isFir x && y == sepa = toSep x : fixEnds (y : xs)
    | not_zap x && isMid x && y == sepa = toEnd x : fixEnds (y : xs)
    | x == sepa && not_zap y && isMid y = fixEnds (toFir y : xs)
    | x == sepa && not_zap y && isEnd y = fixEnds (toSep y : xs)
    | otherwise = x : fixEnds (y : xs)
  where
    not_zap x = x `notElem` bepcodes ++ numcodes
    sepa = symCodeSh '-'

-- fix long cols
fixVols :: [Int] -> [Int]
fixVols [] = []
fixVols [x] = [x]
fixVols (x:y:xs)
    | x `elem` allvepcodes && y == long_term = (x + shift_long) : fixVols xs
    | y == long_term = x : fixVols xs
    | otherwise = x : fixVols (y : xs)
  where
    shift_long = 0x0020
    long_term = symCodeSh '\''

-- translating word with key of middle
translateWordb :: String -> Bool -> [Int]
translateWordb w key
    | len >= 3 && not key = [translateSymbol f First] ++ translateWordb m True ++ [translateSymbol l Last]
    | len >= 3 && key = [translateSymbol f Middle] ++ translateWordb m True ++ [translateSymbol l Middle]
    | len == 2 && not key = translateSymbol f First : [translateSymbol l Last]
    | len == 2 && key = translateSymbol f Middle : [translateSymbol l Middle]
    | len == 1 && not key = [translateSymbol f Separate]
    | len == 1 && key = [translateSymbol f Middle]
    | otherwise = []
  where
    f = head w
    m = init $ tail w
    l = last w
    len = length w

data Code
    = Separate
    | First
    | Middle
    | Last
    deriving (Eq)

translateSymbol :: Char -> Code -> Int
translateSymbol 'â' Separate = toSep (vab ! 'а')
translateSymbol 'â' First = toFir (vab ! 'а')
translateSymbol 'â' Middle = toEnd (alb ! 'а')
translateSymbol 'â' Last = toEnd (vab ! 'а')
translateSymbol 'а' Separate = toSep (alb ! 'а')
translateSymbol 'а' First = toSep (alb ! 'а')
translateSymbol 'а' Middle = toMid (vab ! 'а')
translateSymbol 'а' Last = toEnd (alb ! 'а')
translateSymbol 'g' Middle = translateSymbol 'g' Last
translateSymbol 'j' Middle = translateSymbol 'j' Last
translateSymbol s code
    | s `notElem` leters = symCodeSh s
    | code == Separate = symCodeSh s
    | code == First = symCodeSh s + shift_fir
    | code == Middle = symCodeSh s + shift_mid
    | code == Last = symCodeSh s + shift_end
  where
    leters = albata ++ valss
    shift_fir = 1
    shift_mid = 2
    shift_end = 3

--
-- Блок работы с символами
--
albata = ['а', 'б', 'т', 'с', 'л', 'в', 'д', 'з', 'р', 'х', 'м', 'г', 'к', 'н', 'п', 'ф', 'ч', 'ц', 'ш', 'ж', 'g', 'j']

vals1 = ['а', 'е', 'и', 'у', 'о']

vals2 = ['я', 'э', 'ы', 'ю', 'ё']

valss = vals1 ++ vals2

barbata = [',', '.']

numbata = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

sepcodes = [fStartCode + 0x0010,fStartCode + 0x0014 .. fStartCode + 0x0068]

vepcodes = [fStartCode + 0x0080,fStartCode + 0x0084 .. fStartCode + 0x00B3]

bepcodes = [fStartCode + 0x007D, fStartCode + 0x007F]

numcodes = [fStartCode + 0x0070,fStartCode + 0x0071 .. fStartCode + 0x0079]

allsepcodes = [head sepcodes .. last sepcodes]

allvepcodes = [head vepcodes .. last vepcodes]

allbepcodes = [head bepcodes .. last bepcodes]

allnumcodes = [head numcodes .. last numcodes]

allletters = allsepcodes ++ allvepcodes

alb = Map.fromList $ zip albata sepcodes

vab = Map.fromList $ zip vals1 vepcodes

bab = Map.fromList $ zip barbata bepcodes

nup = Map.fromList $ zip numbata numcodes

alls = alb `union` vab `union` bab `union` nup

symCodeSh :: Char -> Int
symCodeSh s
    | s `elem` Map.keys alls = alls ! s
    | otherwise = fromEnum s

codeToSym c = toEnum (read (show c) :: Int) :: Char

toSep n = n - (n `mod` 4) + 0

toFir n = n - (n `mod` 4) + 1

toMid n = n - (n `mod` 4) + 2

toEnd n = n - (n `mod` 4) + 3

isSep n = n `mod` 4 == 0

isFir n = n `mod` 4 == 1

isMid n = n `mod` 4 == 2

isEnd n = n `mod` 4 == 3

-- ------------------
-- Detranslate Block
-- ------------------
dalb = Map.fromList $ zip sepcodes albata

dvab = Map.fromList $ zip vepcodes vals1

dbab = Map.fromList $ zip bepcodes barbata

dnup = Map.fromList $ zip numcodes numbata

dall = dalb `union` dvab `union` dbab `union` dnup

preFixDecode :: String -> String
preFixDecode [] = []
preFixDecode (x:xs)
    | xz `elem` longvals = toEnum (xz - 0x0020) : preFixDecode xs
    | xz == ati = toEnum ta : toEnum ias : preFixDecode xs
    | xz == g = toEnum d : toEnum zh : preFixDecode xs
    | xz == j = toEnum d : toEnum z : preFixDecode xs
    | otherwise = x : preFixDecode xs
  where
    longvals = [fStartCode + 0x00A0 .. fStartCode + 0x00B3]
    xz = fromEnum x
    ati = fStartCode + 0x00C0 -- code '-ти'
    ta = fStartCode + 0x001A -- code 'т'
    ias = fStartCode + 0x008B -- code 'и'
    g = fStartCode + 0x0060
    j = fStartCode + 0x0064
    d = fStartCode + 0x0028
    zh = fStartCode + 0x005C
    z = fStartCode + 0x002C

preDecode = map (toEnum . toSep . fromEnum) . preFixDecode

decode = decodeM . preDecode

decodeM :: String -> String
decodeM [] = []
decodeM (x:xs)
    | fromEnum x `elem` Map.keys dall = dall ! fromEnum x : decode xs
    | otherwise = x : decode xs