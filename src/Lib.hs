module Lib (translate, debug, symCodeSh, translate_wordb) where

import Data.List hiding (union)
import System.Environment
import Data.Int
import Data.Char
import Data.Maybe

import Data.Map (Map, (!), union)
import qualified Data.Map as Map

-- -------------------------

f_start_code = 0x2800

-- -------------------------
debug = print . (map (codeToSym)) . fix_vols'

translate :: [Char] -> [Char]
translate l = unwords $ map (translate_word) (words l)

translate_word :: String -> String
translate_word w = map (codeToSym) $ fix $ translate_wordb w False

fix = fix_tiend . fix_ends . fix_vols . fix_vols'


-- transform end -ти to ligature code
fix_tiend :: [Int] -> [Int]
fix_tiend [] = []
fix_tiend (x: []) = [x]
fix_tiend (x:y:xs)
                 | x == ta && y `elem` ias = ati : fix_tiend xs
                 | otherwise = x : fix_tiend (y:xs)
                 where
                 ta  =  f_start_code + 0x001A                         -- code 'т'
                 ias = [f_start_code + 0x008B, f_start_code + 0x00AB] -- code 'и'
                 ati =  f_start_code + 0x00C0                         -- code '-ти'

-- fix breaks after end letters (bya)
fix_ends :: [Int] -> [Int]
fix_ends [] = []
fix_ends (x : []) = [x]
fix_ends (x:y:xs)
                | not_zap x && not_zap y && (isEnd x ||  isSep x) && isMid y = x : fix_ends (toFir y : xs)
                | not_zap x && not_zap y && (isEnd x ||  isSep x) && isMid y = x : fix_ends (toSep y   : xs)

                | not_zap x && isFir x &&  y == sepa = toSep x : fix_ends (y : xs)
                | not_zap x && isMid   x &&  y == sepa = toEnd x : fix_ends (y : xs)

                | x == sepa && not_zap y &&  isMid y = fix_ends (toFir y : xs)
                | x == sepa && not_zap y &&  isEnd y = fix_ends (toSep   y : xs)

                | otherwise = x : fix_ends (y:xs)
                where
                   not_zap x = x `notElem` bepcodes ++ numcodes
                   sepa  = symCodeSh '-'


-- fix long cols
fix_vols :: [Int] -> [Int]
fix_vols [] = []
fix_vols (x:[]) = [x]
fix_vols (x:y:xs)
                | x `elem` allvepcodes && y == long_term = (x + shift_long) : (fix_vols xs)
                | otherwise = x : fix_vols (y:xs)
                where
                shift_long = 0x0020
                long_term  = symCodeSh '\''


-- fix state 'а'
fix_vols' :: [Int] -> [Int]
fix_vols' [] = []
fix_vols' (x:[]) = [x]
fix_vols' (x:y:xs)
        | x == ekr && y == ekr       = x      : fix_vols' xs
        | y == ekr                 = x      : fix_vols' (y:xs)
        | x == ekr && y == sep_al    = sep_as : fix_vols' xs
        | x == ekr && y == fir_al    = fir_as : fix_vols' xs
        | x == ekr && y == end_al    = end_as : fix_vols' xs
        | y == mid_al && y == fir_al = mid_as : fix_vols' xs
        | otherwise               = x      : fix_vols' xs
        where
        sep_al = f_start_code + 0x0010
        fir_al = f_start_code + 0x0011
        mid_al = f_start_code + 0x0012
        end_al = f_start_code + 0x0013
        sep_as = f_start_code + 0x0080
        fir_as = f_start_code + 0x0081
        mid_as = f_start_code + 0x0082
        end_as = f_start_code + 0x0083
        ekr = symCodeSh '\\'



--TODO FIX EKRAN SYM!
-- translating word with key of middle
translate_wordb :: String -> Bool -> [Int]
translate_wordb w key
               | len >= 3 && key == False = [translate_symbol f First]  ++ (translate_wordb m True) ++ [translate_symbol l Last]
               | len >= 3 && key == True  = [translate_symbol f Middle] ++ (translate_wordb m True) ++ [translate_symbol l Middle]
               | len == 2 && key == False = [translate_symbol f First]  ++ [translate_symbol l Last]
               | len == 2 && key == True  = [translate_symbol f Middle] ++ [translate_symbol l Middle]
               | len == 1 && key == False = [translate_symbol f Separate]
               | len == 1 && key == True  = [translate_symbol f Middle]
               | otherwise             = []
               where
                 f   = head w
                 m   = init $ tail w
                 l   = last w
                 len = length w



data Code = Separate | First | Middle | Last
          deriving (Eq)


translate_symbol :: Char -> Code -> Int

translate_symbol 'g' Middle = translate_symbol 'g' Last
translate_symbol 'j' Middle = translate_symbol 'j' Last

translate_symbol s code
                       | s `notElem` leters      = symCodeSh s
                       | code == Separate = symCodeSh s
                       | code == First    = symCodeSh s + shift_fir
                       | code == Middle   = symCodeSh s + shift_mid
                       | code == Last     = symCodeSh s + shift_end
                       where
                       leters = albatas ++ vabatas
                       shift_fir = 1
                       shift_mid = 2
                       shift_end = 3

--
-- Блок работы с символами
--

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

barbata = [',', '.']

numbata = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

sepcodes = [f_start_code + 0x0010, f_start_code + 0x0014 .. f_start_code + 0x0068]
vepcodes = [f_start_code + 0x0080, f_start_code + 0x0084 .. f_start_code + 0x00A4]
bepcodes = [f_start_code + 0x007D, f_start_code + 0x007F]
numcodes = [f_start_code + 0x0070, f_start_code + 0x0071 .. f_start_code + 0x0079]

allsepcodes = [head sepcodes .. last sepcodes]
allvepcodes = [head vepcodes .. last vepcodes]
allbepcodes = [head bepcodes .. last bepcodes]
allnumcodes = [head numcodes .. last numcodes]
allletters  =  allsepcodes   ++ allvepcodes


alb = Map.fromList $ zip albatas (sepcodes ++ sepcodes)
vab = Map.fromList $ zip vabatas (vepcodes ++ vepcodes ++ vepcodes ++ vepcodes)

bab = Map.fromList $ zip barbata bepcodes
nup = Map.fromList $ zip numbata numcodes

alls = alb `union` vab `union` bab `union` nup

symCodeSh :: Char -> Int
symCodeSh s | s `elem` Map.keys alls = alls ! s
                | otherwise = fromEnum s
                where
                  cod s t = findIndex (\x -> x == s) t

codeToSym c = toEnum (read (show c):: Int) :: Char

toSep n = n - (n `mod` 4) + 0
toFir n = n - (n `mod` 4) + 1
toMid n = n - (n `mod` 4) + 2
toEnd n = n - (n `mod` 4) + 3
isSep n = n `mod` 4 == 0
isFir n = n `mod` 4 == 1
isMid n = n `mod` 4 == 2
isEnd n = n `mod` 4 == 3