module Lib (translate, decode) where

import Data.List hiding (union)
import System.Environment
import Data.Int
import Data.Char
import Data.Maybe

import Data.Map (Map, (!), union)
import qualified Data.Map as Map

--TODO: Add Data type of ShalChar, don't translate other symbols

-- -------------------------

f_start_code = 0x2800

--f_start_code = 0x0020


-- -------------------------

--detranslate :: [Char] -> [Char]
--detranslate (translate a) = a


translate :: [Char] -> [Char]
translate l = unwords $ map (translate_word) (words l)

translate_word :: String -> String
translate_word w = map (codeToSym) $ fix $ translate_wordb (pre_fix w) False

fix = fix_tiend . fix_ends . fix_vols


pre_fix = pre_fixd . pre_fixa . pre_val_fix . (map toLower)

pre_val_fix :: [Char] -> [Char]
pre_val_fix [] = []
pre_val_fix (x:xs)
                 | x == 'й'         = 'и'  : pre_val_fix xs
                 | x == 'ь'         = '\'' : pre_val_fix xs
                 | x == 'ъ'         = '-'  : pre_val_fix xs
                 | x == 'щ'         = 'ш'  : pre_val_fix xs
                 | x `elem` Map.keys vl = vl!x : pre_val_fix xs
                 | otherwise       = x    : pre_val_fix xs
                 where
                 vl = Map.fromList (zip vals2 vals1)

pre_fixa :: [Char] -> [Char]
pre_fixa [] = []
pre_fixa (x:[])
             | x == 'я'   = 'а':[]
             | otherwise =  x :[]
pre_fixa (x:y:xs)
               | x == a             = 'а'  : pre_fix (y:xs)
               | x == ekr && y == ekr = x    : pre_fix xs
               | y == ekr           = x    : pre_fix (y:xs)
               | x == ekr && y == a   = ekra : pre_fix xs
               | otherwise         = x    : pre_fix (y:xs)
               where
               ekra = 'â'
               ekr  = '\\'
               a    = 'а'
-- â

pre_fixd :: [Char] -> [Char]
pre_fixd [] = []
pre_fixd (x:[]) = x:[]
pre_fixd (x:y:xs)
                | x == d && y == zh = g : pre_fixd xs
                | x == d && y == z  = j : pre_fixd xs
                | otherwise      = x : pre_fixd (y:xs)
                where
                d  = 'д'
                zh = 'ж'
                z  = 'з'
                g  = 'g'
                j  = 'j'

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
fix_ends (x : [])
                | isFir x   = toSep x : []
                | isMid x   = toEnd x : []
                | otherwise = x :[ ]
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
                |                   y == long_term =  x               : (fix_vols xs)
                | otherwise = x : fix_vols (y:xs)
                where
                shift_long = 0x0020
                long_term  = symCodeSh '\''


-- fix state 'а'
--fix_vols' :: [Int] -> [Int]
--fix_vols' [] = []
--fix_vols' (x:[]) = [x]
--fix_vols' (x:y:xs)
--        | y == mid_al && y == fir_al = mid_as : fix_vols' xs
--        | otherwise               = x      : fix_vols' xs
--        where
--        sep_al = f_start_code + 0x0010
--        fir_al = f_start_code + 0x0011
--        mid_al = f_start_code + 0x0012
--        end_al = f_start_code + 0x0013
--        sep_as = f_start_code + 0x0080
--        fir_as = f_start_code + 0x0081
--        mid_as = f_start_code + 0x0082
--        end_as = f_start_code + 0x0083
--        ekr = symCodeSh '\\'



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

translate_symbol 'â' Separate = toSep (vab!'а')
translate_symbol 'â' First    = toFir (vab!'а')
translate_symbol 'â' Middle   = toEnd (alb!'а')
translate_symbol 'â' Last     = toEnd (vab!'а')

translate_symbol 'а' Separate = toSep (alb!'а')
translate_symbol 'а' First    = toSep (alb!'а')
translate_symbol 'а' Middle   = toMid (vab!'а')
translate_symbol 'а' Last     = toEnd (alb!'а')

translate_symbol 'g' Middle = translate_symbol 'g' Last
translate_symbol 'j' Middle = translate_symbol 'j' Last

translate_symbol s code
                       | s `notElem` leters      = symCodeSh s
                       | code == Separate = symCodeSh s
                       | code == First    = symCodeSh s + shift_fir
                       | code == Middle   = symCodeSh s + shift_mid
                       | code == Last     = symCodeSh s + shift_end
                       where
                       leters = albata ++ valss
                       shift_fir = 1
                       shift_mid = 2
                       shift_end = 3

--
-- Блок работы с символами
--

albata = [ 'а', 'б', 'т', 'с'
         , 'л', 'в', 'д', 'з'
         , 'р', 'х', 'м', 'г'
         , 'к', 'н', 'п', 'ф'
         , 'ч', 'ц', 'ш', 'ж'
         , 'g', 'j'
         ]


vals1 = ['а', 'е', 'и', 'у', 'о']
vals2 = ['я', 'э', 'ы', 'ю', 'ё']
valss = vals1 ++ vals2

barbata = [',', '.']

numbata = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

sepcodes = [f_start_code + 0x0010, f_start_code + 0x0014 .. f_start_code + 0x0068]
vepcodes = [f_start_code + 0x0080, f_start_code + 0x0084 .. f_start_code + 0x00B3]
bepcodes = [f_start_code + 0x007D, f_start_code + 0x007F]
numcodes = [f_start_code + 0x0070, f_start_code + 0x0071 .. f_start_code + 0x0079]

allsepcodes = [head sepcodes .. last sepcodes]
allvepcodes = [head vepcodes .. last vepcodes]
allbepcodes = [head bepcodes .. last bepcodes]
allnumcodes = [head numcodes .. last numcodes]
allletters  =  allsepcodes   ++ allvepcodes


alb = Map.fromList $ zip albata sepcodes
vab = Map.fromList $ zip vals1  vepcodes

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

-- ------------------
-- Detranslate Block
-- ------------------

dalb = Map.fromList $ zip sepcodes albata
dvab = Map.fromList $ zip vepcodes vals1
dbab = Map.fromList $ zip bepcodes barbata
dnup = Map.fromList $ zip numcodes numbata
dall = dalb `union` dvab `union` dbab `union` dnup

pre_fix_decode :: [Char] -> [Char]
pre_fix_decode [] = []
pre_fix_decode (x:xs)
                    | xz `elem` longvals = toEnum (xz - 0x0020)   : pre_fix_decode xs
                    | xz == ati      = toEnum ta : toEnum ias : pre_fix_decode xs
                    | otherwise     = x                      : pre_fix_decode xs
                    where
                    longvals = [f_start_code + 0x00A0 .. f_start_code + 0x00B3]
                    xz  = fromEnum x
                    ati = f_start_code + 0x00C0                         -- code '-ти'
                    ta  = f_start_code + 0x001A                         -- code 'т'
                    ias = f_start_code + 0x008B -- code 'и'

pre_decode = (map (toEnum . toSep . fromEnum)) . pre_fix_decode

decode = decodeM . pre_decode

decodeM :: [Char] -> [Char]
decodeM [] = []
decodeM (x:xs)
            | (fromEnum x) `elem` Map.keys dall = dall!(fromEnum x) : decode xs
            | otherwise                    = x                 : decode xs
