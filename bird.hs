import Data.Char

cuncurry :: (a -> b -> c) -> ((a, b) -> c)
cuncurry f (x, y) = f x y


ccurry :: ((a, b) -> c) ->  (a -> b -> c)
ccurry f x y = f (x, y)

cfact :: Int -> Int
cfact n
  | n < 0 = error "not defined for negative numbers"
  | n == 0 = 1
  | n > 0 = n * cfact(n -1 )

-- flip (curry f) = curry (f . swap)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

(^),(!) :: Bool -> Bool -> Bool
False ^ x = False
True ^ x = x

False ! x = x
True ! x = True

data CBool = CTrue | CFalse
                     deriving (Show)

-- class CEnum a where
--   toEnum :: a -> Int
--   fromEnum :: Int -> a

-- class CEq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- instance CEnum CBool where
--   toEnum CTrue = 1
--   toEnum CFalse = 0
--   fromEnum n = if n == 0
--                   then CFalse
--                        else CTrue

-- instance CEq CBool where
--   (==) a b = toEnum a == toEnum b
--   (/=) a b = not (a == b)

write :: String -> IO ()
-- write [] = return ()
-- write (c : cs) = putChar c >> write cs
write = foldr (>>) (return()) . map putChar

writeLn :: String -> IO ()
writeLn cs = write cs >> putChar '\n'

done :: IO ()
done = return ()

readn :: Int -> IO String
readn 0 = return []
readn n = getChar >>= q
  where q c = readn (n - 1) >>= r
              where r cs = return (c : cs)

reveal :: IO ()
reveal = do c <- getChar
            putChar 'X'
            putChar 'X'
            putChar 'X'
            putChar '\n'
            write (show (ord c))

palindrome :: IO ()
palindrome = do write "Enter a string: "
                cs <- getLine
                if palin cs
                  then writeLn "Yes"
                  else writeLn "No"

palin :: String -> Bool
palin xs = (ys == reverse ys)
           where ys = map toUpper (filter isLetter xs)

diamond :: (a -> IO b) -> (b -> IO g) -> (a -> IO g)
diamond f g = (\x -> f x >>= (\y -> g y))

-- TODO
-- mybind :: IO a -> (a -> IO b) -> IO b
-- mybind m f =  
-- (\x -> getChar)
-- (\x -> putChar x)

data Term = Con Int | Div Term Term | Try Term Term
                      deriving(Show)

-- First evaluator
-- eval :: Term -> Int
-- eval (Con x) = x
-- eval (Div t u) = (eval t) `div` (eval u)


data Exc a = Raise Exception | Return a

instance Show a => Show (Exc a) where
  show (Raise e) = "exception: " ++ e
  show (Return x) = "value: " ++ show x

type Exception = String

-- eval :: Term -> Exc Int
-- eval (Con x) = Return x
-- eval (Div t u) =
--   h (eval t)
--   where
--     h (Raise e) = Raise e
--     h (Return x) = h' (eval u)
--                    where
--                      h' (Raise e') = Raise e'
--                      h' (Return y)
--                        = if y == 0
--                             then Raise "division by zero"
--                                  else Return (x `div` y)


-- eval :: Term -> St Int
-- eval (Con x) = MkSt f
--               where f s = (x, s)

-- eval (Div t u) = MkSt f
--                  where
--                    f s = (x `div` y, s'' + 1)
--                          where
--                            (x, s') = apply (eval t) s
--                            (y, s'') = apply (eval u) s'

instance Show a => Show (St a) where
  show f = "value: " ++ show x ++ ", count: " ++ show s
    where (x, s) = apply f 0

-- Monadic evaluator 
eval :: Monad m => Term -> m Int
eval (Con x) = return x
eval (Div t u) = do x <- eval t
                    y <- eval u
                    return (x `div` y)

newtype Id a = MkId a
instance Monad Id where
  return x = MkId x
  (MkId x) >>= q = q x

instance Show a => Show (Id a) where
  show (MkId x) = "value: " ++ show x

evalId :: Term -> Id Int
evalId = eval

instance Monad Exc where
  return x = Return x
  (Raise e) >>= q = Raise e
  (Return x) >>= q = q x

raise :: Exception -> Exc a
raise e = Raise e

evalEx :: Term -> Exc Int
evalEx (Con x) = return x
evalEx (Div t u) = do x <- eval t
                      y <- eval u
                      if y == 0
                        then raise "division by zero"
                        else return (x `div` y)

type State = Int
newtype St a = MkSt (State -> (a, State))

instance Monad St where
  return x = MkSt f where f s = (x, s)
  p >>= q = MkSt f
            where
            f s = apply (q x) s'
              where (x, s') = apply p s

tick :: St ()
tick = MkSt f
       where f s = ((), s + 1)

apply :: St a -> State -> (a, State)
apply (MkSt f) s = f s

-- evalSt (Div t u) = do x <- evalSt u
--                       y <- evalSt t
--                       tick
--                       return (x `div` y)
evalSt :: Term -> St Int
evalSt (Con x) = return x
evalSt (Div t u) = tick >>=
                   \s -> evalSt u >>=
                         \eu -> evalSt t >>=
                                \et -> return (et `div` eu)

newtype Out a = MkOut (Output, a)
type Output = String

instance Show a => Show (Out a) where
  show (MkOut (ox, x)) = ox ++ "value: " ++ show x

instance Monad Out where
  return x = MkOut ("", x)
  p >>= q = MkOut (ox ++ oy, y)
    where MkOut (ox, x) = p
          MkOut (oy, y) = q x

out :: Output -> Out ()
out ox = MkOut (ox, ())

line :: Term -> Int -> Output
line t x = "term: " ++ show t ++ ", yields " ++ show x

evalOut :: Term -> Out Int
evalOut (Con x) = do out (line (Con x) x)
                     return x
evalOut (Div t u) = do x <- evalOut t
                       y <- evalOut u
                       out (line (Div t u) (x `div` y))
                       return (x `div` y)
