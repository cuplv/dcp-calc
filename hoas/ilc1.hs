-- /show
{-# LANGUAGE 
  DataKinds,
  FlexibleContexts,
  FlexibleInstances,
  FunctionalDependencies,
  KindSignatures,
  MultiParamTypeClasses,
  NoMonomorphismRestriction,
  OverlappingInstances,
  RankNTypes, 
  TypeFamilies,
  TypeOperators,
  UndecidableInstances,
  ImpredicativeTypes
 #-}
--
-- Type level Nats
--
import Control.Concurrent
import Control.Concurrent.Chan
import Prelude hiding (drop)

data Nat = Z | S Nat

--
-- Type level Nat equality
--
class EQ (x::Nat) (y::Nat) (b::Bool) | x y -> b
instance EQ x x True
instance (b ~ False) => EQ x y b

--
-- Type level machinery for consuming a variable in a list of variables.
--
class Consume (v::Nat) (i::[Maybe Nat]) (o::[Maybe Nat]) | v i -> o
class Consume1 (b::Bool) (v::Nat) (x::Nat) (i::[Maybe Nat]) (o::[Maybe Nat]) | b v x i -> o
instance (Consume v i o) => Consume v (Nothing ': i) (Nothing ': o)
instance (EQ v x b, Consume1 b v x i o) => Consume v (Just x ': i) o
instance Consume1 True v x i (Nothing ': i)
instance (Consume v i o) => Consume1 False v x i (Just x ': o)

type LinVar repr (vid::Nat) a = forall (v::Nat) (i::[Maybe Nat]) (o::[Maybe Nat]). (Consume vid i o) => repr v i o a

type RegVar repr a = forall (v::Nat) (h::[Maybe Nat]) . repr v h h a

-- to distinguish linear arrow from regular arrow
newtype a :-<>: b = Lolli {unLolli :: a -> b}
data a :*: b = Tensor a b deriving Show
data One = One
type a :&: b = (a, b)
type Top = ()
data a :+: b = Inl a | Inr b deriving Show
data Zero 
newtype Bang a = Bang {unBang :: a}

-- Channel type
newtype Rd a = Rd {unRd :: Chan a }
newtype Wr a = Wr {unWr :: Chan a }
newtype Base a = Base { unBase :: a } deriving Show

class Lin (repr :: Nat -> [Maybe Nat] -> [Maybe Nat] -> * -> *) where
--    llam :: (LinVar repr vid a -> repr (S vid) (Just vid ': hi) (Nothing ': ho) b) -> 
--            repr vid hi ho (a :-<>: b)
    nu :: ((RegVar repr     (Wr a), {-- write end --}
            LinVar repr vid (Rd a)) -> {--  read end --}
           repr (S vid) (Just vid ': hi) (Nothing ': ho) b) -> repr vid hi ho b
    (|||) :: repr vid hi h a -> repr vid h ho b -> repr vid hi ho b
    base :: a -> repr vid h h (Base a)
    app :: (a -> repr vid h h b) -> repr vid h h (Base a) -> repr vid h h b
    prt :: String -> repr vid h h ()
    stop :: repr vid h h ()
    wr :: repr vid hi h (Wr a) -> repr vid h ho a -> repr vid hi ho ()
    rd :: repr vid hi ho (Rd a) -> repr vid hi ho (Rd a :*: a)
    lett :: repr vid hi h a -> (LinVar repr vid a -> repr vid h ho b) -> repr vid hi ho b
    drop :: repr vid hi h a -> repr vid h ho b -> repr vid hi ho b
--    (<^>) :: repr vid hi h (a :-<>: b) -> repr vid h ho a -> repr vid hi ho b

    -- Pair
    (<**>) :: repr vid hi h a -> repr vid h ho b ->
             repr vid hi ho (a :*: b)
    letStar :: repr vid hi h (a :*: b) ->
               ((LinVar repr vid a,
                LinVar repr (S vid) b) -> 
                    repr (S (S vid)) 
                         (Just vid ': Just (S vid) ': h) 
                         (Nothing ': Nothing ': ho)
                         c
               ) -> 
               repr vid hi ho c

{--    (!) :: repr vid tf h h a -> repr vid False h h (Bang a)
    letBang :: repr vid tf0 hi h (Bang a) -> 
               (RegVar repr a -> repr vid tf1 h ho b) -> 
               repr vid (Or tf0 tf1) hi ho b
   --} 
type Defn a = forall repr v h . (Lin repr) => repr v h h a

defn :: Defn a -> Defn a
defn x = x
-- show
newtype R (vid::Nat) (hi::[Maybe Nat]) (ho::[Maybe Nat]) a = R {unR :: IO a}

instance Lin R where
--  llam f = R $ Lolli $ \x -> unR $ f $ R x
--  f <^> x = R $ unLolli (unR f) (unR x)
    nu f = R $ do
      x <- newChan;
      unR $ f (R $ return $ Wr x, R $ return $ Rd x)
    p ||| q = R $ do
      forkIO $ unR p >> return ()
      unR q
    base x  = R $ return $ Base x
    app f x = R $ do
                x' <- unR x
                unR $ f (unBase x')
    prt x = R $ putStrLn x
    stop = R $ return ()
    wr c x = R $ do
               c' <- unR c
               unR x >>= writeChan (unWr c')
    rd c = R $ do
             c' <- unR c
             x <- readChan (unRd c')
             return $ Tensor c' x
    x <**> y = R $ do
                 x' <- unR x
                 y' <- unR y
                 return $ Tensor x' y'
    lett e1 e2 = R $ do
                   x <- unR e1
                   unR $ e2 (R (return x))
    drop e1 e2 = R $ do
                   unR e1
                   unR e2
    letStar xy f = R $ do
                     (Tensor x y) <- unR xy
                     unR $ f (R $ return x, R $ return y)
    

eval :: R Z '[] '[] a -> IO a
eval = unR

test0 = prt "hi" ||| base 1
test1 = nu $ \(x,y) -> y
test2 = prt "hi"
test3 = (|||) (prt "1") (prt "2")
test4 = nu $ \(_,a) -> nu $ \(_,b) ->  prt "hi" ||| a ||| b
test5 = nu $ \(_,x) -> drop x stop ||| prt "hi"
test6 = nu $ \(w,r) -> drop (wr w (base 1)) (wr w (base 2)) ||| 
        letStar (rd r) (\(r, a) -> 
                            letStar (rd r) (\(r, b) -> drop r (a <**> b)))
        --                letStar (rd r) (\(r, b) -> drop r b))
-- good = defn $ llam $ \f -> llam $ \x -> f <^> x

main = do
--    putStrLn $ unLolli (eval $ good <^> llam (\x -> x)) "I was passed to a real function."
    putStrLn "ok"
