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
  ImpredicativeTypes,
  StandaloneDeriving,
  GADTs
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
class Consume (v::Nat) (i::[Maybe (Nat,*)]) (o::[Maybe (Nat,*)]) | v i -> o
class Consume1 (b::Bool) (v::Nat) (x::Nat) (t:: *) (i::[Maybe (Nat,*)]) (o::[Maybe (Nat,*)]) | b v x t i -> o
instance (Consume v i o) => Consume v (Nothing ': i) (Nothing ': o)
instance (EQ v x b, Consume1 b v x t i o) => Consume v (Just '(x, t) ': i) o
instance Consume1 True v x t i (Nothing ': i)
instance (Consume v i o) => Consume1 False v x t i (Just '(x, t) ': o)

--
-- Modes / effects
--
data Mode = Write | Readd | Value
class ParMode (ma::Mode) (mb::Mode) (mc::Mode) | ma mb -> mc
instance ParMode Write Value Write -- Excluded
instance ParMode Write Readd Write
instance ParMode Value Write Write
instance ParMode Readd Write Write
instance ParMode Readd Readd Readd
instance ParMode Readd Value Value
instance ParMode Value Value Value
instance ParMode Value Readd Readd
class SeqMode (ma::Mode) (mb::Mode) (mc::Mode) | ma mb -> mc
instance SeqMode Write Value Write
instance SeqMode Write Readd Write
instance SeqMode Readd m Readd
instance SeqMode Value m m


-- Abbreviations for variables

type LinVar repr (vid::Nat) a = forall (v::Nat) (i::[Maybe (Nat,*)]) (o::[Maybe (Nat,*)]). (Consume vid i o) => repr Value v i o a

type RegVar repr a = forall (v::Nat) (h::[Maybe (Nat,*)]) . repr Value v h h a

-- to distinguish linear arrow from regular arrow
data a :-<>: b
infixr 5 :-<>:

-- Catch-all metalanguage type 
data Base a


class Lin (repr :: Mode -> Nat -> [Maybe (Nat,*)] -> [Maybe (Nat,*)] -> * -> *) where
    llam :: (LinVar repr vid a -> repr m (S vid) (Just '(vid, a) ': hi) (Nothing ': ho) b)
         -> repr Value vid hi ho (a :-<>: b)
    base :: a -> repr Value vid h h (Base a)
    app :: SeqMode ma mb mc => repr ma vid hi h (Base a) -> (a -> repr mb vid h ho b) -> repr mc vid hi ho b
    (<^>) :: SeqMode ma mb mc => repr ma vid hi h (a :-<>: b) -> repr mb vid h ho a -> repr mc vid hi ho b
    prt :: String -> repr Value vid h h ()
    stop :: repr Value vid h h ()
    lett :: SeqMode ma mb mc => repr ma vid hi h a -> (LinVar repr vid a -> repr mb (S vid) (Just '(vid, a) ': h) (Nothing ': ho) b) -> repr mc vid hi ho b
    drop :: SeqMode ma mb mc => repr ma vid hi h a -> repr mb vid h ho b -> repr mc vid hi ho b
    one :: repr Value vid h h ()


--newtype R (m::Mode) (vid::Nat) (hi::[Maybe (Nat,*)]) (ho::[Maybe (Nat,*)]) a = R {unR :: IO a}

newtype R (m::Mode) (vid::Nat) (hi::[Maybe (Nat,*)]) (ho::[Maybe (Nat,*)]) a = R {unR :: IO (Rt a)}
type family Rt a where
    Rt (a :-<>: b) = Rt a -> IO (Rt b)
    Rt () = ()
    Rt (Base a) = a

instance Lin R where
    llam f = R $ return $ {--Lolli $ --} \x -> unR $ f $ R (return x)
    f <^> x = R $ do 
                 f' <- unR f
                 x' <- unR x
                 f' x'

    -- Catch all
    base x  = R $ return x
    app x f = R $ unR x >>= unR . f
    prt x = R $ putStrLn x
    stop = R $ return ()
    lett e1 e2 = R $ do
                   x <- unR e1
                   unR $ e2 $ R $ return x
    drop e1 e2 = R $ unR e1 >> unR e2
    one = R $ return ()

-- Evaluation of closed terms
type Defn (m::Mode) a = forall repr v h . (Lin repr) => repr m v h h a

defn :: Defn m a -> Defn m a
defn x = x

eval :: R Value Z '[] '[] a -> IO (Rt a)
eval = unR


test2 = prt "hi"
test3 = lett (prt "1") $ \x -> drop x (prt "2")
test8 = app (base 1) (\x -> base x)

main = do
    putStrLn "ok"
