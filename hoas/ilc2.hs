-- /show
{-# LANGUAGE 
  DataKinds,
  FlexibleContexts,
  FlexibleInstances,
  FunctionalDependencies,
  KindSignatures,
  MultiParamTypeClasses,
  NoMonomorphismRestriction,
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
instance {-# OVERLAPPING #-} EQ x x True
instance {-# OVERLAPPING #-} (b ~ False) => EQ x y b

--
-- Type level machinery for consuming a variable in a list of variables.
--
class Consume (v::Nat) (i::[Maybe (Nat,Typ)]) (o::[Maybe (Nat,Typ)]) | v i -> o
class Consume1 (b::Bool) (v::Nat) (x::Nat) (t::Typ) (i::[Maybe (Nat,Typ)]) (o::[Maybe (Nat,Typ)]) | b v x t i -> o
instance (Consume v i o) => Consume v (Nothing ': i) (Nothing ': o)
instance (EQ v x b, Consume1 b v x t i o) => Consume v (Just '(x, t) ': i) o
instance Consume1 True v x t i (Nothing ': i)
instance (Consume v i o) => Consume1 False v x t i (Just '(x, t) ': o)

--
-- Modes / effects
--
data Mode = Write | Readd | Value
class ParMode (ma::Mode) (mb::Mode) (mc::Mode) | ma mb -> mc
instance ParMode Write Value Write
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

-- Type grammar
data Typ where
    Base :: (a :: *) -> Typ
    (:*:) :: Typ -> Typ -> Typ
    (:+:) :: Typ -> Typ -> Typ
    (:-<>:) :: Typ -> Typ -> Typ
    One :: Typ
    List :: Typ -> Typ
    Bang :: Typ -> Typ
    Rd :: Typ -> Typ
    Wr :: Typ -> Typ

infixr 5 :-<>:


-- "Sendable" Relations 
class Send (a::Typ)
instance Send b => Send (a :-<>: b) -- Negative position is OK
instance (Send a, Send b) => Send (a :*: b)
instance Send (Base ())
instance Send (Base Int)
instance Send One
instance (Send a, Send b) => Send (a :+: b)
instance Send a => Send (Bang a)
instance Send a => Send (List a)

-- "Sendable" environments only contain sendable elements
class Sendable (i::[Maybe (Nat,Typ)]) (o::[Maybe (Nat,Typ)])
instance Sendable '[] '[]
instance Sendable i o => Sendable (a ': i) (a ': o)
instance (Sendable i o, Send a) => Sendable (Just '(n, a) ': i) (Nothing ': o)


{-- Terms and types --}
-- Abbreviations for variables

type LinVar repr (vid::Nat) (a::Typ) = forall (v::Nat) (i::[Maybe (Nat,Typ)]) (o::[Maybe (Nat,Typ)]). (Consume vid i o) => repr Value v i o a

type RegVar repr (a::Typ) = forall (v::Nat) (h::[Maybe (Nat,Typ)]) . repr Value v h h a

class Lin (repr :: Mode -> Nat -> [Maybe (Nat,Typ)] -> [Maybe (Nat,Typ)] -> Typ -> *) where
    llam :: (LinVar repr vid a -> repr m (S vid) (Just '(vid, a) ': hi) (Nothing ': ho) b)
         -> repr Value vid hi ho (a :-<>: b)
    base :: a -> repr Value vid h h (Base a)
    app :: SeqMode ma mb mc => repr ma vid hi h (Base a) -> (a -> repr mb vid h ho b) -> repr mc vid hi ho b
    (<^>) :: SeqMode ma mb mc => repr ma vid hi h (a :-<>: b) -> repr mb vid h ho a -> repr mc vid hi ho b
    prt :: String -> repr Value vid h h One
    stop :: repr Value vid h h One
    lett :: SeqMode ma mb mc => repr ma vid hi h a -> (LinVar repr vid a -> repr mb (S vid) (Just '(vid, a) ': h) (Nothing ': ho) b) -> repr mc vid hi ho b
    drop :: SeqMode ma mb mc => repr ma vid hi h a -> repr mb vid h ho b -> repr mc vid hi ho b
    one :: repr Value vid h h One

    -- Communication
    nu :: ((RegVar repr     (Wr a),    {-- write end --}
            LinVar repr vid (Rd a)) -> {--  read end --}
           repr m (S vid) (Just '(vid, a) ': hi) (Nothing ': ho) b) 
       -> repr m vid hi ho b
    (|>|) :: ParMode ma mb mc => repr ma vid hi h a -> repr mb vid h ho b -> repr mc vid hi ho b
    wrSend :: (SeqMode ma mb mc, SeqMode mc Write md, Sendable h ho, Send a) => repr ma vid hi h (Wr a) -> repr mb vid h ho a -> repr md vid hi ho One
    wr :: (SeqMode ma mb mc, SeqMode mc Write md) => repr ma vid hi h (Wr a) -> repr mb vid h ho a -> repr md vid hi ho One
    rd :: SeqMode ma Readd mc => repr ma vid hi ho (Rd a) -> repr mc vid hi ho (Rd a :*: a)

    -- Bang
    (!) :: repr m vid h h a -> repr m vid h h (Bang a)
    letBang :: (SeqMode ma mb mc, Send a) => repr ma vid hi h (Bang a) -> 
               (RegVar repr a -> repr mb vid h ho b) -> 
               repr mc vid hi ho b
    pick :: repr m vid hi ho (Bang a) -> repr m vid hi ho (a :*: Bang a)

    -- Plus
    inl :: repr m vid hi ho a -> repr m vid hi ho (a :+: b)
    inr :: repr m vid hi ho b -> repr m vid hi ho (a :+: b)
    letPlus :: SeqMode ma mb mc => repr ma vid hi h (a :+: b) ->
               (LinVar repr vid a -> repr mb (S vid) (Just '(vid,a) ': h) (Nothing ': ho) c) ->
               (LinVar repr vid b -> repr mb (S vid) (Just '(vid,b) ': h) (Nothing ': ho) c) ->
               repr mc vid hi ho c

    -- Pair
    (<**>) :: SeqMode ma mb mc => 
              repr ma vid hi h a -> repr mb vid h ho b ->
              repr mc vid hi ho (a :*: b)
    letStar :: SeqMode ma mb mc =>
               repr ma vid hi h (a :*: b) ->
               ((LinVar repr vid a,
                LinVar repr (S vid) b) -> 
                    repr mb (S (S vid)) 
                         (Just '(S vid, b) ': Just '(vid, a) ': h) 
                         (Nothing ': Nothing ': ho)
                         c
               ) -> 
               repr mc vid hi ho c

    -- Recursive lists
    wrapL :: repr m v hi ho (One :+: (a :*: List a)) -> repr m v hi ho (List a)
    unwrapL :: repr m v hi ho (List a) -> repr m v hi ho (One :+: (a :*: List a))


{-- Concrete Instantiation with GADTs --}
{--data Term (t::Typ) where
    Nu   :: (Term (Wr a) -> Term (Rd a) -> Term b) -> Term b
    Lam  :: (Term a -> Term b) -> Term (a :-<>: b)
    App  :: Term (a :-<>: b) -> Term a -> Term b
    Pair :: Term a -> Term b -> Term (a :*: b)
    Lett :: Term a -> (Term a -> Term b) -> Term b
    LetStar :: Term (a :*: b) -> (Term a -> Term b -> Term c) -> Term c
--}
data Term where
  Cha  :: Int -> Term
  Nu   :: (Term -> Term -> Term) -> Term
  Lam  :: (Term -> Term) -> Term
  App  :: Term -> Term -> Term
  Pair :: Term -> Term -> Term

isValue :: Term -> Bool
isValue (Nu _) = False

type Env = Data.Map Term
data Ring = Map 
--data Vv = R Term | W Term | Stack

{-- Denotation --}
--newtype R (m::Mode) (vid::Nat) (hi::[Maybe (Nat,Typ)]) (ho::[Maybe (Nat,Typ)]) a = R {unR :: IO (Rt a)}
newtype T (m::Mode) (vid::Nat) (i::[Maybe (Nat,Typ)]) (o::[Maybe (Nat,Typ)]) a = T {unTT :: Term }

data Result t = Wri Int Term | Term --(a, Wr a) | Tv t
--evalT :: Term t -> Result t

type family Tv (t::Typ) where
    Tv (a :-<>: b) = Tv a -> Tv b
    Tv (a :*: b) = (Tv a, Tv b)
    Tv (a :+: b) = Either (Tv a) (Tv b)
    Tv One = ()
    Tv (Base a) = a
    Tv (Bang a) = Tv a
    Tv (Wr a) = Int
    Tv (Rd a) = Int
    Tv (List a) = [Tv a]

--instance Lin T where

{-- Concrete Instantiation in the form of Channels and IO -}

newtype R (m::Mode) (vid::Nat) (hi::[Maybe (Nat,Typ)]) (ho::[Maybe (Nat,Typ)]) a = R {unR :: IO (Rt a)}
newtype ListR a = L { unL :: Either () (Rt a, ListR a) }
deriving instance Show (Rt a) => Show (ListR a)

type family Rt (a :: Typ) where
    Rt (a :-<>: b) = Rt a -> IO (Rt b)
    Rt (a :*: b) = (Rt a, Rt b)
    Rt (a :+: b) = Either (Rt a) (Rt b)
    Rt One = ()
    Rt (Base a) = a
    Rt (Bang a) = Rt a
    Rt (Wr a) = Chan (Rt a)
    Rt (Rd a) = Chan (Rt a)
    Rt (List a) = ListR a

instance Lin R where
    llam f = R $ return $ \x -> unR $ f $ R (return x)
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

    -- Bang
    (!) a = R $ unR a
    letBang a f = R $ do
                    a' <- unR a
                    unR $ f $ R $ return $ a'
    pick a = R $ do { a <- unR a; return $ (a,a) }

    -- Pair
    letStar xy f = R $ do
                     (x, y) <- unR xy
                     unR $ f (R $ return x, R $ return y)
    x <**> y = R $ do
                 x' <- unR x
                 y' <- unR y
                 return $ (x', y')

    -- Plus
    letPlus ab fa fb = R $ do
                      ab <- unR ab
                      case ab of 
                        Left  a -> unR $ fa $ R $ return a
                        Right b -> unR $ fb $ R $ return b
    inl a = R $ unR a >>= return . Left
    inr a = R $ unR a >>= return . Right

    -- Parallel composition
    p |>| q = R $ forkIO (unR p >> return ()) >> unR q

    -- Channels
    nu f = R $ do
      x <- newChan;
      unR $ f (R $ return x, R $ return x)
    wr c x = R $ do
               c' <- unR c
               unR x >>= writeChan c'
    wrSend c x = R $ do
                   c' <- unR c
                   unR x >>= writeChan c'
    rd c = R $ do
             c <- unR c
             x <- readChan c
             return (c, x)

    -- Lists
    wrapL   a = R $ unR a >>= return . L
    unwrapL a = R $ unR a >>= return . unL




-- Evaluation of closed terms
type Defn m a = forall repr v h . (Sendable h h, Lin repr) => repr m v h h a

defn :: Defn m a -> Defn m a
defn x = x

eval :: R Value Z '[] '[] a -> IO (Rt a)
eval = unR 

-- Examples with Lists
type Map k a = List (k :*: a)

mapIns :: Defn Value (Map k a :-<>: k :*: a :-<>: Map k a)
mapIns = llam $ \lst -> llam $ \ka -> wrapL (inr (ka <**> lst))

mapLookup :: Eq k => Defn Value (Map (Base k) a :-<>: Base k :-<>: 
                           Map (Base k) a :*: (One :+: a))
mapLookup = llam $ \lst -> llam $ \k -> app k $ \k ->
            letPlus (unwrapL lst)
            (\nil -> (wrapL $ inl nil) <**> (inl one))
            (\hlst -> letStar hlst $ 
                      \(ka,lst) -> letStar ka $ \(k',a) -> app k' $ \k' ->
                                   if k' == k then lst <**> inr a
                                   else letStar (mapLookup <^> lst <^> base k) $ \(lst',res) -> (wrapL $ inr ((base k' <**> a) <**> lst')) <**> res)

mapNil = wrapL (inl one)
tl0 :: Defn Value (Map (Base Int) (Base String))
tl0 = defn $ mapIns <^> mapNil <^> (base 1 <**> base "1")

tl1 :: Defn Value (Map (Base Int) (Base String))
tl1 = defn $ mapIns <^> tl0 <^> (base 2 <**> base "2")



-- Test cases

test0 = prt "hi" |>| base 1
test1 = nu $ \(x,y) -> y
test2 = prt "hi"
test3 = lett (prt "1") $ \x -> drop x (prt "2")
test4 = nu $ \(_,a) -> nu $ \(_,b) ->  prt "hi" |>| a |>| b
test5 = nu $ \(_,x) -> drop x stop |>| prt "hi"
test6 = nu $ \(w,r) -> drop (wr w (base 1)) (wr w (base 2)) |>| 
        letStar (rd r) (\(r, a) -> 
                            letStar (rd r) (\(r, b) -> drop r (a <**> b)))
test7 = nu $ \(ww,rr) -> drop rr $
        nu $ \(w,r) -> drop (wrSend w $ base ww) (wrSend w $ base ww) |>| 
        letStar (rd r) (\(r, a) -> 
                            letStar (rd r) (\(r, b) -> drop r (a <**> b)))
test8 = app (base 1) (\x -> base x)

readForever'' :: Lin repr => Int -> Int -> repr Value vid hi h (Rd a) -> repr Readd vid hi h b
readForever'' m n c = letStar (rd c) $ \(c, x) -> drop x $ drop (prt ("[" ++ show m ++ "]:" ++ show n)) $ readForever'' m (n+1) c

test22 :: Send a => Defn Value (Wr a :*: a :-<>: One)
test22 = llam $ \wx -> letStar wx $ \(w, x) -> wrSend w x

test11 :: Defn Value (Wr (Base Int) :-<>: (Base Int :-<>: One))
test11 = defn $ llam $ \c -> llam $ \x -> 
         wrSend c x

main = do
    putStrLn "ok"
