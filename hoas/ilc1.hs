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
  StandaloneDeriving
 #-}
--
-- Type level Nats
--
import Control.Concurrent
import Control.Concurrent.Chan
import Prelude hiding (drop)

data Nat = Z | S Nat

--data Mayb = Noth | Jus Nat Typ

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

type LinVar repr (vid::Nat) a = forall (v::Nat) (i::[Maybe (Nat,*)]) (o::[Maybe (Nat,*)]). (Consume vid i o) => repr v i o a

type RegVar repr a = forall (v::Nat) (h::[Maybe (Nat,*)]) . repr v h h a

-- to distinguish linear arrow from regular arrow
newtype a :-<>: b = Lolli {unLolli :: a -> IO b}
infixr 5 :-<>:
data a :*: b = Tensor a b deriving Show
data One = One deriving Show
data a :+: b = Inl a | Inr b deriving Show
newtype Bang a = Bang {unBang :: a}

-- Recursive types
newtype Mu a = Mu {unMu :: a (Mu a)}
deriving instance Show (a (Mu a)) => Show (Mu a)

class LinRec (repr :: Nat -> [Maybe (Nat,*)] -> [Maybe (Nat,*)] -> * -> *) where
    wrap :: (b -> a (Mu a)) -> repr vid i o b -> repr vid i o (Mu a)

    unwrap :: (a (Mu a) -> b) -> repr vid i o (Mu a) -> repr vid i o b

-- Channel types
newtype Rd a = Rd {unRd :: Chan a }
newtype Wr a = Wr {unWr :: Chan a }

-- Catch-all metalanguage type 
newtype Base a = Base { unBase :: a } deriving (Show, Eq)

-- "Sendable" Relations 
class Send a
instance Send b => Send (a :-<>: b) -- Negative position is OK
instance (Send a, Send b) => Send (a :*: b)
instance Send (Base ())
instance Send (Base Int)
instance Send One
instance (Send a, Send b) => Send (a :+: b)
instance Send a => Send (Bang a)

-- "Sendable" environments only contain sendable elements

class Sendable (i::[Maybe (Nat,*)]) (o::[Maybe (Nat,*)])
instance Sendable '[] '[]
instance Sendable i o => Sendable (a ': i) (a ': o)
instance (Sendable i o, Send a) => Sendable (Just '(n, a) ': i) (Nothing ': o)

class Lin (repr :: Nat -> [Maybe (Nat,*)] -> [Maybe (Nat,*)] -> * -> *) where
    llam :: (LinVar repr vid a -> repr (S vid) (Just '(vid, a) ': hi) (Nothing ': ho) b) -> repr vid hi ho (a :-<>: b)
    nu :: ((RegVar repr     (Wr a), {-- write end --}
            LinVar repr vid (Rd a)) -> {--  read end --}
           repr (S vid) (Just '(vid, a) ': hi) (Nothing ': ho) b) -> repr vid hi ho b
    (|||) :: repr vid hi h a -> repr vid h ho b -> repr vid hi ho b
    base :: a -> repr vid h h (Base a)
    app :: repr vid hi h (Base a) -> (a -> repr vid h ho b) -> repr vid hi ho b
    (<^>) :: repr vid hi h (a :-<>: b) -> repr vid h ho a -> repr vid hi ho b
    prt :: String -> repr vid h h ()
    stop :: repr vid h h ()
    wrClosed :: (Sendable h ho, Send a) => repr vid hi h (Wr a) -> repr vid h ho a -> repr vid hi ho ()
    wr :: repr vid hi h (Wr a) -> repr vid h ho a -> repr vid hi ho ()
    rd :: repr vid hi ho (Rd a) -> repr vid hi ho (Rd a :*: a)
    lett :: repr vid hi h a -> (LinVar repr vid a -> repr vid h ho b) -> repr vid hi ho b
    drop :: repr vid hi h a -> repr vid h ho b -> repr vid hi ho b
    one :: repr vid h h One

    -- Bang
    (!) :: repr vid h h a -> repr vid h h (Bang a)
    letBang :: Send a => repr vid hi h (Bang a) -> 
               (RegVar repr a -> repr vid h ho b) -> 
               repr vid hi ho b
    pick :: repr vid hi ho (Bang a) -> repr vid hi ho (a :*: Bang a)

    -- Plus
    inl :: repr vid hi ho a -> repr vid hi ho (a :+: b)
    inr :: repr vid hi ho b -> repr vid hi ho (a :+: b)
    letPlus :: repr vid hi h (a :+: b) ->
               (LinVar repr vid a -> repr (S vid) (Just '(vid,a) ': h) (Nothing ': ho) c) ->
               (LinVar repr vid b -> repr (S vid) (Just '(vid,b) ': h) (Nothing ': ho) c) ->
               repr vid hi ho c

    -- Pair
    (<**>) :: repr vid hi h a -> repr vid h ho b ->
             repr vid hi ho (a :*: b)
    letStar :: repr vid hi h (a :*: b) ->
               ((LinVar repr vid a,
                LinVar repr (S vid) b) -> 
                    repr (S (S vid)) 
                         (Just '(S vid, b) ': Just '(vid, a) ': h) 
                         (Nothing ': Nothing ': ho)
                         c
               ) -> 
               repr vid hi ho c

type Defn a = forall repr v h . (Sendable h h, Lin repr, LinRec repr) => repr v h h a

defn :: Defn a -> Defn a
defn x = x
-- show
newtype R (vid::Nat) (hi::[Maybe (Nat,*)]) (ho::[Maybe (Nat,*)]) a = R {unR :: IO a}

instance Lin R where
    llam f = R $ return $ Lolli $ \x -> unR $ f $ R (return x)
    f <^> x = R $ do 
                 f' <- unR f
                 x' <- unR x
                 unLolli f' x'

    -- Parallel composition
    p ||| q = R $ forkIO (unR p >> return ()) >> unR q

    -- Channels
    nu f = R $ do
      x <- newChan;
      unR $ f (R $ return $ Wr x, R $ return $ Rd x)
    wr c x = R $ do
               c' <- unR c
               unR x >>= writeChan (unWr c')
    wrClosed c x = R $ do
                     c' <- unR c
                     unR x >>= writeChan (unWr c')
    rd c = R $ do
             c <- unR c
             x <- readChan (unRd c)
             return $ Tensor c x

    -- Catch all
    base x  = R $ return $ Base x
    app x f = R $ unR x >>= unR . f . unBase
    prt x = R $ putStrLn x
    stop = R $ return ()
    lett e1 e2 = R $ do
                   x <- unR e1
                   unR $ e2 $ R $ return x
    drop e1 e2 = R $ unR e1 >> unR e2

    -- Pair
    letStar xy f = R $ do
                     (Tensor x y) <- unR xy
                     unR $ f (R $ return x, R $ return y)
    x <**> y = R $ do
                 x' <- unR x
                 y' <- unR y
                 return $ Tensor x' y'

    -- Plus
    letPlus ab fa fb = R $ do
                      ab <- unR ab
                      case ab of 
                        Inl a -> unR $ fa $ R $ return a
                        Inr b -> unR $ fb $ R $ return b
    inl a = R $ unR a >>= return . Inl
    inr a = R $ unR a >>= return . Inr

    -- Bang
    (!) a = R $ unR a >>= return . Bang
    letBang a f = R $ do
                    a' <- unR a
                    unR $ f $ R $ return $ unBang a'
    pick a = R $ do { a <- unR a; return $ Tensor (unBang a) a}

    one = R $ return One


instance LinRec (R :: Nat -> [Maybe (Nat,*)] -> [Maybe (Nat,*)] -> * -> *) where
    wrap f a = R $ unR a >>= return . Mu . f
    unwrap f a = R $ unR a >>= return . f . unMu




eval :: R Z '[] '[] a -> IO a
eval = unR

-- List, as an example of recursive type
newtype MyListF a lst = MLF {unMLF :: One :+: (a :*: lst)} deriving Show
type MyList a = Mu (MyListF a)
instance Send a => Send (MyList a)

type Map k a = MyList (k :*: a)

mapIns :: Defn (Map k a :-<>: k :*: a :-<>: Map k a)
mapIns = llam $ \lst -> llam $ \ka -> wrap MLF (inr (ka <**> lst))

mapLookup :: Eq k => Defn (Map (Base k) a :-<>: Base k :-<>: 
                           Map (Base k) a :*: (One :+: a))
mapLookup = llam $ \lst -> llam $ \k -> app k $ \k ->
            letPlus (unwrap unMLF lst)
            (\nil -> (wrap MLF $ inl nil) <**> (inl one))
            (\hlst -> letStar hlst $ 
                      \(ka,lst) -> letStar ka $ \(k',a) -> app k' $ \k' ->
                                   if k' == k then lst <**> inr a
                                   else letStar (mapLookup <^> lst <^> base k) $ \(lst',res) -> (wrap MLF $ inr ((base k' <**> a) <**> lst')) <**> res)

mapNil = wrap MLF (inl one)
tl0 :: Defn (Map (Base Int) (Base String))
tl0 = defn $ mapIns <^> mapNil <^> (base 1 <**> base "1")

tl1 :: Defn (Map (Base Int) (Base String))
tl1 = defn $ mapIns <^> tl0 <^> (base 2 <**> base "2")


test0 = prt "hi" ||| base 1
test1 = nu $ \(x,y) -> y
test2 = prt "hi"
test3 = (|||) (prt "1") (prt "2")
test4 = nu $ \(_,a) -> nu $ \(_,b) ->  prt "hi" ||| a ||| b
test5 = nu $ \(_,x) -> drop x stop ||| prt "hi"
test6 = nu $ \(w,r) -> drop (wr w (base 1)) (wr w (base 2)) ||| 
        letStar (rd r) (\(r, a) -> 
                            letStar (rd r) (\(r, b) -> drop r (a <**> b)))
test7 = nu $ \(ww,rr) -> drop rr $
        nu $ \(w,r) -> drop (wrClosed w $ base ww) (wrClosed w $ base ww) ||| 
        letStar (rd r) (\(r, a) -> 
                            letStar (rd r) (\(r, b) -> drop r (a <**> b)))
test8 = app (base 1) (\x -> base x)


readForever'' :: Lin repr => Int -> Int -> repr vid hi h (Rd a) -> repr vid hi h b
readForever'' m n c = letStar (rd c) $ \(c, x) -> drop x $ drop (prt ("[" ++ show m ++ "]:" ++ show n)) $ readForever'' m (n+1) c

test10 = nu $ \(w,r) -> drop (wrClosed w (base ())) (wrClosed w (base ())) ||| readForever'' 0 0 r

multiplex :: Lin repr => repr vid hi h (Rd (Base Int)) -> repr vid hi h (Base Int)
multiplex c = letStar (rd c) $ \(c, x) -> drop c $ app x (\x -> base x)


test22 :: Send a => Defn (Wr a :*: a :-<>: ())
test22 = llam $ \wx -> letStar wx $ \(w, x) -> wrClosed w x

test11 :: Defn (Wr (Base Int) :-<>: (Base Int :-<>: ()))
test11 = defn $ llam $ \c -> llam $ \x -> 
         wrClosed c x



main = do
--    putStrLn $ unLolli (eval $ good <^> llam (\x -> x)) "I was passed to a real function."
    putStrLn "ok"
