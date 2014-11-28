{-
Poor Man's Concurrency Monad - Haskell

In this lab we are going to implement a Monad for handling concurrency. You must use the provided template for this lab.

We are going to simulate concurrent processes by interleaving them. Interleaving implements concurrency by running the first part of one process, suspending it, and then allowing another process to run, et cetera, et cetera. 

A process is represented by the following recursive algebraic data type Action that encodes primitive actions that perform a side-effect and then return a continuation action, or the concurrent execution of two actions, or an action that has terminated.



To suspend a process, we need to grab its "future" and store it away for later use. Continuations are an excellent way of implementing this. We can change a function into continuation passing style by adding an extra parameter, the continuation, that represents the "future" work that needs to be done after this function terminates. Instead of producing its result directly, the function will now apply the continuation to the result. 

Given a computation of type Action, a function that uses a continuation with result type a has the following type:



This type can be read as a function that takes as input a continuation function (a -> Action), that specifies how to continue once the result of type a of the current computation is available. An application f c of this type will call c with its result when it becomes available. 

Unfortunately, because we want to make (a -> Action) -> Action into a monad, we first need to wrap it into a trivial algebraic data type, which we have to wrap and unwrap when implementing the monad operators. You might remember that in the exercises on the Parser monad we used a newtype declaration but since we are ignoring bottoms the difference is insignificant. 



As we will emphasize several times in the exercises below, we find it easiest to derive the implementations by ignoring the wrapper (think like a fundamentalist) since that makes "listening to the types" easier, and then add pattern matching and constructor calls to make GHC happy (code like a hacker). This may, or may not, hold for you.

EXERCISE 0

To express the connection between an expression of type Concurrent a and one of type Action, we define a function action :: Concurrent a -> Action that transforms a ((a -> Action) -> Action) into an Action that uses Stop :: Action to create the continuation to the Concurrent a passed as the first argument to action.

The easiest road to implement this function is to initially ignore the Concurrent wrapper, and first define a function action :: ((a -> Action) -> Action) -> Action and later add the pattern-matching to remove the wrapper and transform a value of type Concurrent a into a value of type ((a-> Action) -> Action) -> Action. As always, let the types guide you. There is only one obvious way to create a value of type a -> Action from the value Stop :: Action. Then when you get a value of type ma :: ((a-> Action) -> Action) there is only one way to combine these two to obtain a value of type Action.

Implement the function action:



EXERCISE 1

To make the constructors of the data type Action easily accessible, we can define helper functions that hide the boilerplate required to use them.

The first helper function that we will define is the function stop :: Concurrent a, which discards any continuation, thus ending a computation.

Thus we need to return a function of type ((Action -> ()) -> ()) wrapped in the Concurrent data type. This function takes a continuation, which gets discarded, and then it returns a Stop action.

Implement the helper function stop:



EXERCISE 2

Now we can define the helper function atom :: IO a -> Concurrent a, which turns an arbitrary computation in the IO Monad into an atomic action represented using the Atom constructor. 

The easiest way to implement this function is to first implement atom :: IO a -> ((a -> Action) -> Action) by taking an value x :: IO a and returning a value of type ((a -> Action) -> Action) which looks like \c :: (a -> Action) -> ... value of type Action ...

You already know, from the previous homework and labs, how to combine a value of type IO a and a function of type a -> IO b into a value of type IO b using (>>=), in this case b is instantiated to Action. You also know how to convert a value of type Action into a value of type IO Action using return. Finally, the obvious choice to turn a value of type IO Action into an Action is by using the Atom constructor. With all these pieces on the table, there is really only one way to wire them together to implement your function. Now the only step that is left is to wrap and unwrap the Concurrent data type, to implement atom :: IO a -> Concurrent a.

Implement the function atom:



EXERCISE 3

In order to access Fork, we need to define two operations. The first, called fork :: Concurrent a -> Concurrent (), forks its argument by turning it into an action and continues by passing () as the input to the continuation:



The second, par :: Concurrent a -> Concurrent a -> Concurrent a, combines two computations into one by forking them both and passing the given continuation to both parts.



Implement the functions fork and par.

EXERCISE 4

To make Concurrent an instance of the Monad type class, we need to provide implementations of (>>=) and return. Since the staff is nice, we will give you return for free. But you will have to define (>>=) yourself.



Don't panic! Let the types guide you and everything will be alright. There is really just one way to wire up all the pieces you have on the table to create a result of the required type. Don't try to understand what the code does operationally, trust the types.

The easiest way to do this is by first developing on a piece of scratch paper (*), a function (>>=) :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action) ignoring the Concurrent wrapper. Now you can let the types lead you to the only reasonable implementation. Once you've found this, you can add the boilerplate pattern matching and applications of Concurrent that Haskell unfortunately requires. Your implementation without Concurrent will look as follows:



Remember when you return a value of a function type, such as ((b -> Action) -> Action), the value you create looks like \c -> ... expression of type Action ...

Similarly, when you need to pass a value of a function type, for example a -> ((b -> Action) -> Action), the value you pass can be an expression of the form:

\a -> ... expression of type ((b -> Action) -> Action) ... 

In the end, the solution only needs two lambda expressions and a couple of function applications.

While doing all this, you don't need to look at the structure of Action at all. In fact, this would work for any type instead of Action.

(*) You can use GHC as your scratch paper by using a different name than (>>=), for example bind. This is what we mean by "Think like a Fundamentalist". Implementing (>>=) with all the nasty wrapping is "Code like a Hacker". Thinking like a fundamentalist is like Dolby noise reduction for the Hacker.

Implement (>>=) for the Concurrency monad. 

EXERCISE 5

At any moment, the status of the computation is going to be modelled as a list of "concurrently running" actions. We will use a scheduling technique called round robin to interleave the processes. The concept is easy: take the first process from the list, run its first part, then take the resulting continuation and put that at the back of the list. Keep doing this until the list is empty.

We implement this idea in the function roundRobin :: [Action] -> IO ().  An Atom monadically executes its argument and puts the resulting process at the back of the process list. Fork creates two new processes and Stop discards its process. Make sure you leverage the helper functions you defined before.

Implement the function roundRobin:



LET'S FINISH

To finish this lab, go to the question page and answer all 26 of them.
-}
module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
action (Concurrent (f)) = f (\x -> Stop) -- error "You have to implement action"
--action (Concurrent (\a -> Stop))
--action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))
--action (Concurrent (\a -> Atom $ putStr "Haskell"))
--action (Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop))

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent(\x -> Stop) --error "You have to implement stop"


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom m = Concurrent(\c -> Atom (do a <- m; return (c a)))
   --error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork m = Concurrent(\c -> Fork (action m) (c ())) --error "You have to implement fork"

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent f1) (Concurrent f2) = Concurrent(\c -> Fork (f1 c) (f2 c)) -- error "You have to implement par" --


-- ===================================
-- Ex. 4
-- ===================================
-- Concurrent h >>= k  =  Concurrent (\f -> h (\x -> runConcurrent (k x) f))
{-
http://stackoverflow.com/questions/27190934/implementation-for-poor-mans-concurrency-monad
The definition you are looking for reads something like

Concurrent h >>= k  =  Concurrent (\f -> h (\x -> runConcurrent (k x) f))
How did we get there? As always, we let the types do the work. :)

Let us first introduce a helper function:

runConcurrent                 :: Concurrent b -> (b -> Action) -> Action
runConcurrent (Concurrent h)  =  h
If you start out with the left-hand side of your definition

Concurrent h >>= k  =  ...
with h :: (a -> Action) -> Action and k :: a -> Concurrent b, then your goal is to replace ... with an expression of type Concurrent b, isn't it?

How can we construct a value of type Concurrent b? One way is to apply our function k, but that won't work, because we don't have a suitable value of type a available as an argument. So, pretty much the only thing we can do is to apply the data constructor Concurrent which is of type ((b -> Action) -> b) -> Concurrent b.

That gives:

Concurrent h >>= k = Concurrent ...
Now we have to go and find us an expression of type (b -> Action) -> b to supply as an argument for Concurrent. We know that expressions of function type can always be constructed through lambda-abstraction:

Concurrent h >>= k  =  Concurrent (\f -> ...)
This gives us f :: b -> Action and the obligation to replace ... with an expresion of type Action. Using one of the Action-constructors directly would be cheating of course ;). To guarantee the genericity of (>>=) (more precisely, to make sure that we end up obeying the monad laws), we treat Action as if it's an abstract datatype. Then, the only way to produce an Action-value is to apply the function h:

Concurrent h >>= k  =  Concurrent (\f -> h ...)
Hence, next we need to supply h with an argument of type a -> Action. That is a function type again, so we throw in another lambda:

Concurrent h >>= k  =  Concurrent (\f -> h (\x -> ...))
Hence, we have x :: a and need to construct a body of type Action. What can we do with a value of type a? We can supply it to the function k. This gives us a value of type Concurrent b, which we can then pass to our helper function runConcurrent:

Concurrent h >>= k  =  Concurrent (\f -> h (\x -> runConcurrent (k x) ...))
This gives us a function of type (b -> Action) -> Action and supplying f as an argument does the trick:

Concurrent h >>= k  =  Concurrent (\f -> h (\x -> runConcurrent (k x) f))
-}
runConcurrent                 :: Concurrent b -> (b -> Action) -> Action
runConcurrent (Concurrent h)  =  h

instance Monad Concurrent where
    (Concurrent ma) >>= f = Concurrent (\x -> ma(\y -> runConcurrent (f y) x)) 
    return x = Concurrent (\c -> c x)

-- action (stop >>= (\c -> stop))
-- action (fork stop >>= \_ -> fork stop)

-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a : as) = case a of 
     Atom am    -> do a' <- am; roundRobin (as ++ [a'])
     Fork a1 a2 -> roundRobin (as ++ [a1, a2])
     Stop       -> roundRobin as
-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

