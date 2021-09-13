{-
HW 2 - Fold, Datatypes and Trees
================================

*Note: this homework is significantly longer than HW #1 and covers material
from [HigherOrder](/lectures/stub/01-intro/HigherOrder.html) and
[Datatypes](/lectures/stub/02-trees/Datatypes.html).*

The goal of this homework assignment is practice with fold, user-defined
datatypes and trees in Haskell.

This homework is composed of three files: two support files
[`XMLTypes.hs`](XMLTypes.hs) and [`Play.hs`](Play.hs), plus the main part of
the assignment (this file). For testing, you will also need the file
[`sample.html`](sample.html). To complete the homework, you should edit
*only* the file [`Main.hs`](Main.hs), and **submit only this file**.

You can access all of these files through github.
-}

module Main where

-- unit test support

-- support file for XML problem (provided)
import Play -- support file for XML problem (provided)
import Test.HUnit
import XMLTypes
import Prelude hiding (all, concat, takeWhile)

{-
For the XML transformation problem at the end of this assignment, you are
allowed to import modules from the Haskell [`base`
library](https://hackage.haskell.org/package/base-4.13.0.0).  The rest of the
homework should be done without the use of the list library functions, unless we
tell you otherwise.
-}

doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [testHO, testFoldr, testTree, testXML]
  return ()

main :: IO ()
main = doTests

{-
Problem - higher-order list operations
--------------------------------------

Complete these operations which take higher-order functions as arguments.  (For
extra practice, you may try to define these operations using `foldr`, but that
is not required for this problem.) Otherwise, you may not use any list library
functions for this problem.
-}

testHO :: Test
testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: (assertFailure "testcase for takeWhile" :: Assertion)

{-
>

-}

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3
find :: (a -> Bool) -> [a] -> Maybe a
find = undefined

tfind :: Test
tfind = "find" ~: (assertFailure "testcase for find" :: Assertion)

{-
>

-}

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False
all :: (a -> Bool) -> [a] -> Bool
all = undefined

tall :: Test
tall = "all" ~: (assertFailure "testcase for all" :: Assertion)

{-
>

-}

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = undefined

tmap2 :: Test
tmap2 = "map2" ~: (assertFailure "testcase for map2" :: Assertion)

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: (assertFailure "testcase for mapMaybe" :: Assertion)

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

----------------------------------------------------------------------

{-
Problem - map and foldr practice for lists
------------------------------------------

Go back to the following functions that you defined in HW #1 and redefine them
using one of the higher-order functions `map`, `foldr` or `para` (see
below). These are the *only* list library functions that you should use on this
problem. If you need any additional helper functions you must define them
yourself (and any helper functions should also use `map`, `foldr` or `para`
instead of explicit recursion).
-}

testFoldr :: Test
testFoldr = TestList [tconcat, tstartsWith, tendsWith, ttails, tcountSub]

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]

{-
**NOTE**: remember you cannot use any list functions from the `Prelude` or
`Data.List` for this problem, even for use as a helper function. Instead, define
it yourself.
-}

concat :: [[a]] -> [a]
concat = undefined

tconcat :: Test
tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

{-
**NOTE**: use `foldr` for this one, but it is tricky! (Hint: the value
*returned* by `foldr` can itself be a function.)
-}

startsWith :: String -> String -> Bool
startsWith = undefined

tstartsWith = "tstartsWith" ~: (assertFailure "testcase for startsWith" :: Assertion)

-- INTERLUDE: para

{-
Now consider a variant of `foldr` called `para`.  In the case of cons,
`foldr` provides access to the head of the list and the result of the fold
over the tail of the list. The `para` function should do the same, but should
*also* provide access to the tail of the list (before it has been processed).
-}

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x : xs) = f x xs (para f b xs)

{-
For example, consider the `tails` function.
-}

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
--
-- >>> tails "abc"
-- ["abc", "bc", "c", ""],
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

{-
It is a natural fit to implement `tails` using `para`.  See if you can
 redefine the function above so that the test cases still pass.
-}

tails' = undefined

ttails :: Test
ttails =
  "tails"
    ~: TestList
      [ "tails0" ~: tails' "abc" ~?= ["abc", "bc", "c", ""],
        "tails1" ~: tails' "" ~?= [""],
        "tails2" ~: tails' "a" ~?= ["a", ""]
      ]

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

{-
**NOTE**: use `para` for this one!
-}

endsWith :: String -> String -> Bool
endsWith = undefined

tendsWith :: Test
tendsWith = "endsWith" ~: (assertFailure "testcase for endsWith" :: Assertion)

{-
>

-}

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

{-
(You may use the `para` and `startsWith` functions in `countSub`.)
-}

countSub :: String -> String -> Int
countSub = undefined

tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)

----------------------------------------------------------------------

{-
(The following problems rely on material from `Datatypes`).

Problem - Tree processing
---------------------------
-}

testTree :: Test
testTree =
  TestList
    [ tappendTree,
      tinvertTree,
      ttakeWhileTree,
      tallTree,
      tmap2Tree
    ]

{-
This next problem involves writing some library functions for tree data
structures. The following datatype defines a binary tree, storing data at each
internal node.
-}

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

{-
This is the definition of a mappping operation for this data structure:
-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

{-
And here is a `fold`-like operations for trees:
-}

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

{-
Use one of these functions to define the following operations over trees.
-}

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
--
-- >>> appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty)
-- Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
--
-- and
--
-- >>> appendTree Empty (Branch 'a' Empty Empty)
-- Branch 'a' Empty Empty

appendTree :: Tree a -> Tree a -> Tree a
appendTree = undefined

tappendTree :: Test
tappendTree = "appendTree" ~: (assertFailure "testcase for appendTree" :: Assertion)

{-
>
-}

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--
-- >>> invertTree (Branch ("a",True) Empty Empty)
-- Branch (True,"a") Empty Empty

invertTree :: Tree (a, b) -> Tree (b, a)
invertTree = undefined

tinvertTree :: Test
tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)

{-
>

-}

-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

-- >>> takeWhileTree (< 3) tree1
-- Branch 1 (Branch 2 Empty Empty) Empty
--
-- >>> takeWhileTree (< 0) tree1
-- Empty

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree = undefined

ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: (assertFailure "testcase for takeWhileTree" :: Assertion)

{-
>

-}

-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
--
-- >>> allTree odd tree1
-- False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree = undefined

tallTree :: Test
tallTree = "allTree" ~: (assertFailure "testcase for allTree" :: Assertion)

{-
>

-}

-- WARNING: This one is a bit tricky!  (Hint: use `foldTree` and remember
--  that the value returned by `foldTree` can itself be a function. If you are
-- stuck on this problem, go back to `startsWith` and make sure you understand
-- how that function can work with a single fold.)

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
--
-- >>> map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
-- Branch 4 Empty Empty

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = undefined

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)

----------------------------------------------------------------------

{-
Problem - XML Transformation
--------------------------------

**WARNING**: this next problem requires some design as well as
implementation!

This problem involves transforming XML documents. To keep things
simple, we will not deal with the full generality of XML, or with
issues of parsing. Instead, we will represent XML documents as
values of the following simplified type:

    data SimpleXML
        = PCDATA  String
        | Element ElementName [SimpleXML]

    type ElementName = String

That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
data") node containing a `String`, corresponding to a leaf, or else an
`Element` node containing a tag and a list of sub-nodes, corresponding
to a branch with arbitrarily many children.

For example, this XML snippet

    <body>
      <p>Hello!</p>
      <p>Bye!</p>
    </body>

is represented by this Haskell value

    Element "body" [
        Element "p" [PCDATA "Hello!"],
        Element "p" [PCDATA "Bye!"  ] ]

The goal of this exercise is to write a transformation function
'formatPlay', which takes a play in an XML format *specific for plays*
and converts it to HTML (which is also an XML format).
-}

formatPlay :: SimpleXML -> SimpleXML
formatPlay = error "implement formatPlay"

{-
The input format is demonstrated by the sample file `Play.hs`.

The XML value in `Play.hs` has the following structure (as it would be
written in standard XML syntax):

     <PLAY>
       <TITLE>TITLE OF THE PLAY</TITLE>
       <PERSONAE>
         <PERSONA> PERSON1 </PERSONA>
         <PERSONA> PERSON2 </PERSONA>
         ... -- MORE PERSONAE
       </PERSONAE>
       <ACT>
         <TITLE>TITLE OF FIRST ACT</TITLE>
         <SCENE>
           <TITLE>TITLE OF FIRST SCENE</TITLE>
           <SPEECH>
             <SPEAKER> PERSON1 </SPEAKER>
             <LINE>LINE1</LINE>
             <LINE>LINE2</LINE>
             ... -- MORE LINES
           </SPEECH>
           ... -- MORE SPEECHES
         </SCENE>
         ... -- MORE SCENES
       </ACT>
       ... -- MORE ACTS
     </PLAY>

The output format is demonstrated by the file `sample.html`. This file
contains a very basic HTML rendition of the same information as
`Play.hs`. You may want to have a look at it in your favorite browser.
The HTML in `sample.html` has the following structure (with whitespace
added for readability). Note that the `<br/>` tags below should be
represented as `br` elements with no children.

      <html>
        <body>
          <h1>TITLE OF THE PLAY</h1>
          <h2>Dramatis Personae</h2>
          PERSON1<br/>
          PERSON2<br/>
          ...
          <h2>TITLE OF THE FIRST ACT</h2>
          <h3>TITLE OF THE FIRST SCENE</h3>
          <b>PERSON1</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...
          <b>PERSON2</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...

          <h3>TITLE OF THE SECOND SCENE</h3>
          <b>PERSON3</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...
        </body>
      </html>

Your version of `formatPlay` should add no whitespace except
what's in the textual data in the original XML.

The test below uses your function to generate a file `dream.html` from
the sample play. To receive any credit for this problem, the contents
of this file after your program runs must be *character for character*
identical to `sample.html`.

Your solution only needs to work for input in the same format as in `Play.hs`. You
do not need to worry about formatting errors for this assignment. (We will only
test your code on valid input.)
-}

-- | Find the first point where two lists differ and return
-- the remaining elements in the two lists.
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a], [a])
firstDiff [] [] = Nothing
firstDiff (c : cs) (d : ds)
  | c == d = firstDiff cs ds
  | otherwise = Just (c : cs, d : ds)
firstDiff cs ds = Just (cs, ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs, ds) -> assertFailure msg
      where
        msg = "Results differ: '" ++ take 20 cs ++ "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

{-
Important: The purpose of this assignment is not just to “get the job
done”, i.e. to produce the right HTML. A more important goal is to
think about what is a good way to do this job, and jobs like it.

To this end, your solution should be organized into two parts:

1. a collection of generic functions for transforming XML structures
that have nothing to do with plays, plus

2. a short piece of code (a single function definition or a collection
of short functions) that uses the generic functions to do the
particular job of transforming a play into HTML.

Obviously, there are many ways to do the first part. The main
challenge of the assignment is to find a clean design that matches the
needs of the second part.  You will be graded not only on correctness
(producing the required output), but also on the elegance of your
solution and the clarity and readability of your code and
documentation. As always, style most definitely counts.

It is strongly recommended that you rewrite this part of the
assignment a couple of times: get something working, then step back
and see if there is anything you can abstract out or generalize,
rewrite it, then leave it alone for a few hours or overnight and
rewrite it again. Try to use some of the higher-order programming
techniques we’ve been discussing in class.
-}

-----------------------------------------------------------------------------

{-
Describe how you and your partner worked together on this assignment. Who did
what? What parts did you complete separately and what parts did you complete
together? Were your contributions even?
-}

answer2 :: String
answer2 = undefined
