
-- 1.8 --------

{-
What does function composition do to a sequence?
For a sequence a what is a ◦ (1+)? What is (1+) ◦ a?
 - The sequence a could be thougt of as a function
 application a(i). So for a given index i we get the
 element at a(i). When a ◦ (1+) composed and run for
 som input it will first incriment and then perform a(i+1).
 While (1+) ◦ a will perform the function a(i) and then
 increment, giving us a(i)+1.

How is liftSeq1 related to fmap? liftSeq0 to conSeq?
- The liftSeq1 takes a function and applies it on to
 every element in a sequence, giving us a new sequence.
 So {a_1, a_2, ..., a_n} and f(a) gives us 
 {f(a_1), f(a_2), ..., f(a_n)}. Which is exactly wat fmap
 does, but to a list.

 liftSeq0 is a "nullary" operation taking a constant and an
 index i and returning a sequence of i c:s. This is exactly
 what conSeq does.
-}