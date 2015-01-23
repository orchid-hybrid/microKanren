~µKanren
==========

Trying to add the =/= constraint to µKanren based on William Byrd's lecture: [Uncourse #8](https://www.youtube.com/watch?v=mMQ6On3vdvA)

First attempt seems to be working ok but isn't efficient and the code looks ugly (doesn't fit with the very elegant minimal muKanren style), hope to make it cleaner.

**Bugs**

Noticed a bug in this when i tried rembero and it was failing when it shouldn't have. I cut the test case down to this:

```
> (run* (q) (fresh (x xs) (== '(a b) `(,x . ,xs)) (=/= 'c x)))
((_.0 (and (or))))
> (run* (q) (fresh (x xs) (== '(a b) `(,x . ,xs)) (=/= 'c x) (== q `(,x ,xs))))
()
```

and I think the bug came from an edge case in de-morgans law applied to an empty or like `(and (or))`. So the fix was to filter out empties.



µKanren
==========

Copyright (C) 2013 Jason Hemann and Daniel P. Friedman

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



