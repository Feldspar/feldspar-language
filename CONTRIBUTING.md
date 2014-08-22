Contributing to Feldspar
---------------------

Start by reading [tibbe's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
Current Feldspar follows most of the conventions except for the section on
data types.

Use your judgement. Having a line that is 83 characters long might be a better
choice compared to the alternative even if it violates the style guide.

1. No commits to master in your fork.

1. Follow whatever code style the file uses. One consistent but bad
   style is preferable over having eight different good styles in the
   same file.

1. No trailing whitespace.

1. Run `cabal test` before committing.

1. Use one commit per logical change.

1. Keep the pile of small style fixes and similar in one or several 
   separate commits.

1. Make a sensible commit message.


