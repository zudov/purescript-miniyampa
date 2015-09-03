# purescript-miniyampa

Toy implementation of [AFRP](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf)
with [Yampa](http://hackage.haskell.org/package/Yampa-0.10.2)-like API.

The idea is to have a simple (though inefficient) implementation that follows
the original FRP denotation.

It is primarily useful for learning FRP, and (perhaps) for building small applications.

It's not designed for practical usage, since there is no optimizations and lack
of arrow notation makes it very unwieldy to work with arrows.

Much of the credit goes to [@cobpg](https://github.com/cobbpg) as
purescript-miniyampa is based on his [gist](https://gist.github.com/cobbpg/34b16cf9c1f076be7d3f).
