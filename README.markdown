Rose (n-ary) trees with both upwards- (*i.e.* cached) and
downwards-traveling (*i.e.* accumulating) monoidal annotations.  This
is used as the core data structure underlying the
[diagrams framework](http://projects.haskell.org/diagrams), but
potentially has other applications as well.

Abstractly, a `DUALTree` is a rose (n-ary) tree with data of one type
at leaves, data of another type at internal nodes, and two types of
monoidal annotations, one travelling "up" the tree and
one traveling "down".

For a bit of background and motivation, see the paper

[Brent Yorgey. *Monoids: Theme and Variations*.](http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf) Haskell Symposium, 2012.

To install,

```bash
    npm install
    bower install
    npm run build
```

## Changelog

  * v0.0.2 Updated bower metadata and made component public.
  * v0.0.1 Initial release for use in higher-level libraries.  Expect significant change.