# Mu semantics in Haskell

## Lambda-mu

The lambda-mu calculus is an extension to the lambda calculus first described by Parigot in [1992][0]. It was designed to model classical logic proofs, based on the correspondence between lambda calculus terms and intuitionistic proofs.

Lambda-mu introduces two new syntax elements:

  * names - a, b, c, etc. These are distinct from variables
  * the **mu abstraction** - `mu a. [b] M`, where `a`, `b` are names, and `M` is an inner term

The mu abstraction is similar to the lambda abstraction, in that it can have arguments applied to it, and reduction rules exist to apply the abstraction to those arguments. For mu, the reduction rule is
```
(mu a. [b] M) N ~> (mu a. [b] M { N / a })
```
where `{ N / a }` is not replacement, but instead *appending*, the term `N` to positions with name `a`. For example,
```
(mu c. [a] P) { Q / a } === (mu c. [a] P Q)
```

Unlike the lambda abstraction, mu does not disappear on application. Instead, it can be passed any number of arguments. For typed terms, a mu abstraction can only be passed as many arguments as the type allows.

## The two approaches

This library describes two ways to embed "mu semantics" in the Haskell. These approaches are meant to be more the spirit than the letter of the mu abstraction, and they explore some ideas in the type system.

### Functors and monads

Since mu semantics are ultimately about a kind of computation, a natural approach is to consider them in terms of functors and monads. In this world, mu is treated as an "unlimited continutation":
```haskell
data Mu k = Mu (forall f r. (k -> f r) -> f (Mu r)
```
(see `Mu.Functor` for the implementation details). This is similar in principle to the side-effecting continuation of the form `(k -> f r) -> f r`, but the key difference is the replacement of the final term with another continuation - thus, giving an "unlimited" continuation, which has no concept of a final value.

This definition is in fact a `Monad`. It is essentially a kind of "superfree monad" - a structure similar to a free monad, but which is agnostic of the type of functor that appears at any layer of the recursive structure. 

However, this approach isn't that interesting technically, and it doesn't concern argument-passing, which is the main way that the mu abstraction actually operates.

### Apptors and embeddings

#### Sidenote: on computers

Haskell is full of many things which are "arrow-ish" - that is, they can sensibly be treated as arrow types, but they are not actually arrow types. A good example is `Identity (a -> b)` - this is actually isomorphic to an arrow type, but you cannot use it as an arrow - that is, you cannot directly pass arguments to it.

Another example is `Maybe (a -> b)` - this is of course not isomorphic to an arrow type, but it does still have a sensible idea of argument passing. It's possible to pass a `Maybe (a -> b)` an argument of `a`, and get back a `b`-ish value - namely, `Maybe b`. (We could also consider `a`-ish values like `Maybe a`, but we won't.)

Because we are interested in argument passing, it's helpful to have an abstraction over these different kinds of "arrow-ish" vales, including arrows themselves. This is the `Computer` class, which describes argument-passing in terms of a domain, codomain, and an argument-passing operation called `run`. You can read more about it in `Data.Computer`.

#### Apptors

A functor lifts arrows - but when considering argument passing, there is a naturally weaker operation we can consider - just lifting arguments. The Haskell translation is something like
```haskell
fpass :: f (a -> b) -> a -> f b
```
This is similar to `fmap` - but obviously weaker. The resulting class, which is not unlike a `Functor` or an `Applicative`, is called an `Apptor`.

More descriptively, in Hask, `Apptor` is a category endomorphism `f : Hask -> Hask` with a right action `fpass : Hask -> Hask -> Hask` such that, for every arrow `x` and object `y`, `fpass (f(x), y) == f(x y)`.

#### Embeddings

##### Preimages in a monad

In general, monads in Haskell can be said to have a codomain which is *more powerful* than the domain, in terms of arrows - that is, the image of any two objects under the monad tend to have more arrows (in the codomain) between them than just images of arrows (in the domain) between those two objects.

To understand this more concretely, take `Maybe` as an example again. For any arrow `f :: a -> b`, then there is a lifted arrow `fmap f :: Maybe a -> Maybe b`. However, for most arrows `g :: Maybe a -> Maybe b`, there exists no corresponding `h :: a -> b` satisfying `fmap h == g` (for example, what happens if `g (Just x) == Nothing` for some `x`?).

If we investigate inverting `fmap` in this way - that is, finding a preimage under `fmap` for any arrows in the codomain - we will end up actually just looking for isomorphisms. The more interesting question is - can we find *lifted* preimages for `fmap` i.e. for every `g` as above, can we find `pre :: (Maybe a -> Maybe b) -> Maybe (a -> b)`, such that, if `g == fmap f`, then `pre g == pure f`?

Conceptually, for `Maybe`, we could - with a bounded, enumerable domain, we could check the image of `Nothing` and every `Just x`: if the result looks like a lifted arrow, we could return `Just f`, and `Nothing` otherwise. Of course, domains are not generally bounded or enumerable. So `Maybe` doesn't actually have such an implementaton in Haskell.

Some other monads do. `Const ()` is one example - even though every arrow in the codomain is trivial, we can find a lifted preimage for `fmap` by just giving the same trivial arrow. For every `f :: a -> b`, `pure f` is the trivial arrow, and so is `fmap f` - so `pre` maps every lifted arrow (which is just trivial) to the trivial arrow. This satisfies `pre (fmap f) == pure f`.

##### Embeddings

The existence of this `pre` function is limited to monads which have a *less powerful* codomain, compared to the domain. In fact, the only kinds of arrows in the codomain between the images of objects are those which are images of arrows - so the monad, restricted to its image in objects, is surjective for arrows.

These monads are of interest to `Apptor`s, because they represent transformations where argument passing is still the only kind of computation. The class, which is not actually a monad subclass (because it doesn't require `Applicative`'s `liftA2`) is called an `Embedding`, for lack of a better name. More detail, and some instances, can be found in `Data.Embedding`.

`Embedding`s and `Monad`s are very similar - but `Embedding` is more specific, because every `Applicative` `Embedding` is in fact a `Monad`, but not the other way around.

#### Mu as an embedding

It is possible to represent mu as an apptor, in such a way that it actually turns out to be an embedding. The essential idea is to have a term that can do argument-passing only when its contents is a function:
```haskell
data Mu k = Mu (forall f x y. k ~ (x -> y) => f x -> f (Mu y))
```
(The actual definition is more complex, due to the use of `Computer` - see `Mu.Apptor`).

This version really does unlimited argument passing, as opposed to the more general continuation form. Additionally, it is an embedding, and *not* a monad.

## The ignorant term

Names in the mu abstraction are not required to be names which actually appear in the inner term, much like lambda variables do not have to be variables that are actually in the lambda body. For example
```
lambda x. y
```
discards the argument `x`. Similarly,
```
mu a. [b] x
```
will discard its first argument, because there is nowhere to append it to (no term labelled `[a]`). However, unlike the `lambda` version, the `mu` version can discard *any number* of arguments - and it can be given any arrow type, while the lambda is limited in type to the number of its arguments.

This "ignorant" ability is a key feature of mu semantics - so it is also encoded in this library. The `Ignorant` class in `Data.Ignorant` describes embeddings which have such an `ignorant` term, satisfying `fpass ignorant a == ignorant`.

Both the monad and embedding versions of mu have such ignorant terms, called `empty`. However, the monad version does not implement the class instance, because it is not an `Embedding`.

 [0]: https://link.springer.com/chapter/10.1007%2FBFb0013061

