# haskell oh boy

## Correctness

Data.Set provided by the "containers" package shows us this delightful warning 

```The size of the set must not exceed maxBound::Int. 
Violation of this condition is not detected and if the size limit is exceeded, 
its behaviour is undefined.
```

I beg your pardon...

# Usage 

So how do i interact with this cabal haskell thing anyways 

## building the project from source 

assuming everything is okay with universe and haskell and cabal are available from ghcup 

you can build the project using

```
> cabal build
```

## running tests

```
> cabal test
```
### what if multiple tests and only want to run some portion of tests ?

## running the program 

```
> cabal run
```

## what if i have multiple applications executables ?


## cabal config .cabal at toplevel of project

some configuration options in .cabal file are weird

module name in haskell WILL be randomly different Upper or Lower case

if want to use Data.Array module in haskell , I have to include array in .cabal file
if i try to use Data.Array all hell breaks loose 

```
build-depends: ...,
               array
			   
```


i like to use quickcheck also .
```
build-depends: base , HUnit , hspec , QuickCheck , basic-sum-lib 
```

# Packages 


## what is hunit ?

haskell unit testing 

## what is hspec ? 

hspec is a testing framework i think , works with hunit , quickcheck etc.. 

## what is quickcheck ?

quickcheck tests properties of functions such as head of a pair (x:xs) is always the 
first element (the x) of a pair 

## what is array ? 

array represents Data.Array apparently 

since haskell is a functional programming language by default want to use functional 
equivalents 

essentially everything is a CONS operation so nearest can get to a non-mutable array is
some sort of s expression tree

there is a lot going on underneath the hood to be sure.

## what is containers ? 

Data.Map and Data.Set is found in containers package 

## performance 

## space leaks 

holding onto memory that can never release because somewhere have a handle to it , it may
potentially be required in some distant future , may give rise to a space leak

## 
