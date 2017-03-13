# VERSION 2.0

Upgrade focusing on ease of use and CRAN-ability. Bumping major version because of a breaking change in the behavior of `closest_to`, which now returns a data.frame.

# Changes

## New default function: closest_to.

`nearest_to` was previously the easiest way to interact with cosine similarity functions. That's been deprecated
in favor of a new function, `closest_to`. (I would have changed the name but for back-compatibility reasons.)
The data.frame columns have elaborate names so they can easily be manipulated with dplyr, and/or plotted with ggplot.
`nearest_to` is now just a wrapped version of the new function.

## New syntax for vector addition.

This package now allows formula scoping for the most common operations, and string inputs to access in the context of a particular matrix. This makes this much nicer for handling the bread and butter word2vec operations.

For instance, instead of writing 
```R
vectors %>% closest_to(vectors[rownames(vectors)=="king",] - vectors[rownames(vectors)=="man",] + vectors[rownames(vectors)=="woman",])
```

(whew!), you can write

```R
vectors %>% closest_to(~"king" - "man" + "woman")
```


## Reading tweaks.

In keeping with the goal of allowing manipulation of models in low-memory environments, it's now possible to read only rows with words matching certain criteria by passing an argument to read.binary.vectors(); either `rowname_list` for a fixed list, or `rowname_regexp` for a regular expression. (You could, say, read only the gerunds from a file by entering `rowname_regexp = "*.ing"`).

## Test Suite

The package now includes a test suite.

## Other materials for rOpenScience and JOSS. 

This package has enough users it might be nice to get it on CRAN. I'm trying doing so through rOpenSci. That requires a lot of small files scattered throughout.


# VERSION 1.3

Two significant performance improvements.
1. Row magnitudes for a `VectorSpaceModel` object are now **cached** in an environment that allows some pass-by-reference editing. This means that the most time-consuming part of any comparison query is only done once for any given vector set; subsequent queries are at least an order of magnitude (10-20x)? faster.

Although this is a big performance improvement, certain edge cases might not wipe the cache clear. **In particular, assignment inside a VSM object might cause incorrect calculations.** I can't see why anyone would be in the habit of manually tweaking a row or block (rather than a whole matrix).
1. Access to rows in a `VectorSpaceModel` object is now handled through callNextMethod() rather than accessing the element's .Data slot. For reasons opaque to me, hitting the .Data slot seems to internally require copying the whole huge matrix internally. Now that no longer happens.


# VERSION 1.2

This release implements a number of incremental improvements and clears up some errors.
- The package is now able to read and write in the binary word2vec format; since this is faster and takes much less hard drive space (down by about 66%) than writing out floats as text, it does so internally.
- Several improvements to the C codebase to avoid warnings by @mukul13, described [here](https://github.com/bmschmidt/wordVectors/pull/9). (Does this address the `long long` issue?)
- Subsetting with `[[` now takes an argument `average`; if false, rather than collapse a matrix down to a single row, it just extracts the elements that correspond to the words.
- Added sample data in the object `demo_vectors`: the 999 words from the most common vectors.
- Began adding examples to the codebase.
- Tracking build success using Travis.
- Dodging most warnings from R CMD check.

Bug fixes
- If the `dir.exists` function is undefined, it creates one for you. This should allow installation on R 3.1 and some lower versions.
- `reject` and `project` are better about returning VSM objects, rather than dropping back into a matrix.

# VERSION 1.1

A few changes, primarily to the functions for _training_ vector spaces to produce higher quality models. A number of these changes are merged back in from the fork of this repo by github user @sunecasp . Thanks!

## Some bug fixes

Filenames can now be up to 1024 characters. Some parameters on alpha decay may be fixed; I'm not entirely sure what sunecasp's changes do.

## Changes to default number of iterations.

Models now default to 5 iterations through the text rather than 1. That means training may take 5 times as long; but particularly for small corpora, the vectors should be of higher quality. See below for an example. 

## More training arguments

You can now specify more flags to the word2vec code. `?train_word2vec` gives a full list, but particularly useful are:
1. `window` now accurately sets the window size.
2. `iter` sets the number of iterations. For very large corpora, `iter=1` will train most quickly; for very small corpora, `iter=15` will give substantially better vectors. (See below). You should set this as high as you can stand within reason (Setting `iter` to a number higher than `window` is probably not that useful). But more text is better than more iterations.
3. `min_count` gives a cutoff for vocabulary size. Tokens occurring fewer than `min_count` times will be dropped from the model. Setting this high can be useful. (But note that a trained model is sorted in order of frequency, so if you have the RAM to train a big model you can reduce it in size for analysis by just subsetting to the first 10,000 or whatever rows).

## Example of vectors

Here's an example of training on a small set (c. 1000 speeches on the floor of the house of commons from the early 19th century). 

> proc.time({one = train_word2vec("~/tmp2.txt","~/1_iter.vectors",iter = 1)})
> Error in train_word2vec("~/tmp2.txt", "~/1_iter.vectors", iter = 1) : 
>   The output file '~/1_iter.vectors' already exists: delete or give a new destination.
> proc.time({one = train_word2vec("~/tmp2.txt","~/1_iter.vectors",iter = 1)})
> Starting training using file /Users/bschmidt/tmp2.txt
> Vocab size: 4469
> Words in train file: 407583
> Alpha: 0.000711  Progress: 99.86%  Words/thread/sec: 67.51k  
> Error in proc.time({ : 1 argument passed to 'proc.time' which requires 0
> ?proc.time
> system.time({one = train_word2vec("~/tmp2.txt","~/1_iter.vectors",iter = 1)})
> Starting training using file /Users/bschmidt/tmp2.txt
> Vocab size: 4469
> Words in train file: 407583
> Alpha: 0.000711  Progress: 99.86%  Words/thread/sec: 66.93k     user  system elapsed 
>   6.753   0.055   6.796 
> system.time({two = train_word2vec("~/tmp2.txt","~/2_iter.vectors",iter = 3)})
> Starting training using file /Users/bschmidt/tmp2.txt
> Vocab size: 4469
> Words in train file: 407583
> Alpha: 0.000237  Progress: 99.95%  Words/thread/sec: 67.15k     user  system elapsed 
>  18.846   0.085  18.896 
> 
> two %>% nearest_to(two["debt"]) %>% round(3)
>         debt    remainder          Jan    including     drawback manufactures    prisoners   mercantile   subsisting 
>        0.000        0.234        0.256        0.281        0.291        0.293        0.297        0.314        0.314 
>          Dec 
>        0.318 
> one %>% nearest_to(one[["debt"]]) %>% round(3)
>       debt  Christmas  exception preventing     Indies     import  remainder        eye   eighteen  labouring 
>      0.000      0.150      0.210      0.214      0.215      0.220      0.221      0.223      0.225      0.227 
> 
> system.time({ten = train_word2vec("~/tmp2.txt","~/10_iter.vectors",iter = 10)})
> Starting training using file /Users/bschmidt/tmp2.txt
> Vocab size: 4469
> Words in train file: 407583
> Alpha: 0.000071  Progress: 99.98%  Words/thread/sec: 66.13k     user  system elapsed 
>  62.070   0.296  62.333 
> 
> ten %>% nearest_to(ten[["debt"]]) %>% round(3)
>          debt       surplus           Dec     remainder manufacturing        grants           Jan      drawback     prisoners 
>         0.000         0.497         0.504         0.510         0.519         0.520         0.533         0.536         0.546 
>    compelling 
>         0.553 

```
```

