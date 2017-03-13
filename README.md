# Word Vectors

[![Build Status](https://travis-ci.org/bmschmidt/wordVectors.svg?branch=master)](https://travis-ci.org/bmschmidt/wordVectors)

An R package for building and exploring word embedding models.

# Description

This package does three major things to make it easier to work with word2vec and other vectorspace models of language.

1. [Trains word2vec models](#creating-text-vectors) using an extended Jian Li's word2vec code; reads and writes the binary word2vec format so that you can import pre-trained models such as Google's; and provides tools for reading only *part* of a model (rows or columns) so you can explore a model in memory-limited situations.
2. [Creates a new `VectorSpaceModel` class in R that gives a better syntax for exploring a word2vec or GloVe model than native matrix methods.](#vectorspacemodel-object) For example, instead of writing `model[rownames(model)=="king",]`, you can write `model[["king"]]`, and instead of writing `vectors %>% closest_to(vectors[rownames(vectors)=="king",] - vectors[rownames(vectors)=="man",] + vectors[rownames(vectors)=="woman",])` (whew!), you can write
`vectors %>% closest_to(~"king" - "man" + "woman")`.
3. [Implements several basic matrix operations that are useful in exploring word embedding models including cosine similarity, nearest neighbor, and vector projection](#useful-matrix-operations) with some caching that makes them much faster than the simplest implementations.

### Quick start

For a step-by-step interactive demo that includes installation and training a model on 77 historical cookbooks from Michigan State University, [see the introductory vignette.](https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd).

### Credit

This includes an altered version of Tomas Mikolov's original C code for word2vec; those wrappers were origally written by Jian Li, and I've only tweaked them a little. Several other users have improved that code since I posted it here.

Right now, it [does not (I don't think) install under Windows 8](https://github.com/bmschmidt/wordVectors/issues/2).  Help appreciated on that thread. OS X, Windows 7, Windows 10, and Linux install perfectly well, with one or two exceptions. 

It's not extremely fast, but once the data is loaded in most operations happen in suitable time for exploratory data analysis (under a second on my laptop.)

For high-performance analysis of models, C or python's numpy/gensim will likely be better than this package, in part because R doesn't have support for single-precision floats. The goal of this package is to facilitate clear code and exploratory data analysis of models.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

## Creating text vectors.

One portion of this is an expanded version of the code from Jian Li's `word2vec` package with a few additional parameters enabled as the function `train_word2vec`.

The input must still be in a single file and pre-tokenized, but it uses the existing word2vec C code. For online data processing, I like the gensim python implementation, but I don't plan to link that to R.

In RStudio I've noticed that this appears to hang, but if you check processors it actually still runs. Try it on smaller portions first, and then let it take time: the training function can take hours for tens of thousands of books.

## VectorSpaceModel object

The package loads in the word2vec binary format with the format `read.vectors` into a new object called a "VectorSpaceModel" object. It's a light superclass of the standard R matrix object. Anything you can do with matrices, you can do with VectorSpaceModel objects.

It has a few convenience functions as well. 

### Faster Access to text vectors

The rownames of a VectorSpaceModel object are presumed to be tokens in a vector space model and therefore semantically useful. The classic word2vec demonstration is that vector('king') - vector('man') + vector('woman') =~ vector('queen'). With a standard matrix, the vector on the right-hand side of the equation would be described as

```{r, include=F,show=T}
vector_set[rownames(vector_set)=="king",] - vector_set[rownames(vector_set)=="man",] + vector_set[rownames(vector_set)=="woman",]
```

In this package, you can simply access it by using the double brace operators:

```{r, include=F,show=T}
vector_set[["king"]] - vector_set[["man"]] + vector_set[["woman"]]
```

(And in the context of the custom functions, as a formula like `~"king" - "man" + "woman"`: see below).

Since frequently an average of two vectors provides a better indication, multiple words can be collapsed into a single vector by specifying multiple labels. For example, this may provide a slightly better gender vector:

```{r}
vector_set[["king"]] - vector_set[[c("man","men")]] + vector_set[[c("woman","women")]]
```

Sometimes you want to subset *without* averaging. You can do this with the argument `average==FALSE` to the subset. This is particularly useful for comparing slices of the matrix to itself in similarity operations.

```{r}
cosineSimilarity(vector_set[[c("man","men","king"),average=F]], vector_set[[c("woman","women","queen"),average=F]]
```

## A few native functions defined on the VectorSpaceModel object.

The native `show` method just prints the dimensions; the native `plot` method does some crazy reductions with the T-SNE package (installation required for functionality) because T-SNE is a nice way to reduce down the size of vectors, **or** lets you pass `method='pca'` to array a full set or subset by the first two principal components.


## Useful matrix operations

One challenge of vector-space models of texts is that it takes some basic matrix multiplication functions to make them dance around in an entertaining way.

This package bundles the ones I think are the most useful. 
Each takes a `VectorSpaceModel` as its first argument. Sometimes, it's appropriate for the VSM to be your entire data set; other times, it's sensible to limit it to just one or a few vectors. Where appropriate, the functions can also take vectors or matrices as inputs.

  * `cosineSimilarity(VSM_1,VSM_2)` calculates the cosine similarity of every vector in on vector space model to every vector in another. This is `n^2` complexity. With a vocabulary size of 20,000 or so, it can be reasonable to compare an entire set to itself; or you can compare a larger set to a smaller one to search for particular terms of interest. 
  * `cosineDistance(VSM_1,VSM_2)` is the inverse of cosineSimilarity. It's not really a distance metric, but can be used as one for clustering and the like.
  * `closest_to(VSM,vector,n)` wraps a particularly common use case for `cosineSimilarity`, of finding the top `n` terms in a `VectorSpaceModel` closest to term m
  * `project(VSM,vector)` takes a `VectorSpaceModel` and returns the portion parallel to the vector `vector`. 
  * `reject(VSM,vector)` is the inverse of `project`; it takes a `VectorSpaceModel` and returns the portion orthogonal to the vector `vector`. This makes it possible, for example, to collapse a vector space by removing certain distinctions of meaning.
  * `magnitudes` calculated the magnitude of each element in a VSM. This is useful in many operations.
  
All of these functions place the VSM object as the first argument. This makes it easy to chain together operations using the `magrittr` package. For example, beginning with a single vector set one could find the nearest words in a set to a version of the vector for "bank" that has been decomposed to remove any semantic similarity to the banking sector.

``` {r}
library(magrittr)
not_that_kind_of_bank = chronam_vectors[["bank"]] %>%
      reject(chronam_vectors[["cashier"]]) %>% 
      reject(chronam_vectors[["depositors"]]) %>%   
      reject(chronam_vectors[["check"]])
chronam_vectors %>% closest_to(not_that_kind_of_bank)
```

These functions also allow an additional layer of syntactic sugar when working with word vectors. 

Or even just as a formula, if you're working entirely with a single model, so you don't have to keep referring to words; instead, you can use a formula interface to reduce typing and increase clarity.

```{r}
vectors %>% closest_to(~ "king" - "man" + "woman")
```


# Quick start

## Install the wordVectors package.

One of the major hurdles to running word2vec for ordinary people is that it requires compiling a C program. For many people, it may be easier to install it in R. 

1. If you haven't already, [install R](https://cran.rstudio.com/) and then [install RStudio](https://www.rstudio.com/products/rstudio/download/).
2. Open	R, and get a command-line prompt (the thing with a `>` on the left hand side.) This is where you'll be copy-pasting commands.
3. Install (if you don't already have it) the package `devtools` by pasting the	following
    ```R
    install.packages("devtools")
    ```

4. Install the latest version of this package from Github by pasting in the following.
    ```R
    devtools::install_github("bmschmidt/wordVectors")
    ```
    Windows users may need to install "Rtools" as well: if so, a message to this effect should appear in red on the screen. This may cycle through a very large number of warnings: so long as it says "warning" and not "error", you're probably OK.

## Train a model.

For instructions on training, see the [introductory vignette](https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd)

## Explore an existing model.

For instructions on exploration, see the end of the [introductory vignette](https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd), or the slower-paced [vignette on exploration](https://github.com/bmschmidt/wordVectors/blob/master/vignettes/exploration.Rmd)
