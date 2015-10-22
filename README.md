An R package for building and exploring Word2Vec models. (And ideally, other models like GloVe as well, though I haven't gotten around to it.) Includes an altered version of Tomas Mikolov's original C code for word2vec; those wrappers were origally written by Jian Li, and I've only tweaked them a little. Also includes a number of vector manipulation functions (like cosine similarity) for people who don't want to worry too much about vectors.

It doesn't include a full vignette in the code, but if you cut and paste what's below, you can get started by building a model on 77 historical cookbooks from Michigan State University (which I encountered thanks to Alan Liu.)

This package gathers several useful operations for working with vector-space models of text.

If you want to run it without knowing the details, [jump to the quick-start guide](#quick-start).

# Description

It does three major things:

1. [Trains word2vec models](#creating-text-vectors)
2. [Creates a new `VectorSpaceModel` class to simplify extracting elements of vector space models](#vectorspacemodel-object)
3. [Bundles several basic functions to execute basic vector space operations like cosine similarity, nearest neighbor, and vector projection.](#useful-matrix-operations)

It's not extra fast, but once the data is loaded in most operations happen in suitable time for exploratory data analysis (under a second on my laptop.)

## Creating text vectors.

One portion of this is an expanded version of the code from Jian Li's `word2vec` package with a few additional parameters enabled as the function `train_word2vec`.

The input must still be in a single file and pre-tokenized, but it uses the existing word2vec C code. For online data processing, I like the gensim python implementation, but I don't plan to link that to R.

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

Since frequently an average of two vectors provides a better indication, multiple words can be collapsed into a single vector by specifying multiple labels. For example, this may provide a slightly better gender vector:

```{r}
vector_set[["king"]] - vector_set[[c("man","men")]] + vector_set[[c("woman","women")]]
```

## A few native functions defined on the VectorSpaceModel object.

The native `show` method just prints the dimensions; the native `print` method does some crazy reductions with the T-SNE package (installation required for functionality) because I think T-SNE is a nice way to reduce down the size of vectors.



## Useful matrix operations

One challenge of vector-space models of texts is that it takes some basic matrix multiplication functions to make them dance around in an entertaining way.

This package bundles the ones I think are the most useful. 
Each takes a `VectorSpaceModel` as its first argument. Sometimes, it's appropriate for the VSM to be your entire data set; other times, it's sensible to limit it to just one or a few vectors. Where appropriate, the functions can also take vectors or matrices as inputs.

  * `cosineSimilarity(VSM_1,VSM_2)` calculates the cosine similarity of every vector in on vector space model to every vector in another. This is `n^2` complexity. With a vocabulary size of 20,000 or so, it can be reasonable to compare an entire set to itself; or you can compare a larger set to a smaller one to search for particular terms of interest. 
  * `cosineDistance(VSM_1,VSM_2)` is the inverse of cosineSimilarity. It's not really a distance metric, but can be used as one for clustering and the like.
  * `nearest_to(VSM,vector,n)` wraps a particularly common use case for `cosineSimilarity`, of finding the top `n` terms in a `VectorSpaceModel` closest to term m
  * `project(VSM,vector)` takes a `VectorSpaceModel` and returns the portion parallel to the vector `vector`. 
  * `reject(VSM,vector)` is the inverse of `project`; it takes a `VectorSpaceModel` and returns the portion orthogonal to the vector `vector`. This makes it possible, for example, to collapse a vector space by removing certain distinctions of meaning.
  * `magnitudes` calculated the magnitude of each elmeent in a VSM. 
  
All of these functions place the VSM object first. This makes it easy to chain together operations using the `magrittr` package. For example, beginning with a single vector set one could find the nearest words in a set to a version of the vector for "bank" that has been decomposed to remove any semantic similarity to the banking sector.

``` {r}
library(magrittr)
not_that_kind_of_bank = chronam_vectors[["bank"]] %>%
      reject(chronam_vectors[["cashier"]]) %>% 
      reject(chronam_vectors[["depositors"]]) %>%   
      reject(chronam_vectors[["check"]])
chronam_vectors %>% nearest_to(not_that_kind_of_bank)
```

# Quick start

## Install the wordVectors package.

One of the major hurdles to running word2vec for ordinary people is that it requires compiling a C program. For many people, it may be easier to install it in R.

1. If you haven't already, [install R](https://cran.rstudio.com/) and then [install RStudio](https://www.rstudio.com/products/rstudio/download/).
2. Open	  R, and get a command-line prompt (the thing with a carat on the left hand side.) This is where you'll be copy-pasting commands.
3. Install (if you don't already have it) the package `devtools` by pasting the	    following
    ```R
    install.packages("devtools")
    ```

4. Install the latest version of this package from Github by pasting in the following.
    ```R
    library(devtools)
    install_github("bmschmidt/wordVectors")
    ```
    Windows users may need to install "Rtools" as well: if so, a message to this effect should appear in red on the screen. This may through a very large number of warnings: so long as it says "warning" and not "error", you're probably OK.

## Testing the setup

First, download and extract a zip file of cookbooks from the MSU library by pasting the following lines.
```{r}
if (!file.exists("cookbooks.zip")) {
  download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
  }
  unzip("cookbooks.zip",exdir="cookbooks")
  ```

Then load the wordVectors package you have already installed.
```{r}
library(wordVectors)
```

Next, we build a single text file consisting of all the cookbooks converted to lowercase with punctuation removed.

```{r}
prep_word2vec("cookbooks","cookbooks.txt",lowercase=T)
```

Now we *train* the model.

```{r}
model = train_word2vec("cookbooks.txt",output="cookbooks.vectors",threads = 3,vectors = 100,window=12)
```

* NOTE: If at any point you want to *read in* a previously trained model, you can do so by typing `model =  read.vectors("cookbooks.vectors")`

Now we have a model in memory, trained on about 10 million words from 77 cookbooks. What can it tell us about food?

Well, you can run some basic operations to find the nearest elements:

```{r}
nearest_to(model,model[["fish"]])
```

With that list, you can expand out further to search for multiple words:

```{r}
nearest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],50)
```

Now we have a pretty expansive list of potential fish-related words from old cookbooks. This may be useful for something in real life.

Or we can just arrange them somehow. If you have the tsne package installed, (type `install.packages("tsne")` to download it), you can plot these words in a reduced dimensional space. In this case, it doesn't look like much of anything.

```{r}
some_fish = nearest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],50)
plot(filter_to_rownames(model,names(some_fish)))
```

But this set actually gives a fairly nicely clustered set of results if you plot the top words in the whole thing.

```{r}
plot(model)
```

There's a lot of other stuff you can do besides just measuring nearness: you can do analogies, projection, and more complicated plots. But for that you should read my blog posts on this.
