An R package for building and exploring Word2Vec models. (And ideally, other models like GloVe as well, though I haven't gotten around to it.) Includes an altered version of Tomas Mikolov's original C code for word2vec; those wrappers were origally written by Jian Li, and I've only tweaked them a little. Also includes a number of vector manipulation functions (like cosine similarity) for people who don't want to worry too much about vectors.

It doesn't include a full vignette in the code, but if you cut and paste what's below, you can get started by building a model on 77 historical cookbooks from Michigan State University (which I encountered thanks to Alan Liu.)

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
