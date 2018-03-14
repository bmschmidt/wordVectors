##' Train a model by word2vec.
##'
##' The word2vec tool takes a text corpus as input and produces the
##' word vectors as output. It first constructs a vocabulary from the
##' training text data and then learns vector representation of words.
##' The resulting word vector file can be used as features in many
##' natural language processing and machine learning applications.
##'
##'
##'
##' @title Train a model by word2vec.
##' @param train_file Path of a single .txt file for training. Tokens are split on spaces.
##' @param output_file Path of the output file.
##' @param vectors The number of vectors to output. Defaults to 100.
##' More vectors usually means more precision, but also more random error, higher memory usage, and slower operations.
##' Sensible choices are probably in the range 100-500.
##' @param threads Number of threads to run training process on.
##' Defaults to 1; up to the number of (virtual) cores on your machine may speed things up.
##' @param window The size of the window (in words) to use in training.
##' @param classes Number of classes for k-means clustering. Not documented/tested.
##' @param cbow If 1, use a continuous-bag-of-words model instead of skip-grams.
##' Defaults to false (recommended for newcomers).
##' @param min_count Minimum times a word must appear to be included in the samples.
##' High values help reduce model size.
##' @param iter Number of passes to make over the corpus in training.
##' @param force Whether to overwrite existing model files.
##' @param negative_samples Number of negative samples to take in skip-gram training. 0 means full sampling, while lower numbers
##' give faster training. For large corpora 2-5 may work; for smaller corpora, 5-15 is reasonable.
##' @return A VectorSpaceModel object.
##' @author Jian Li <\email{rweibo@@sina.com}>, Ben Schmidt <\email{bmchmidt@@gmail.com}>
##' @references \url{https://code.google.com/p/word2vec/}
##' @export
##'
##' @useDynLib wordVectors
##'
##' @examples \dontrun{
##' model = train_word2vec(system.file("examples", "rfaq.txt", package = "wordVectors"))
##' }
train_word2vec <- function(train_file, output_file = "vectors.bin",vectors=100,threads=1,window=12,
                           classes=0,cbow=0,min_count=5,iter=5,force=F, negative_samples=5)
{
  if (!file.exists(train_file)) stop("Can't find the training file!")
  if (file.exists(output_file) && !force) stop("The output file '",
                                     output_file ,
                                     "' already exists: give a new destination or run with 'force=TRUE'.")

  train_dir <- dirname(train_file)

  # cat HDA15/data/Dickens/* | perl -pe 'print "1\t"' | egrep "[a-z]" | bookworm tokenize token_stream > ~/test.txt

  if(missing(output_file)) {
    output_file <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
    output_file <- file.path(train_dir, output_file)
  }

  outfile_dir <- dirname(output_file)
  if (!file.exists(outfile_dir)) dir.create(outfile_dir, recursive = TRUE)

  train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  # Whether to output binary, default is 1 means binary.
  binary = 1

  OUT <- .C("CWrapper_word2vec",
            train_file = as.character(train_file),
            output_file = as.character(output_file),
            binary = as.character(binary),
            dims=as.character(vectors),
            threads=as.character(threads),
            window=as.character(window),
            classes=as.character(classes),
            cbow=as.character(cbow),
            min_count=as.character(min_count),
            iter=as.character(iter),
            neg_samples=as.character(negative_samples)
  )

  read.vectors(output_file)
}

#' Prepare documents for word2Vec
#'
#' @description This function exports a directory or document to a single file
#' suitable to Word2Vec run on. That means a single, seekable txt file
#' with tokens separated by spaces. (For example, punctuation is removed
#' rather than attached to the end of words.)
#' This function is extraordinarily inefficient: in most real-world cases, you'll be
#' much better off preparing the documents using python, perl, awk, or any other
#' scripting language that can reasonable read things in line-by-line.
#'
#' @param origin A text file or a directory of text files
#'  to be used in training the model
#' @param destination The location for output text.
#' @param lowercase Logical. Should uppercase characters be converted to lower?
#' @param bundle_ngrams Integer. Statistically significant phrases of up to this many words
#' will be joined with underscores: e.g., "United States" will usually be changed to "United_States"
#' if it appears frequently in the corpus. This calls word2phrase once if bundle_ngrams is 2,
#' twice if bundle_ngrams is 3, and so forth; see that function for more details.
#' @param ... Further arguments passed to word2phrase when bundle_ngrams is
#' greater than 1.
#'
#' @export
#'
#' @return The file name (silently).
prep_word2vec <- function(origin,destination,lowercase=F,
                          bundle_ngrams=1, ...)
{
  # strsplit chokes on large lines. I would not have gone down this path if I knew this
  # to begin with.



  message("Beginning tokenization to text file at ", destination)
  if (!exists("dir.exists")) {
    # Use the version from devtools if in R < 3.2.0
    dir.exists <- function (x)
    {
      res <- file.exists(x) & file.info(x)$isdir
      stats::setNames(res, x)
    }
  }

  if (dir.exists(origin)) {
    origin = list.files(origin,recursive=T,full.names = T)
  }

  if (file.exists(destination)) file.remove(destination)

  tokenize_words = function (x, lowercase = TRUE) {
    # This is an abbreviated version of the "tokenizers" package version to remove the dependency.
    # Sorry, Lincoln, it was failing some tests.
    if (lowercase) x <- stringi::stri_trans_tolower(x)
    out <- stringi::stri_split_boundaries(x, type = "word", skip_word_none = TRUE)
    unlist(out)
  }

  prep_single_file <- function(file_in, file_out, lowercase) {
    message("Prepping ", file_in)

    text <- file_in %>%
      readr::read_file() %>%
      tokenize_words(lowercase) %>%
      stringr::str_c(collapse = " ")

    if (length(text) == 1) {
      readr::write_lines(text, file_out, append = TRUE)
      return(TRUE)
    } else {
      message("Warning: No text to prep in ", file_in)
      return(FALSE)
    }
  }


  Map(prep_single_file, origin, lowercase=lowercase, file_out=destination)

  # Save the ultimate output
  real_destination_name = destination

  # Repeatedly build bigrams, trigrams, etc.
  if (bundle_ngrams > 1) {
    while(bundle_ngrams > 1) {
      old_destination = destination
      destination = paste0(destination,"_")
      word2phrase(old_destination,destination,...)
      file.remove(old_destination)
      bundle_ngrams = bundle_ngrams - 1
    }
    file.rename(destination,real_destination_name)
  }

  silent = real_destination_name
}


#' Convert words to phrases in a text file.
#'
#' This function attempts to learn phrases given a text document.
#' It does so by progressively joining adjacent pairs of words with an '_' character.
#' You can then run the code multiple times to create multiword phrases.
#' Wrapper around code from the Mikolov's original word2vec release.
#'
#' @title Convert words to phrases
#' @author Tomas Mikolov
#' @param train_file Path of a single .txt file for training.
#'   Tokens are split on spaces.
#' @param output_file Path of output file
#' @param debug_mode debug mode. Must be 0, 1 or 2. 0 is silent; 1 print summary statistics;
#'  prints progress regularly.
#' @param min_count Minimum times a word must appear to be included in the samples.
#'   High values help reduce model size.
#' @param threshold Threshold value for determining if pairs of words are phrases.
#' @param force Whether to overwrite existing files at the output location. Default FALSE
#'
#' @return The name of output_file, the trained file where common phrases are now joined.
#'
#' @export
#' @examples
#' \dontrun{
#' model=word2phrase("text8","vec.txt")
#' }

word2phrase=function(train_file,output_file,debug_mode=0,min_count=5,threshold=100,force=FALSE)
{
  if (!file.exists(train_file)) stop("Can't find the training file!")
  if (file.exists(output_file) && !force) stop("The output file '",
                                               output_file ,
                                               "' already exists: give a new destination or run with 'force=TRUE'.")
  OUT=.C("word2phrase",rtrain_file=as.character(train_file),
         rdebug_mode=as.integer(debug_mode),
         routput_file=as.character(output_file),
         rmin_count=as.integer(min_count),
         rthreshold=as.double(threshold))
  return(output_file)
}
