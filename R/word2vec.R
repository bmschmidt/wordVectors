
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
##' @return A word2vec object.
##' @author Jian Li <\email{rweibo@@sina.com}>, Ben Schmidt <\email{bmchmidt@@gmail.com}>
##' @references \url{https://code.google.com/p/word2vec/}
##' @export
##'
##' @examples \dontrun{
##' model = word2vec(system.file("examples", "rfaq.txt", package = "tmcn.word2vec"))
##' }
train_word2vec <- function(train_file, output_file = "vectors.txt",vectors=100,threads=1,window=12)
{
  if (!file.exists(train_file)) stop("Can't find the training file!")
  if (file.exists(output_file)) return(read.vectors(output_file))

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
  binary = 0

  OUT <- .C("CWrapper_word2vec",
            train_file = as.character(train_file),
            output_file = as.character(output_file),
            binary = as.character(binary),
            dims=as.character(vectors),
            threads=as.character(threads),
            window=as.character(window)

  )

  read.vectors(output_file)
}

#' Prepare documents for word2Vec
#'
#' @description This function exports a directory or document to a single file
#' suitable to Word2Vec run on. That means a single, seekable txt file
#' with tokens separated by spaces. (For example, punctuation is removed
#' rather than attached to the end of words.)
#' This function is quite inefficient: in most cases, you'll be better
#' off preparing the documents using.
#'
#' @param origin A text file or a directory of text files
#'  to be used in training the model
#' @param destination The location for output text.
#' @param split_characters A list of characters that mark word breaks. By default,
#' any nonword characters according to the perl regex engine.
#' @param tolower Logical. Should uppercase characters be converted to lower?
#'
#' @export
#'
#' @return The file name (silently).
prep_word2vec <- function(origin,destination,
                          split_characters="\\W",lowercase=F)
{
  message("Beginning tokenization to text file at ", destination)
  if (dir.exists(origin)) {
    origin = list.files(origin,recursive=T,full.names = T)
  }
  cat("",file=destination,append=F)
  for (filename in origin) {
    message(filename)
    con = file(filename)
    all = readLines(con)
    words = unlist(strsplit(all,split_characters,perl=T))
    if (lowercase) {words=tolower(words)}
    cat(c(words,"\n"),file=destination,append=T)
    close(con)
  }
  silent = destination
}

