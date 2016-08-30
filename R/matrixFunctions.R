#' Vector Space Model class
#'
#' @description A class for describing and accessing Vector Space Models like Word2Vec.
#' The base object is simply a matrix with columns describing dimensions and unique rownames
#' as the names of vectors. This package gives a number of convenience functions for printing
#' and, most importantly, accessing these objects.
#' @slot magnitudes The cached sum-of-squares for each row in the matrix. Can be cached to
#' speed up similarity calculations
#' @return An object of class "VectorSpaceModel"
#' @exportClass VectorSpaceModel
setClass("VectorSpaceModel",slots = c(".cache"="environment"),contains="matrix")
#setClass("NormalizedVectorSpaceModel",contains="VectorSpaceModel")

# This is Steve Lianoglu's method for associating a cache with an object
# http://r.789695.n4.nabble.com/Change-value-of-a-slot-of-an-S4-object-within-a-method-td2338484.html
setMethod("initialize", "VectorSpaceModel",
          function(.Object, ..., .cache=new.env()) {
            callNextMethod(.Object, .cache=.cache, ...)
          })

#' Square Magnitudes with caching
#'
#' @param VectorSpaceModel A matrix or VectorSpaceModel object
#' @description square_magnitudes Returns the square magnitudes and
#' caches them if necessary
#' @return A vector of the square magnitudes for each row
#' @keywords internal
square_magnitudes = function(object) {
  if (class(object)=="VectorSpaceModel") {
      if (.hasSlot(object, ".cache")) {
      if (is.null(object@.cache$magnitudes)) {
        object@.cache$magnitudes = rowSums(object^2)
      }
      return(object@.cache$magnitudes)
      } else {
        message("You seem to be using a VectorSpaceModel saved from an earlier version of this package.")
        message("To turn on caching, which greatly speeds up queries, type")
        message("yourobjectname@.cache = new.env()")
        return(rowSums(object^2))
      }
  } else {
    return(rowSums(object^2))
  }
}


#' VectorSpaceModel indexing
#'
#' @description Reduce a VectorSpaceModel to a smaller one
#' @param x The vectorspace model to subset
#' @param i The row numbers to extract
#' @param j The column numbers to extract
#' @param j Other arguments to extract (unlikely to be useful).
#' @param drop Whether to drop columns. This parameter is ignored.
#' @return A VectorSpaceModel
#'
setMethod("[","VectorSpaceModel",function(x,i,j,...,drop) {
  nextup = callNextMethod()
  if (!is.matrix(nextup)) {
    # A verbose way of effectively changing drop from TRUE to FALSE;
    # I don't want one-dimensional matrices turned to vectors.
    # I can't figure out how to do this more simply
    if (missing(j)) {
      nextup = matrix(nextup,ncol=ncol(x))
    } else {
      nextup = matrix(nextup,ncol=j)
    }
  }
  new("VectorSpaceModel",nextup)
})

#' VectorSpaceModel subtraction
#'
#' @description Keep the VSM class when doing subtraction operations;
#' make it possible to subtract a single row from an entire model.
#' @param e1 A vector space model
#' @param e2 A vector space model of equal size OR a vector
#' space model of a single row. If the latter (which is more likely)
#' the specified row will be subtracted from each row.
#'
#'
#' @return A VectorSpaceModel of the same dimensions and rownames
#' as e1
#'
#' I believe this is necessary, but honestly am not sure.
#'
setMethod("-",signature(e1="VectorSpaceModel",e2="VectorSpaceModel"),function(e1,e2) {
    if (nrow(e1)==nrow(e2) && ncol(e1)==ncol(e2)) {
      return (methods::new("VectorSpaceModel",callNextMethod()))
    }
    if (nrow(e2)==1) {
      return(
        new("VectorSpaceModel",e1 - matrix(rep(e2,each=nrow(e1)),nrow=nrow(e1)))
        )
    }
    stop("Vector space model subtraction must use models of equal dimensions")
})

#' VectorSpaceModel subsetting
#'
#  @description Reduce a VectorSpaceModel to a single object.
#' @param x The object being subsetted.
#' @param i A character vector: the words to use as rownames.
#' @param average Whether to collapse down to a single vector,
#' or to return a subset of one row for each asked for.
#'
#' @return A VectorSpaceModel of a single row.
setMethod("[[","VectorSpaceModel",function(x,i,average=TRUE) {
  # The wordvec class can extract a row from the matrix
  # by accessing the rownames. x[["king"]] gives the row
  # for which the rowname is "king"; x[[c("king","queen")]] gives
  # the midpoint of x[["king"]] and x[["queen"]], which can occasionally
  # be useful.
  if(typeof(i)=="character") {
    matching_rows = x[rownames(x) %in% i,]
    if (average) {
      val = matrix(
              colMeans(matching_rows)
              ,nrow=1
              ,dimnames = list(
                c(),colnames(x))
            )
    } else {
      val=matching_rows
      rownames(val) = rownames(x)[rownames(x) %in% i]
    }

  return(methods::new("VectorSpaceModel",val))
  }

  else if (typeof(i)=="integer") {
  return(x[i,])
  } else {
    stop("VectorSpaceModel objects are accessed by vectors of numbers or words")
  }
})

setMethod("show","VectorSpaceModel",function(object) {
  dims = dim(object)
  cat("A VectorSpaceModel object of ",dims[1]," words and ", dims[2], " vectors\n")
  methods::show(unclass(object[1:min(nrow(object),10),1:min(ncol(object),6)]))
})

#' Plot a Vector Space Model.
#'
#' Visualizing a model as a whole is sort of undefined. I think the
#' sanest thing to do is reduce the full model down to two dimensions
#' using T-SNE, which preserves some of the local clusters.
#'
#' This plots only the first 300 words in the model.
#'
#' @param x The model to plot
#' @param y (ignored)
#' @param ... Further arguments passed to tsne::tsne.
#' (Note: not to plot.)
#'
#' @return The TSNE model (silently.)
#' @export
setMethod("plot","VectorSpaceModel",function(x,y,...) {
  message("Attempting to use T-SNE to plot the vector representation")
  message("Cancel if this is taking too long")
  message("Or run 'install.packages' tsne if you don't have it.")
  x = as.matrix(x)
  short = x[1:min(300,nrow(x)),]
  m = tsne::tsne(short,...)
  plot(m,type='n',main="A two dimensional reduction of the vector space model using t-SNE")
  graphics::text(m,rownames(short),cex = ((400:1)/200)^(1/3))
  rownames(m)=rownames(short)
  silent = m
})

#' Convert to a Vector Space Model
#'
#' @param matrix A matrix to coerce.
#'
#' @return An object of class "VectorSpaceModel"
#' @export as.VectorSpaceModel
as.VectorSpaceModel = function(matrix) {
  return(methods::new("VectorSpaceModel",matrix))
}

#' Read VectorSpaceModel
#'
#' Read a VectorSpaceModel from a file exported from word2vec or a similar output format.
#'
#' @param filename The file to read in.
#' @param vectors The number of dimensions word2vec calculated. Imputed automatically if not specified.
#' @param binary Read in the binary word2vec form. (Wraps `read.binary.vectors`)
#' @param ... Further arguments passed to read.table or read.binary.vectors.
#' Note that both accept 'nrow' as an argument. Word2vec produces
#' by default frequency sorted output. Therefore 'read.vectors(...,nrows=500)', for example,
#' will return the vectors for the top 500 words. This can be useful on machines with limited
#' memory.
#' @export
#' @return An matrixlike object of class `VectorSpaceModel`
#'
read.vectors <- function(filename,vectors=guess_n_cols(),binary=FALSE,...) {

  if(rev(strsplit(filename,"\\.")[[1]])[1] =="bin") {
    message("Filename ends with .bin, so reading in binary format")
    binary=TRUE
  }

  if(binary) {
    return(read.binary.vectors(filename,...))
  }

  # Figure out how many dimensions.
  guess_n_cols = function() {
    # if cols is not defined
    test = utils::read.table(filename,header=F,skip=1,
                       nrows=1,quote="",comment.char="")
  return(ncol(test)-1)
  }
  vectors_matrix = utils::read.table(filename,header=F,skip=1,
                               colClasses = c("character",rep("numeric",vectors)),
                       quote="",comment.char="",...)
  names(vectors_matrix)[1] = "word"
  vectors_matrix$word[is.na(vectors_matrix$word)] = "NA"
  matrix = as.matrix(vectors_matrix[,colnames(vectors_matrix)!="word"])
  rownames(matrix) = vectors_matrix$word
  colnames(matrix) = paste0("V",1:vectors)
  return(methods::new("VectorSpaceModel",matrix))
}

#' Read binary word2vec format files
#'
#' @param filename A file in the binary word2vec format to import.
#' @param nrows Optionally, a number of rows to stop reading after.
#' Word2vec sorts by frequency, so limiting to the first 1000 rows will
#' give the thousand most-common words; it can be useful not to load
#' the whole matrix into memory
#' @param cols The column numbers to read. Default is "All";
#' if you are in a memory-limited environment,
#' you can limit the number of columns you read in by giving a vector of column integers
#' @return A VectorSpaceModel object
#' @export
#'
#'

read.binary.vectors = function(filename,nrows=Inf,cols="All") {
  a = file(filename,'rb')
  rows = ""
  mostRecent=""
  while(mostRecent!=" ") {
    mostRecent = readChar(a,1)
    rows = paste0(rows,mostRecent)
  }
  rows = as.integer(rows)

  col_number = ""
  while(mostRecent!="\n") {
    mostRecent = readChar(a,1)
    col_number = paste0(col_number,mostRecent)
  }
  col_number = as.integer(col_number)

  if(nrows<rows) {
    message(paste("Reading the first",nrows, "rows of a word2vec binary file of",rows,"rows and",col_number,"columns"))
    rows = nrows
  } else {
    message(paste("Reading a word2vec binary file of",rows,"rows and",col_number,"columns"))
  }


  ## Read a row
  rownames = rep("",rows)

  # create progress bar
  pb <- utils::txtProgressBar(min = 0, max = rows, style = 3)


  returned_columns = col_number
  if (is.integer(cols)) {
    returned_columns = length(cols)
  }

  matrix = t(
    vapply(1:rows,function(i) {
      utils::setTxtProgressBar(pb,i)
      rowname=""
      mostRecent=""
      while(TRUE) {
        mostRecent = readChar(a,1)
        if (mostRecent==" ") {break}
        if (mostRecent!="\n") {
          # Some versions end with newlines, some don't.
          rowname = paste0(rowname,mostRecent)
        }
      }
      rownames[i] <<- rowname
      row = readBin(a,numeric(),size=4,n=col_number,endian="little")
      if (is.integer(cols)) {
        return(row[cols])
      }
      return(row)
    },as.array(rep(0,returned_columns)))
  )
  close(pb)
  close(a)
  rownames(matrix) = rownames
  return(as.VectorSpaceModel(matrix))
}

#' Write in word2vec binary format
#'
#' @param model The wordVectors model you wish to save. (This can actually be any matrix with rownames,
#' if you want a smaller binary serialization in single-precision floats.)
#' @param filename The file to save the vectors to. I recommend ".bin" as a suffix.
#'
#' @return Nothing
#' @export
write.binary.word2vec = function(model,filename) {
  if (!endsWith(filename,".bin")) {
    warning("Your filename doesn't end with '.bin'; any subsequent calls to read.vectors()
            will fail unless you set 'binary' in the function args to TRUE.")
  }
  filehandle = file(filename,"wb")
  dim = dim(model)
  writeChar(as.character(dim[1]),filehandle,eos=NULL)
  writeChar(" ",filehandle,eos=NULL)
  writeChar(as.character(dim[2]),filehandle,eos=NULL)
  writeChar("\n",filehandle,eos=NULL)
  names = rownames(model)
  # I just store the rownames outside the loop, here.
  i = 1
  names = rownames(model)
  silent = apply(model,1,function(row) {
    # EOS must be null for this to work properly, because, ridiculously,
    # 'eos=NULL' is the command that tells R *not* to insert a null string
    # after a character.
    writeChar(paste0(names[i]," "),filehandle,eos=NULL)
    writeBin(row,filehandle,size=4,endian="little")
    i <<- i+1
  })
  close(filehandle)
}

#' Vector Magnitudes
#'
#' @param matrix A matrix or VectorSpaceModel object.
#'
#' @return A vector consisting of the magnitudes of each row.
#'
#' This is an extraordinarily simple function.
#'
#' @export
magnitudes <- function(matrix) {
  sqrt(rowSums(matrix^2))
}

#' Matrix normalization.
#'
#' Normalize a matrix so that all rows are of unit length.
#'
#' @param matrix A matrix or VectorSpaceModel object
#'
#' @return An object of the same class as matrix
#' @export
normalize_lengths = function(matrix) {
  t(t(matrix)/magnitudes(matrix))
}

#' Reduce by rownames
#'
#' @param matrix A matrix or VectorSpaceModel object
#' @param words A list of rownames or VectorSpaceModel names
#'
#' @return An object of the same class as matrix, consisting
#' of the rows that match its rownames.
#'
#' Deprecated: use instead VSM[[c("word1","word2",...),average=FALSE]]
#'
#' @export
filter_to_rownames <- function(matrix,words) {
  warning('The function`filter_to_rownames` is deprecated and will be removed in a later version.
          Use instead `VSM[[c("word1","word2",...),average=FALSE]]`')
  matrix[rownames(matrix) %in% words,]
}

#' Cosine Similarity
#'
#' @description Calculate the cosine similarity of two matrices or a matrix and a vector.
#'
#' @param x A matrix or VectorSpaceModel object
#' @param y A vector, matrix or VectorSpaceModel object.
#'
#' Vector inputs are coerced to single-row matrices; y must have the
#' same number of dimensions as x.
#'
#'
#' @return A matrix. Rows correspond to entries in x; columns to entries in y.
#'
#' @examples
#' subjects = demo_vectors[[c("history","literature","biology","math","stats"),average=FALSE]]
#' similarities = cosineSimilarity(subjects,subjects)
#'
#' subjects = demo_vectors[[c("history","literature","biology","math","stats"),average=TRUE]]
#' new_subject_list = nearest_to(demo_vectors,subjects,20)
#' new_subjects = demo_vectors[[names(new_subject_list),average=FALSE]]
#' plot(hclust(as.dist(cosineDist(new_subjects,new_subjects))))
#'
#' @export
cosineSimilarity <- function(x,y){
  # The most straightforward definition would be just:
  #  x %*% t(y)      /     (sqrt(rowSums(x^2) %*% t(rowSums(y^2))))
  # However, we have to do a little type-checking and a few speedups.

  if (!(is.matrix(x) || is.matrix(y))) {
    if (length(x)==length(y)) {
      x = as.matrix(x,ncol=length(x))
      y = as.matrix(y,ncol=length(y))
    }
    else {
      stop("At least one input must be a matrix")
    }
  }

  if (is.vector(x)) {
    x = as.matrix(x,ncol=ncol(y))
  }
  if (is.vector(y)) {
    y = as.matrix(y,ncol=ncol(x))
  }

  # Using tcrossprod should be faster than transposing manually.
  # Of course, this is still double-inefficient b/c we're calculating both
  # triangles of a symmetrical matrix, I think.
  tcrossprod(x,y)/
    (sqrt(tcrossprod(square_magnitudes(x),square_magnitudes(y))))

  #
}

#' Cosine Distance
#' @description Calculate the cosine distance between two vectors.
#'
#' Not an actual distance metric, but can be used in similar contexts.
#' It is calculated as simply the inverse of cosine similarity,
#' and falls in a fixed range of 0 (identical) to 2 (completely opposite in direction.)
#'
#' @param x A matrix, VectorSpaceModel, or vector.
#' @param y A matrix, VectorSpaceModel, or vector.
#'
#' @return A matrix whose dimnames are rownames(x), rownames(y)
#'
#' @export
cosineDist <- function(x,y) {
  1-(cosineSimilarity(x,y))
}

#' Project each row of an input matrix along a vector.
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector A vector (or an object coercable to a vector, see project)
#' of the same length as the VectorSpaceModel.
#'
#' @return A new matrix or VectorSpaceModel of the same dimensions as `matrix`,
#' each row of which is parallel to vector.
#'
#' If the input is a matrix, the output will be a matrix: if a VectorSpaceModel,
#' it will be a VectorSpaceModel.
#'
#'
#' @export
project = function(matrix,vector) {
  # The matrix is a matrix:
  # b is a vector to reproject the matrix to be orthogonal to.
  b = as.vector(vector)
  if (length(b)!=ncol(matrix)) {
    stop("The vector must be the same length as the matrix it is being compared to")
  }
  newmat = crossprod(t(matrix %*% b)/as.vector((b %*% b)) , b)
  return(new("VectorSpaceModel",matrix))
  }

#' Return a vector rejection for each element in a VectorSpaceModel
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector A vector (or an object coercable to a vector, see project)
#' of the same length as the VectorSpaceModel.
#'
#' @return A new matrix or VectorSpaceModel of the same dimensions as `matrix`,
#' each row of which is orthogonal to the `vector` object.
#'
#' This is defined simply as `matrix-project(matrix,vector)`, but having a separate
#' name may make for cleaner code.
#'
#' See `project` for more details.
#'
#' @examples
#' nearest_to(demo_vectors,demo_vectors[["man"]])
#' genderless = reject(demo_vectors,demo_vectors[["he"]] - demo_vectors[["she"]])
#' nearest_to(genderless,genderless[["man"]])
#'
#' @export
reject = function(matrix,vector) {
  # The projection of the matrix that _does not_ lie parallel to a given vector.
  val = matrix-project(matrix,vector)
  return(val)
}

#' Return the n closest words in a VectorSpaceModel to a given vector.
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector  Avector (or an object coercable to a vector, see project)
#' of the same length as the VectorSpaceModel.
#' @param n The number of closest words to include.
#'
#' @return A vector of distances, with names corresponding to the words
#' in the parent VectorSpaceModel, of length n.
#'
#' @examples
#'
#' #Synonyms and similar words
#' nearest_to(demo_vectors,demo_vectors[["good"]])
#'
#' # Something close to the classic king:man::queen:woman;
#' # What's the equivalent word for a female teacher that "guy" is for
#' # a male one?
#' nearest_to(demo_vectors,demo_vectors[["guy"]] - demo_vectors[["man"]] + demo_vectors[["woman"]])
#'
#' @export

nearest_to = function(matrix,vector,n=10) {
  sims = cosineSimilarity(matrix,matrix(as.vector(vector),ncol=ncol(matrix)))
  ords = order(-sims[,1])
  structure(
    1-sims[ords[1:n]], # Convert from similarity to distance.
    names=rownames(sims)[ords[1:n]])
}

