#' Vector Space Model class
#'
#' @description A class for describing and accessing Vector Space Models like Word2Vec.
#' The base object is simply a matrix with columns describing dimensions and unique rownames
#' as the names of vectors. This package gives a number of convenience functions for printing
#' and, most importantly, accessing these objects.
#' @return An object of class "VectorSpaceModel"
#' @exportClass VectorSpaceModel
setClass("VectorSpaceModel",contains = "matrix")


#' VectorSpaceModel indexing
#'
#  @description Reduce a VectorSpaceModel to a smaller one
#' @param VectorSpaceModel
#'
#' @return A VectorSpaceModel
#'
#' I believe this is necessary, but honestly am not sure.
#'
setMethod("[","VectorSpaceModel",function(x,i,j,...) {
  new("VectorSpaceModel",x@.Data[i,j])
})


#' VectorSpaceModel subsetting
#'
#  @description Reduce a VectorSpaceModel to a single object.
#' @param VectorSpaceModel
#'
#' @return A VectorSpaceModel of a single row.
setMethod("[[","VectorSpaceModel",function(x,i,...) {
  # The wordvec class can extract a row from the matrix
  # by accessing the rownames. x[["king"]] gives the row
  # for which the rowname is "king"; x[[c("king","queen")]] gives
  # the midpoint of x[["king"]] and x[["queen"]], which can occasionally
  # be useful.
  if(typeof(i)=="character")
  {
  val = matrix(
    colMeans(
      x[rownames(x) %in% i,,drop=F]
      )
    ,nrow=1
    ,dimnames = list(
      c(),colnames(x))
    )
  return(new("VectorSpaceModel",val))
  }
  else if (typeof(i)=="integer") {
  return(x[i,])
  } else {
    stop("VectorSpaceModel objects are accessed by vectors of numbers or words")
  }
})

setMethod("show","VectorSpaceModel",function(object) {
  dims = dim(object)
  message("A VectorSpaceModel object of ",dims[1]," words and ", dims[2], " vectors")
})

#' Plot a Vector Space Model.
#'
#' Visualizing a model as a whole is sort of undefined. I think the
#' sanest thing to do is reduce the full model down to two dimensions
#' using T-SNE, which preserves some of the local clusters.
#'
#' This plots only the first 300 words in the model.
#'
#' @param VectorSpaceModel
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
  text(m,rownames(short),cex = ((400:1)/200)^(1/3))
  rownames(m)=rownames(short)
  silent = m
})

#' Convert to a Vector Space Model
#'
#' @param matrix
#'
#' @return An object of class "VectorSpaceModel"
#' @export as.VectorSpaceModel
as.VectorSpaceModel = function(matrix) {
  return(new("VectorSpaceModel",matrix))
}

#' Read VectorSpaceModel
#'
#' Read a VectorSpaceModel from a file exported from word2vec or a similar output format.
#'
#' @param filename The file to read in.
#' @param vectors The number of dimensions word2vec calculated. Imputed automatically if not specified.
#' @param ... Further arguments passed to read.table. Word2vec produces
#' by default frequency sorted output. Therefore [limits nrow=500], for example,
#' will return the vectors to the top 500 words.
#' @export
#' @return An matrixlike object of class `VectorSpaceModel`
read.vectors <- function(filename,vectors=guess_n_cols(),
                         ...) {

  # Figure out how many dimensions.
  guess_n_cols = function() {
    # if cols is not defined
    test = read.table(filename,header=F,skip=1,
                       nrows=1,quote="",comment.char="")
  return(ncol(test)-1)
  }
  vectors_matrix = read.table(filename,header=F,skip=1,
                               colClasses = c("character",rep("numeric",vectors)),
                       quote="",comment.char="",...)
  names(vectors_matrix)[1] = "word"
  vectors_matrix$word[is.na(vectors_matrix$word)] = "NA"
  matrix = as.matrix(vectors_matrix[,colnames(vectors_matrix)!="word"])
  rownames(matrix) = vectors_matrix$word
  colnames(matrix) = paste0("V",1:vectors)
  return(new("VectorSpaceModel",matrix))
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
normalize_lengths =function(matrix) {
  t(t(matrix)/magnitudes(matrix))
}


#' Reduce by rownames
#'
#' @param matrix A matrix or VectorSpaceModel object
#' @param words A list of rownames or VectorSpaceModel names
#'
#' @return An object of the same class as matrix, consisting
#' of the rwos that match its rownames.
#' @export
filter_to_rownames <- function(matrix,words) {
  matrix[rownames(matrix) %in% words,]
}

#' Cosine Similarity
#'
#' @description Calculate the cosine similarity of two matrices or a matrix and a vector.
#'
#' @param x A matrix or VectorSpaceModel object
#' @param y A vector, matrix or VectorSpaceModel object.
#' Vectors are coerced to single-row matrices; y must have the
#' same number of dimensions as x.
#'
#'
#' @return A matrix with one row for each row of x. Columns and their names correspond to y.
#' @export
cosineSimilarity <- function(x,y){
  # The most straightforward definition would be just:
  #  x %*% t(y)      /     (sqrt(rowSums(x^2) %*% t(rowSums(y^2))))
  # However, we have to do a little type-checking and a few speedups.

  if (!(is.matrix(x) || is.matrix(y))) {
    stop("At least one input must be a matrix")
  }

  if (is.vector(x)) {
    x = as.matrix(x,ncol=ncol(y))
  }
  if (is.vector(y)) {
    x = as.matrix(y,ncol=ncol(x))
  }

  # Using tcrossprod should be faster than transposing manually.
  # Of course, this is still double-inefficient b/c we're calculating both
  # triangles of a symmetrical matrix, I think.
  tcrossprod(x,y)/
    (sqrt(tcrossprod(rowSums(x^2),rowSums(y^2))))

}

#' Cosine Distance
#' @description Calculate the cosine distance between two vectors.
#'
#' Not an actual distance metric, but can be used in similar ways.
#' It is calculated as simply the inverse of cosine similarity,
#' and falls in a fixed range of 0 to two.
#'
#' @param x
#' @param y
#'
#' @return A matrix whose dimnames are rownames(x), rownames(y)
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
#' each row of which is parallel to vector
#'
#' If the input is a matrix, the output will be a matrix: if a VectorSpaceModel,
#' it will be a VectorSpaceModel. Objects in the vector slot are coerced to vector class;
#' this means it is fine to use a single slice of a
#' @export
#'
project = function(matrix,vector) {
  # The matrix is a matrix:
  # b is a vector to reproject the matrix to be orthogonal to.
  # There's probably a more elegant way to do this than each row at a time, but it took me forever
  # to get this far.
  b = as.vector(vector)
  if (length(b)!=ncol(matrix)) {
    stop("The vector must be the same length as the matrix it is being compared to")
  }
  # The dot product of the projected matrix is a constant.
  bdotitself = b %*% b

  projected = crossprod(t(matrix %*% b)/as.vector((b %*% b)) , b)
  return(projected)
}

#' Return a vector rejection for each element in a VectorSpaceModel
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector A vector (or an object coercable to a vector, see project)
#' of the same length as the VectorSpaceModel.
#'
#' @return A new matrix or VectorSpaceModel of the same dimensions as `matrix`,
#' each row of which is orthogonal to `vector.`
#'
#' This is defined simply as `matrix-project(matrix,vector)`, but having a separate
#' name may make for cleaner code.
#'
#' See `project` for more details.
#'
#' @export
reject = function(matrix,vector) {
  # The projection of the matrix that _does not_ lie parallel to a given vector.
  val = matrix-project(matrix,vector)
#  if (class(matrix)=="VectorSpaceModel") {
#    return(new("VectorSpaceModel",val))
#  }
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
#' @export
nearest_to = function(matrix,vector,n=10) {
  dists = cosineDist(matrix,matrix(as.vector(vector),ncol=ncol(matrix)))
  dists_vector = c(dists)
  names(dists_vector) = rownames(matrix)
  sort(dists_vector)[1:n]
}

