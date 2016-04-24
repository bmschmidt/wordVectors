#' Vector Space Model class
#'
#' @description A class for describing and accessing Vector Space Models like Word2Vec.
#' The base object is simply a matrix with columns describing dimensions and unique rownames
#' as the names of vectors. This package gives a number of convenience functions for printing
#' and, most importantly, accessing these objects.
#' @return An object of class "VectorSpaceModel"
#' @exportClass VectorSpaceModel
setClass("VectorSpaceModel",representation("matrix"))


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
  x@.Data = x@.Data[i,j,drop=F]
  return(x)
  #methods::new("VectorSpaceModel",x@.Data[i,j,drop=F])
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
      return (methods::new("VectorSpaceModel",e1@.Data-e2@.Data))
    }
    if (nrow(e2)==1) {
      return(t(t(e1)-as.vector(e2)))
    }
    stop("Vector space model subtraction must use models of equal dimensions")
})

#' VectorSpaceModel subsetting
#'
#  @description Reduce a VectorSpaceModel to a single object.
#' @param VectorSpaceModel
#' @param i The words to use as rownames
#' @param average Whether to collapse down to a single vector,
#' or to return a subset of one row for each asked for.
#'
#' @return A VectorSpaceModel of a single row.
setMethod("[[","VectorSpaceModel",function(x,i,average=TRUE,...) {
  # The wordvec class can extract a row from the matrix
  # by accessing the rownames. x[["king"]] gives the row
  # for which the rowname is "king"; x[[c("king","queen")]] gives
  # the midpoint of x[["king"]] and x[["queen"]], which can occasionally
  # be useful.
  if(typeof(i)=="character")
  {
    matching_rows = x@.Data[rownames(x) %in% i,,drop=F]
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
  methods::show(object@.Data[1:min(nrow(object),10),1:min(ncol(object),6)])
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
#' @param matrix
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
    return(read.binary.vectors(filename))
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
#'
#' @return A word2vec object
#' @export
#'
#'

read.binary.vectors = function(filename,nrows=Inf) {
  a = file(filename,'rb')
  rows = ""
  mostRecent=""
  while(mostRecent!=" ") {
    mostRecent = readChar(a,1)
    rows = paste0(rows,mostRecent)
  }
  rows = as.integer(rows)

  cols = ""
  while(mostRecent!="\n") {
    mostRecent = readChar(a,1)
    cols = paste0(cols,mostRecent)
  }
  cols = as.integer(cols)

  if(nrows<rows) {
    rows = nrows
  }

  message(paste("Reading a word2vec binary file of",rows,"rows and",cols,"columns"))

  ## Read a row
  rownames = rep("",rows)

  # create progress bar
  pb <- utils::txtProgressBar(min = 0, max = rows, style = 3)


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
      row = readBin(a,numeric(),size=4,n=cols,endian="little")
      return(row)
    },as.array(rep(0,cols)))
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
normalize_lengths =function(matrix) {
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
#' cosineSimilarity(subjects,subjects)
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
#' each row of which is parallel to vector
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
  matrix@.Data = crossprod(t(matrix %*% b)/as.vector((b %*% b)) , b)
  return(matrix)
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
  dists = cosineDist(matrix,matrix(as.vector(vector),ncol=ncol(matrix)))
  dists_vector = c(dists)
  names(dists_vector) = rownames(matrix)
  sort(dists_vector)[1:n]
}

