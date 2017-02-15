#' Improve Vectorspace
#'
#'
#' @param vectorspace A VectorSpacemodel to be improved.
#' @param D The number of principal components to eliminate.
#'
#' @citation
#' @return A VectorSpaceModel object, transformed from the original.
#' @export
#'
#' @examples
improve_vectorspace = function(vectorspace,D=round(ncol(vectorspace)/100)) {
  mean = new("VectorSpaceModel",
             matrix(apply(vectorspace,2,mean),
                    ncol=ncol(vectorspace))
  )
  vectorspace = vectorspace-mean
  pca = prcomp(vectorspace)

  # I don't totally understand the recommended operation in the source paper, but this seems to do much
  # the same thing uses the internal functions of the package to reject the top i dimensions one at a time.
  drop_top_i = function(vspace,i) {
    if (i<=0) {vspace} else if (i==1) {
      vspace %>% reject(pca$rotation[,i])
      }
    else {
      vspace %>% reject(pca$rotation[,i]) %>% drop_top_i(i-1)
    }
  }
  better = vectorspace %>% drop_top_i(D)
}


#' Internal function to subsitute strings for a tree. Allows arithmetic on words.
#'
#' @noRd
#'
#' @param tree an expression from a formula
#' @param context the VSM context in which to parse it.
#'
#' @return a tree
sub_out_tree = function(tree, context) {
  # This is a whitelist of operators that I think are basic for vector math.
  # It's possible it could be expanded.

  # This might fail if you try to pass a reference to a basic
  # arithmetic operator, or something crazy like that.

  if (deparse(tree[[1]]) %in% c("+","*","-","/","^","log","sqrt","(")) {
    for (i in 2:length(tree)) {
      tree[[i]] <- sub_out_tree(tree[[i]],context)
    }
  }
  if (is.character(tree)) {
    return(context[[tree]])
  }
  return(tree)
}

#' Internal function to wrap for sub_out_tree. Allows arithmetic on words.
#'
#' @noRd
#'
#' @param formula A formula; string arithmetic on the LHS, no RHS.
#' @param context the VSM context in which to parse it.
#'
#' @return an evaluated formula.

sub_out_formula = function(formula,context) {
  # Despite the name, this will work on something that
  # isn't a formula. That's by design: we want to allow
  # basic reference passing, and also to allow simple access
  # to words.

  if (class(context) != "VectorSpaceModel") {return(formula)}
  if (class(formula)=="formula") {
    formula[[2]] <- sub_out_tree(formula[[2]],context)
    return(eval(formula[[2]]))
  }
  if (is.character(formula)) {return(context[[formula]])}
  return(formula)
}

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
#' @param ... Other arguments passed to extract (unlikely to be useful).
#'
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
      return (methods::new("VectorSpaceModel",e1@.Data-e2@.Data))
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
  methods::show(unclass(object[1:min(nrow(object),10),1:min(ncol(object),6),drop=F]))
})

#' Plot a Vector Space Model.
#'
#' Visualizing a model as a whole is sort of undefined. I think the
#' sanest thing to do is reduce the full model down to two dimensions
#' using T-SNE, which preserves some of the local clusters.
#'
#' For individual subsections, it can make sense to do a principal components
#' plot of the space of just those letters. This is what happens if method
#' is pca. On the full vocab, it's kind of a mess.
#'
#' This plots only the first 300 words in the model.
#'
#' @param x The model to plot
#' @param method The method to use for plotting. "pca" is principal components, "tsne" is t-sne
#' @param ... Further arguments passed to the plotting method.
#'
#' @return The TSNE model (silently.)
#' @export
setMethod("plot","VectorSpaceModel",function(x,method="tsne",...) {
  if (method=="tsne") {
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
  } else if (method=="pca") {
    vectors = predict(prcomp(x))[,1:2]
    plot(vectors,type='n')
    text(vectors,labels=rownames(vectors))
  }
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
#' @param binary Read in the binary word2vec form. (Wraps `read.binary.vectors`) By default, function
#' guesses based on file suffix.
#' @param ... Further arguments passed to read.table or read.binary.vectors.
#' Note that both accept 'nrows' as an argument. Word2vec produces
#' by default frequency sorted output. Therefore 'read.vectors("file.bin", nrows=500)', for example,
#' will return the vectors for the top 500 words. This can be useful on machines with limited
#' memory.
#' @export
#' @return An matrixlike object of class `VectorSpaceModel`
#'
read.vectors <- function(filename,vectors=guess_n_cols(),binary=NULL,...) {
  if(rev(strsplit(filename,"\\.")[[1]])[1] =="bin" && is.null(binary)) {
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
#' the whole matrix into memory. This limit is applied BEFORE `name_list` and
#' `name_regexp`.
#' @param cols The column numbers to read. Default is "All";
#' if you are in a memory-limited environment,
#' you can limit the number of columns you read in by giving a vector of column integers
#' @param rowname_list A whitelist of words. If you wish to read in only a few dozen words,
#' all other rows will be skipped and only these read in.
#' @param rowname_regexp A regular expression specifying a pattern for rows to read in. Row
#' names matching that pattern will be included in the read; all others will be skipped.
#' @return A VectorSpaceModel object
#' @export

read.binary.vectors = function(filename,nrows=Inf,cols="All", rowname_list = NULL, rowname_regexp = NULL) {
  if (!is.null(rowname_list) && !is.null(rowname_regexp)) {stop("Specify a whitelist of names or a regular expression to be applied to all input, not both.")}
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
  if (is.numeric(cols)) {
    returned_columns = length(cols)
  }

  read_row = function(i) {
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
    if (is.numeric(cols)) {
      return(row[cols])
    }
    return(row)
  }

  # When the size is fixed, it's faster to do as a vapply than as a for loop.
  if (is.null(rowname_list) && is.null(rowname_regexp)) {
    matrix = t(
      vapply(1:rows,read_row,as.array(rep(0,returned_columns)))
    )
  } else {
    elements = list()
    mynames = c()
    for (i in 1:rows) {
      row = read_row(i)
      if (!is.null(rowname_list)) {
        if (rownames[i] %in% rowname_list) {
          elements[[rownames[i]]] = row
        }
      }
      if (!is.null(rowname_regexp)) {
        if (grepl(pattern = rowname_regexp, x = rownames[i])) {
          elements[[rownames[i]]] = row
        }
      }
    }
    matrix = t(simplify2array(elements))
    rownames = names(elements)

  }
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

  val = matrix/magnitudes(matrix)
  if (inherits(val,"VectorSpaceModel")) {
    val@.cache = new.env()
  }
  val
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
#'
#' # Inspect the similarity of several academic disciplines by hand.
#' subjects = demo_vectors[[c("history","literature","biology","math","stats"),average=FALSE]]
#' similarities = cosineSimilarity(subjects,subjects)
#'
#' # Use 'nearest_to' to build up a large list of similar words to a seed set.
#' subjects = demo_vectors[[c("history","literature","biology","math","stats"),average=TRUE]]
#' new_subject_list = nearest_to(demo_vectors,subjects,20)
#' new_subjects = demo_vectors[[new_subject_list$word,average=FALSE]]
#'
#' # Plot the cosineDistance of these as a dendrogram.
#' plot(hclust(as.dist(cosineDist(new_subjects,new_subjects))))
#'
#' @export

cosineSimilarity <- function(x,y) {
  # The most straightforward definition would be just:
  #  x %*% t(y)      /     (sqrt(rowSums(x^2) %*% t(rowSums(y^2))))
  # However, we do a little type-checking and a few speedups.

  # Allow non-referenced characters to refer to the original matrix.
  y = sub_out_formula(y,x)

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
#' @return A matrix whose dimnames are rownames(x), rownames(y) and whose entires are
#' the associated distance.
#'
#' @export
cosineDist <- function(x,y) {
  1-(cosineSimilarity(x,y))
}

#' Project each row of an input matrix along a vector.
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector A vector (or object coercable to a vector)
#' of the same length as the VectorSpaceModel.
#'
#'
#' @description As with 'cosineSimilarity
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
  vector = sub_out_formula(vector,matrix)
  b = as.vector(vector)
  if (length(b)!=ncol(matrix)) {
    stop("The vector must be the same length as the matrix it is being compared to")
  }
  newmat = crossprod(t(matrix %*% b)/as.vector((b %*% b)) , b)
  return(new("VectorSpaceModel",newmat))
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
#'
#' genderless = reject(demo_vectors,demo_vectors[["he"]] - demo_vectors[["she"]])
#' nearest_to(genderless,genderless[["man"]])
#'
#' @export
reject = function(matrix,vector) {
  # The projection of the matrix that _does not_ lie parallel to a given vector.
  val = matrix-project(matrix,vector)
  return(val)
}


#' Compress or expand a vector space model along a vector.
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector A vector (or an object coercable to a vector, see project)
#' of the same length as the VectorSpaceModel.
#' @param multiplier A scaling factor. See below.
#'
#' @description This is an experimental function that might be useful sometimes.
#' 'Reject' flatly eliminates a particular dimension from a vectorspace, essentially
#' squashing out a single dimension; 'distend' gives finer grained control, making it
#' possible to stretch out or compress in the same space. High values of 'multiplier'
#' make a given vector more prominent; 1 keeps the original matrix untransformed; values
#' less than one compress distances along the vector; and 0 is the same as "reject,"
#' eliminating a vector entirely. Values less than zero will do some type of mirror-image
#' universe thing, but probably aren't useful?
#'
#'
#' @return A new matrix or VectorSpaceModel of the same dimensions as `matrix`,
#' distended along the vector 'vector' by factor 'multiplier'.
#'
#' See `project` for more details and usage.
#'
#' @examples
#' nearest_to(demo_vectors,"sweet")
#'
#' # Stretch out the vectorspace 4x longer along the gender direction.
#' more_sexist = distend(demo_vectors, ~ "man" + "he" - "she" -"woman", 4)
#'
#' nearest_to(more_sexist,"sweet")
#'
#' @export
distend = function(matrix,vector, multiplier) {
  parallel_track = project(matrix,vector)
  return(new("VectorSpaceModel",matrix - parallel_track*(multiplier-1)))
}

#' Return the n closest words in a VectorSpaceModel to a given vector.
#'
#' @param matrix A matrix or VectorSpaceModel
#' @param vector  A vector (or a string or a formula coercable to a vector)
#' of the same length as the VectorSpaceModel. See below.
#' @param n The number of closest words to include.
#' @param as_df Return as a data.frame? If false, returns a named vector, for back-compatibility.
#' @param fancy_names If true (the default) the data frame will have descriptive names like
#' 'similarity to "king+queen-man"'; otherwise, just 'similarity.' The default can speed up
#'  interactive exploration.
#'
#' @return A sorted data.frame with columns for the words and their similarity
#' to the target vector. (Or, if as_df==FALSE, a named vector of similarities.)
#'
#' @description This is a convenience wrapper around the most common use of
#' 'cosineSimilarity'; the listing of several words similar to a given vector.
#' Unlike cosineSimilarity, it returns a data.frame object instead of a matrix.
#' cosineSimilarity is more powerful, because it can compare two matrices to
#' each other; nearest_to can only take a vector or vectorlike object as its second argument.
#' But with (or without) the argument n=Inf, nearest_to is often better for
#' plugging directly into a plot.
#'
#' As with cosineSimilarity, the second argument can take several forms. If it's a vector or
#' matrix slice, it will be taken literally. If it's a character string, it will
#' be interepreted as a word and the associated vector from `matrix` will be used. If
#' a formula, any strings in the formula will be converted to rows in the associated `matrix`
#' before any math happens.
#'
#' @examples
#'
#' # Synonyms and similar words
#' nearest_to(demo_vectors,demo_vectors[["good"]])
#'
#' # If 'matrix' is a VectorSpaceModel object,
#' # you can also just enter a string directly, and
#' # it will be evaluated in the context of the passed matrix.
#'
#' nearest_to(demo_vectors,"good")
#'
#' # You can also express more complicated formulas.
#'
#' nearest_to(demo_vectors,"good")
#'
#' # Something close to the classic king:man::queen:woman;
#' # What's the equivalent word for a female teacher that "guy" is for
#' # a male one?
#'
#' nearest_to(demo_vectors,~ "guy" - "man" + "woman")
#'
#' @export
nearest_to = function(matrix, vector, n=10, as_df = TRUE, fancy_names = TRUE) {
  label = deparse(substitute(vector),width.cutoff=500)
  if (substr(label,1,1)=="~") {label = substr(label,2,500)}

  # The actually wrapping.
  sims = cosineSimilarity(matrix,vector)

  # Top n shouldn't be greater than the vocab length.
  n = min(n,length(sims))

  # For sorting.
  ords = order(-sims[,1])

  if (!as_df) {
    structure(
      1-sims[ords[1:n]], # Convert from similarity to distance.
      names=rownames(sims)[ords[1:n]])
  } else {
    return_val = data.frame(rownames(sims)[ords[1:n]], sims[ords[1:n]],stringsAsFactors=FALSE)
    if (fancy_names) {
      names(return_val) = c("word", paste("similarity to", label))
    } else {
      names(return_val) = c("word","similarity")
    }
    rownames(return_val) = NULL
    return_val
  }
}


