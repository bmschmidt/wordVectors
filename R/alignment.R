#' Align two matrices with orthogonal procrustes
#'
#' @description Orthogonal procrustes determines a matrix rotation that
#' best positions points from one arbitrary vectorspace into another. This
#' function uses rownames to find which rows should be aligned with each other.
#' Note that this may produce undesired results if the rownames are not truly
#' the same words; for example, if the models were changed with different tokenization
#' schemes or rules for ngram bundling.
#'
#' William L. Hamilton, Jure Leskovec, and Dan Jurafsky.
#' 2016. Diachronic Word Embeddings Reveal Statistical Laws of Semantic Change.
#' Proceedings of ACL.
#'
#' @param x A reference matrix or VectorSpaceModel.
#' @param y The matrix or VectorSpaceModel to be rotated.
#'
#' @return A matrix of the same shape as y, projected into the space of x.
#'
#' @examples
#'
#' # Create a new model from the reference with gender rejected out.
#' genderless <- demo_vectors %>% reject(~"man" - "woman") %>% reject(~"he" - "she")
#'
#' # The vector for "she" in the new space is only somewhat similar to the old one.
#' demo_vectors[["she"]] %>% cosineSimilarity(genderless[["she"]])
#'
#' # We can re-align the genderless vectorspace into the original one.
#' adjusted <- demo_vectors %>% procrustes(genderless)
#'
#' # Now the cosine similarity is ever-so-slightly higher.
#' demo_vectors[["she"]] %>% cosineSimilarity(adjusted[["she"]])
procrustes = function(x, y) {
  # The relevant parts pulled from the vegan `procrustes` function.

  reorder_rows = function(matrix,lookup=shared_vocab) matrix[match(lookup,rownames(matrix)),]

  # A few extra copies produces shorter code.
  shared_vocab = intersect(rownames(x),rownames(y))
  x_ = reorder_rows(x,shared_vocab)
  y_ = reorder_rows(y,shared_vocab)

  XY = crossprod(x_,y_)

  sol = svd(XY)
  A = sol$v %*% t(sol$u)
  yRot <- y %*% A

  if (inherits(y, "VectorSpaceModel")) {
    new("VectorSpaceModel",yRot)
    } else {yRot}
}


# This is a rather complicated function to make a slopegraph: I may bundle it into the core package at some point.
usage_slopegraph = function(vector, ..., n=10, label_function = function(...) ggrepel::geom_text_repel(..., direction="y")) {
  sets = list(...)
  names = eval(substitute(alist(...))) %>% purrr::map(deparse) %>% unlist
  names(sets) = paste("set", 1:length(sets))
  groups = lapply(1:length(sets), function(name) {
    set = sets[[name]]
    neighbors = nearest_to(set,vector,fancy_names=F,n=Inf)
    neighbors$rank = rank(-neighbors$similarity)
    names(neighbors) = c("word",name,paste0("rank-",name))
    neighbors %>% as.tbl
  }) %>% purrr::reduce(inner_join)

  in_top_n = groups %>% select(starts_with("rank-")) %>% apply(1,min) <= n
  groups = groups %>% mutate(lab1 = paste0(`rank-1`, ". ", word))
  groups = groups %>% mutate(lab2 = paste0(`rank-2`, ". ", word))

  #label_function = geom_text
  #if (requireNamespace("ggrepel")) label_function = function(...) ggrepel::geom_text_repel(..., direction="y")
  groups[in_top_n,] %>% filter(`1`< .999 & `2` < .999) %>% ggplot() +
    label_function(aes(y=`1`,label = lab1),hjust=1,x=1,alpha=.5,nudge_x=-.1) +
    label_function(aes(y=`2`,label = lab2),hjust=0,x=2,alpha=.5,nudge_x=.1) +
    geom_segment(aes(x=1,xend=2,y=`1`, yend=`2`)) +
    scale_x_continuous(limits=c(0,3)) +
    scale_y_continuous(paste0("Cosine similarity to ",deparse(vector)))+
    theme_bw() +
    labs (title=paste0("Similarity to '", deparse(vector),"' in the\nvectorspaces ", names[1], " (left) and\n", names[2], " (right)"))
}



stanford_plot = function(model_set, word, n=20, transform_type = c("mds","pca")) {

  if (!requireNamespace("ggplot2")) {stop("You need ggplot for this function: run 'install.packages(ggplot2)'")}
  model_set = Filter(function(model) {length(intersect(word,rownames(model))) > 0}, model_set)
  small_list = lapply(model_set, function(model) {
    model %>% nearest_to(model[[word]],n,fancy_names=F)
  }) %>% bind_rows %>% select(word) %>% unlist %>% unique

  just_this_word = lapply(model_set, function(mat) {
    mat[rownames(mat) %in% word,,drop=F]
  })
  for (i in 1:length(just_this_word)) {
    this_name = names(model_set)[i]
    if (is.null(this_name)) {this_name = i}
    rownames(just_this_word[[i]]) = paste(rownames(just_this_word[[i]]),this_name,sep="\n")
  }

  just_this_word = just_this_word %>% do.call(rbind,.) %>% new("VectorSpaceModel",.)


  minify = function(matrix,lookup) matrix[match(lookup,rownames(matrix)),]

  reduced = lapply(model_set,function(mat) {
    mat %>% minify(small_list[!small_list %in% word])
  }) %>% abind::abind(along=3) %>%
    apply(c(1,2),function(point) {mean(point,na.rm=T)}) %>% as.matrix
  rownames(reduced) = small_list[!small_list %in% word]

  if (transform_type=="mds") {
    both = rbind(just_this_word,reduced)
    neighbors = cmdscale(cosineDist(both,both))
    newness = tibble::as_data_frame(neighbors) %>%
      dplyr::transmute(MDS1=V1,MDS2=V2,word=rownames(neighbors),type=ifelse(grepl("\\n",word),"reference","neighbor"))
  } else if (transform_type=="pca") {
    rot = prcomp(reduced)$rotation[,1:2]
    neighbors = reduced %*% rot %>% as.data.frame
    reference = just_this_word %*% rot %>% as.data.frame
    neighbors$type = "neighbor"
    reference$type = "reference"
    newness = rbind(neighbors,reference)
    newness$word = rownames(newness)
  }

  newness$base_word = gsub("\\n.*","",newness$word)
  aesthetic = aes(x=PC1,y=PC2,label=word)
  if (transform_type=="mds") {
      aesthetic = aes(x=MDS1,y=MDS2,label=word)
  }


  ggplot2::ggplot(newness) + aesthetic +
    geom_text(data = newness[newness$type=="neighbor",],alpha=.3) +
    geom_text(data = newness[newness$type=="reference",], color="black") +
    geom_path(data = newness[newness$type=="reference",],
              aes(group=base_word),col='red',arrow = grid::arrow())
}

#' Title Align models.
#'
#' @param ... A number of VectorSpaceModels to align as independent entries;
#' or, alternatively, a single list
#' of several vectorspace models to align.
#' @param shared_vocab_only Two models may have different words. If this variable is TRUE,
#' (the default), the *entire* vocabular will be returned.
#'
#' @return A list of VectorSpaceModel objects. Each is rotated into the space defined by the first model,
#' and altered so that each vector is of unit length.
#' @export
#'
#' @examples
align_models = function(..., shared_vocab_only = TRUE) {
  # shared_vocab_only (unimplemented) is whether to allow more complicated

  # ... are models to be aligned.
  # alternatively, it can be a single *list* of models.
  models = list(...)

  if (typeof(models[[1]]) == "list") {
    models = models[[1]]
  }

  for (i in 1:length(models)) {
    if (max(abs(1-square_magnitudes(models[[i]]))) > 1e-03) {
      warning("Model #", i, " is not normalized: scaling so each vector is unit length")
      models[[i]] <- normalize_lengths(models[[i]])
    }
  }

  minify = function(matrix, lookup) matrix[match(lookup,rownames(matrix)),]

  if (shared_vocab_only) {
    # Shrink them down to only shared vocabulary.
    shared_vocab = Reduce(intersect,lapply(models,rownames))
    models = lapply(models, minify, lookup = shared_vocab)
  }

  combined = Reduce(
    function(x,y) procrustes(x,y),
    models,
    accumulate = TRUE
    )
  # Keep the original names.
  names(combined) = names(models)
  combined
}

#' Read Group
#'
#' @param filenames A list of filenames naming VectorSpaceModels.
#' @param ... Additional parameters passed to read_vectors
#'
#' @return
#' @export
#'
#' @examples
read_group = function(filenames,...) {
  # Additional parameters passed to read.vectors
  models = lapply(filenames, function(filename) {
    message("Reading ",filename)
    read.vectors(filename,...) %>% normalize_lengths
  })
  names(models) = gsub(".*/|.bin|.txt","",filenames)
  models
}
