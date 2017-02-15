

#' Extract a shared vector from a list of vectors.
#'
#' @param matrix
#' @param ... A list of vectors, words, or formulas to extract in the context of "matrix."
#'
#' @return A single vector representing the principal component of the various input vector.
#' @export
#'
#'
#'
#' @examples
#' gender_vector_from_pca <- demo_vectors %>% extract_underlying_vector(~"he"-"she",~"man"-"woman",~"himself"-"herself",~("mrs"+"ms")/2 - "mr")
#' demo_vectors %>% nearest_to(gender_vector_from_pca)
#'

extract_underlying_vector = function(matrix, ...) {
  pairs = list(...)
  vectors = lapply(pairs, function(formula) {
    wordVectors:::sub_out_formula(formula,matrix)
  }) %>% do.call(rbind,.)
  component = {vectors %>% prcomp}$rotation[,1] %>% matrix(nrow=1)
}


