.is.word2vec <- function(obj) {
	if (!inherits(obj, "word2vec")) return(FALSE)
	if (!identical(class(obj$model_file), "character")) return(FALSE)
	if (!file.exists(obj$model_file)) return(FALSE)
	if (length(obj$model_file) > 1) return(FALSE)
	if (file.info(obj$model_file)$isdir) return(FALSE)
	return(TRUE)
}




