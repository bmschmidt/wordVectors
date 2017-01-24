context("Training Functions Work")

source_dir = paste0(utils::getSrcDirectory(wordVectors::train_word2vec))
file.remove("/tmp/tmp.txt", showWarnings=FALSE)

test_that("Preparation produces file",
          expect_equal(
            prep_word2vec(source_dir,"/tmp/tmp.txt"),
            "/tmp/tmp.txt"
          )
)

test_that("Bundling works on multiple levels",
          expect_equal(
            prep_word2vec(source_dir,"/tmp/tmp.txt",bundle_ngrams = 3),
            "/tmp/tmp.txt"
          )
)

test_that("Training Works",
          expect_s4_class(
            train_word2vec("/tmp/tmp.txt"),
            "VectorSpaceModel"
          )
)

