context("Read and Write works")
library(dplyr)


test_that("Writing works",
          expect_null(
            write.binary.word2vec(demo_vectors,"/tmp/binary.bin"),
            1e-07
          )
)

test_that("Reading Works",
          expect_s4_class(
            read.binary.vectors("/tmp/binary.bin"),
            "VectorSpaceModel"
          )
)

test_that("Distance is between 0 and 2 (pt 1)",
          expect_lt(
            max(cosineDist(demo_vectors,demo_vectors)),
            2 + 1e07)
)


