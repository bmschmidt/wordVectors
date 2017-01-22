context("VectorSpaceModel Linear Algebra is sensible")
library(dplyr)

test_that("Vectors are near to themselves",
          expect_lt(
            cosineDist(demo_vectors[1,],demo_vectors[1,]),
            1e-07
          )
)

test_that("Distance is between 0 and 2 (pt 1)",
          expect_gt(
            min(cosineDist(demo_vectors,demo_vectors)),
            -1e-07
          )
)

test_that("Distance is between 0 and 2 (pt 1)",
          expect_lt(
            max(cosineDist(demo_vectors,demo_vectors)),
            2 + 1e07)
          )