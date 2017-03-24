context("VectorSpaceModel Linear Algebra is sensible")

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
            2 + 1e-07)
          )


test_that("Distance is between 0 and 2 (pt 1)",
          expect_lt(
            max(abs(1-square_magnitudes(normalize_lengths(demo_vectors)))),
            1e-07)
)


test_that("Compatability maintained to v1.0",
          expect_null(dim(demo_vectors %>% nearest_to("good"))))


test_that("Single args passed to nearest_to produce two columns",
          expect_equal(2,ncol(demo_vectors %>% closest_to("good"))))

test_that("list args passed to nearest_to produce two columns",
          expect_equal(3,ncol(demo_vectors %>% closest_to(c("good","bad")))))


test_that("list args passed to nearest_to produce two columns",
          expect_lt(
            nrow(demo_vectors %>% closest_to(c("good","bad"),merge.method="all")),
            nrow(demo_vectors %>% closest_to(c("good","bad"),merge.method="any"))
          )
)

          