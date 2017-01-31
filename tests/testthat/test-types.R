context("VectorSpaceModel Class Works")

test_that("Class Exists",
          expect_s4_class(
            demo_vectors,
            "VectorSpaceModel"
          )
)

test_that("Class inherits addition",
          expect_s4_class(
            demo_vectors+1,
            "VectorSpaceModel"
          )
)

test_that("Class inherits slices",
          expect_s4_class(
            demo_vectors[1,],
            "VectorSpaceModel"
          )
)

test_that("Slices aren't dropped in dimensionality",
          expect_s4_class(
            demo_vectors[1,],
            "matrix"
          )
)
