context("Read and Write works")

## TODO: Add tests for non-binary format; check actual value of results; test reading of slices.

test_that("Writing works",
          expect_null(
            write.binary.word2vec(demo_vectors[1:100,],"binary.bin"),
            1e-07
          )
)

test_that("Reading Works",
          expect_s4_class(
            read.binary.vectors("binary.bin"),
            "VectorSpaceModel"
          )
)

