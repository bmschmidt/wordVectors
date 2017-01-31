context("Rejection Works")

test_that("Rejection works along gender binary",
          expect_gt(
            {
              rejected_frame <- demo_vectors %>% reject(~ "man" - "woman")
              cosineDist(demo_vectors[["he"]],demo_vectors[["she"]] ) -
                cosineDist(rejected_frame[["he"]],rejected_frame[["she"]] )
            },
            .4
          )
)
