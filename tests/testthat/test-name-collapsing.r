#source("sample-data.r")
context("Name collapsing")
library(dplyr)

test_that("name substitution works",
  expect_equal(
    demo_vectors %>% nearest_to("good"),
    demo_vectors %>% nearest_to(demo_vectors[["good"]])
  )
)

test_that("addition works in subsititutions",
          expect_equal(
            demo_vectors %>% nearest_to("good" + "bad"),
            demo_vectors %>% nearest_to(demo_vectors[["good"]] + demo_vectors[["bad"]])
          )
)


test_that("nearest_to can wrap in function",
          expect_equal(
            {function(x) {nearest_to(x,"class" + "school")}}(demo_vectors),
            nearest_to(demo_vectors,"class" + "school")
          )
)

test_that("Name substitution is occurring",
  expect_true(
    all_equal(
      cosineSimilarity(demo_vectors,"good"),
      cosineSimilarity(demo_vectors,demo_vectors[["good"]])
    )))

