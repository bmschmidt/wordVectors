context("Name collapsing")

test_that("name substitution works",
  expect_equivalent(
    demo_vectors %>% closest_to(~"good")
    ,
    demo_vectors %>% closest_to(demo_vectors[["good"]])
  )
)

test_that("character substitution works",
          expect_equivalent(
            demo_vectors %>% closest_to("good")
            ,
            demo_vectors %>% closest_to(demo_vectors[["good"]])
          )
)

test_that("addition works in substitutions",
          expect_equivalent(
            demo_vectors %>% closest_to(~ "good" + "bad")
            ,
            demo_vectors %>% closest_to(demo_vectors[["good"]] + demo_vectors[["bad"]])
          )
)

test_that("addition provides correct results",
          expect_gt(
            demo_vectors[["good"]] %>% cosineSimilarity(demo_vectors[["good"]] + demo_vectors[["bad"]])
            ,
            .8))

test_that("single-argument negation works",
          expect_equivalent(
            demo_vectors %>% closest_to(~ -("good"-"bad"))
            ,
            demo_vectors %>% closest_to(~ "bad"-"good")

          ))

test_that("closest_to can wrap in function",
          expect_equal(
            {function(x) {closest_to(x,~ "class" + "school")}}(demo_vectors),
            closest_to(demo_vectors,~ "class" + "school")
          )
)

test_that("Name substitution is occurring",
  expect_equivalent(
      cosineSimilarity(demo_vectors,"good"),
      cosineSimilarity(demo_vectors,demo_vectors[["good"]])
    ))

test_that("reference in functional scope is passed along",
          expect_equivalent(
            lapply(c("good"),function(referenced_word)
              {demo_vectors %>% closest_to(demo_vectors[[referenced_word]])})[[1]],
            demo_vectors %>% closest_to("good")
         )
)
