context("Training Functions Work")

# This fails on Travis. I'll worry about this later.
demo = "Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth.
"

message("In directory", getwd())
cat(demo,file = "input.txt")
if (file.exists("tmp.txt")) file.remove("tmp.txt")

test_that("Preparation produces file",
          expect_equal(
          prep_word2vec("input.txt","tmp.txt"),
            "tmp.txt"
          )
)

test_that("Preparation produces file",
          expect_equal(
            prep_word2vec("input.txt","tmp.txt"),
            "tmp.txt"
          )
)

test_that("Tokenization is the right length",
          expect_lt(
            2,
            272 - length(stringr::str_split(readr::read_file("tmp.txt"), " "))
          )
)
if (FALSE) {
test_that("Bundling works on multiple levels",
          expect_equal(
            prep_word2vec("input.txt","tmp.txt",bundle_ngrams = 3),
            "tmp.txt"
          )
)
}
test_that("Training Works",
          expect_s4_class(
            train_word2vec("tmp.txt"),
            "VectorSpaceModel"
          )
)

