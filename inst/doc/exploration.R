## ------------------------------------------------------------------------
library(wordVectors)
library(magrittr)

## ------------------------------------------------------------------------
demo_vectors[["good"]]

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(demo_vectors[["good"]])

## ------------------------------------------------------------------------
demo_vectors %>% closest_to("bad")

## ------------------------------------------------------------------------

demo_vectors %>% closest_to(~"good"+"bad")

# The same thing could be written as:
# demo_vectors %>% closest_to(demo_vectors[["good"]]+demo_vectors[["bad"]])

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(~"good" - "bad")

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(~ "bad" - "good")

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(~ "he" - "she")
demo_vectors %>% closest_to(~ "she" - "he")

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(~ "guy" - "he" + "she")

## ------------------------------------------------------------------------
demo_vectors %>% closest_to(~ "guy" + ("she" - "he"))

## ------------------------------------------------------------------------

demo_vectors[[c("lady","woman","man","he","she","guy","man"), average=F]] %>% 
  plot(method="pca")


## ------------------------------------------------------------------------
top_evaluative_words = demo_vectors %>% 
   closest_to(~ "good"+"bad",n=75)

goodness = demo_vectors %>% 
  closest_to(~ "good"-"bad",n=Inf) 

femininity = demo_vectors %>% 
  closest_to(~ "she" - "he", n=Inf)

## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

top_evaluative_words %>%
  inner_join(goodness) %>%
  inner_join(femininity) %>%
  ggplot() + 
  geom_text(aes(x=`similarity to "she" - "he"`,
                y=`similarity to "good" - "bad"`,
                label=word))

