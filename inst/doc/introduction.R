## ------------------------------------------------------------------------
if (!require(wordVectors)) {
  if (!(require(devtools))) {
    install.packages("devtools")
  }
  devtools::install_github("bmschmidt/wordVectors")
}



## ------------------------------------------------------------------------
library(wordVectors)
library(magrittr)

## ------------------------------------------------------------------------
if (!file.exists("cookbooks.zip")) {
  download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
}
unzip("cookbooks.zip",exdir="cookbooks")

## ------------------------------------------------------------------------
if (!file.exists("cookbooks.txt")) prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,bundle_ngrams=2)

## ------------------------------------------------------------------------
if (!file.exists("cookbook_vectors.bin")) {model = train_word2vec("cookbooks.txt","cookbook_vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("cookbook_vectors.bin")


## ------------------------------------------------------------------------
model %>% closest_to("fish")

## ------------------------------------------------------------------------
model %>% 
  closest_to(model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],50)

## ------------------------------------------------------------------------
some_fish = closest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],150)
fishy = model[[some_fish$word,average=F]]
plot(fishy,method="pca")

## ------------------------------------------------------------------------
set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)

## ------------------------------------------------------------------------
sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

## ------------------------------------------------------------------------
ingredients = c("madeira","beef","saucepan","carrots")
term_set = lapply(ingredients, 
       function(ingredient) {
          nearest_words = model %>% closest_to(model[[ingredient]],20)
          nearest_words$word
        }) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot


## ------------------------------------------------------------------------
tastes = model[[c("sweet","salty"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
sweet_and_saltiness = model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 20 sweet or salty.
sweet_and_saltiness = sweet_and_saltiness[
  rank(-sweet_and_saltiness[,1])<20 |
  rank(-sweet_and_saltiness[,2])<20,
  ]

plot(sweet_and_saltiness,type='n')
text(sweet_and_saltiness,labels=rownames(sweet_and_saltiness))


## ------------------------------------------------------------------------

tastes = model[[c("sweet","salty","savory","bitter","sour"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)

common_similarities_tastes[20:30,]

## ------------------------------------------------------------------------
high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]

high_similarities_to_tastes %>% 
  prcomp %>% 
  biplot(main="Fifty words in a\nprojection of flavor space")

## ------------------------------------------------------------------------
plot(model,perplexity=50)

