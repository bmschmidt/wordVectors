---
  title: 'WordVectors: an R environment for training and exploring word2vec modes'
  tags:
    - Natural Language Processing
    - Vector Space Models
    - word2vec
  authors:
   - name: Benjamin M Schmidt
     orcid: 0000-0002-1142-5720
     affiliation: 1
  affiliations:
   - name: Northeastern University
     index: 1
  date: 24 January 2017
  bibliography: paper.bib
  ---

  # Summary

  This is an R package for training and exploring word2vec models. It provides wrappers for the reference word2vec implementation released by Google to enable training of vectors from R.[@mikolov_efficient_2013] It also provides a variety of functions enabling exploratory data analysis of word2vec models in an R environment, including 1) functions for reading and writing word2vec's binary form, 2) standard linear algebra functions not bundled in base R (such as cosine similarity) with speed optimizations, and 3) a streamlined syntax for performing vector arithmetic in a vocabulary space.
  
  # References

