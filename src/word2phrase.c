//  Copyright 2013 Google Inc. All Rights Reserved.
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
#include "R.h"
#include "Rmath.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
//#include <pthread.h>

#define MAX_STRING1 60

const int vocab_hash1_size1 = 500000000; // Maximum 500M entries in the vocabulary

typedef float real;                    // Precision of float numbers

struct vocab_word1 {
  long long cn;
  char *word;
};

char train_file1[MAX_STRING1], output_file1[MAX_STRING1];
struct vocab_word1 *vocab;
int debug_mode1 = 2, min_count1 = 5, *vocab_hash1, min_reduce1 = 1;
long long vocab_max_size1 = 10000, vocab_size1 = 0;
long long train_words1 = 0;
real threshold1 = 100;

unsigned long long next_random = 1;

// Reads a single word from a file, assuming space + tab + EOL to be word boundaries
void ReadWord1(char *word, FILE *fin) {
  int a = 0, ch;
  while (!feof(fin)) {
    ch = fgetc(fin);
    if (ch == 13) continue;
    if ((ch == ' ') || (ch == '\t') || (ch == '\n')) {
      if (a > 0) {
        if (ch == '\n') ungetc(ch, fin);
        break;
      }
      if (ch == '\n') {
        strcpy(word, (char *)"</s>");
        return;
      } else continue;
    }
    word[a] = ch;
    a++;
    if (a >= MAX_STRING1 - 1) a--;   // Truncate too long words
  }
  word[a] = 0;
}

// Returns hash value of a word
int GetWordHash1(char *word) {
  unsigned long long a, hash = 1;
  for (a = 0; a < strlen(word); a++) hash = hash * 257 + word[a];
  hash = hash % vocab_hash1_size1;
  return hash;
}

// Returns position of a word in the vocabulary; if the word is not found, returns -1
int SearchVocab1(char *word) {
  unsigned int hash = GetWordHash1(word);
  while (1) {
    if (vocab_hash1[hash] == -1) return -1;
    if (!strcmp(word, vocab[vocab_hash1[hash]].word)) return vocab_hash1[hash];
    hash = (hash + 1) % vocab_hash1_size1;
  }
  return -1;
}

// Reads a word and returns its index in the vocabulary
int ReadWord1Index1(FILE *fin) {
  char word[MAX_STRING1];
  ReadWord1(word, fin);
  if (feof(fin)) return -1;
  return SearchVocab1(word);
}

// Adds a word to the vocabulary
int AddWordToVocab1(char *word) {
  unsigned int hash, length = strlen(word) + 1;
  if (length > MAX_STRING1) length = MAX_STRING1;
  vocab[vocab_size1].word = (char *)calloc(length, sizeof(char));
  strcpy(vocab[vocab_size1].word, word);
  vocab[vocab_size1].cn = 0;
  vocab_size1++;
  // Reallocate memory if needed
  if (vocab_size1 + 2 >= vocab_max_size1) {
    vocab_max_size1 += 10000;
    vocab=(struct vocab_word1 *)realloc(vocab, vocab_max_size1 * sizeof(struct vocab_word1));
  }
  hash = GetWordHash1(word);
  while (vocab_hash1[hash] != -1) hash = (hash + 1) % vocab_hash1_size1;
  vocab_hash1[hash]=vocab_size1 - 1;
  return vocab_size1 - 1;
}

// Used later for sorting by word counts
int VocabCompare1(const void *a, const void *b) {
    return ((struct vocab_word1 *)b)->cn - ((struct vocab_word1 *)a)->cn;
}

// Sorts the vocabulary by frequency using word counts
void SortVocab1() {
  int a;
  unsigned int hash;
  // Sort the vocabulary and keep </s> at the first position
  qsort(&vocab[1], vocab_size1 - 1, sizeof(struct vocab_word1), VocabCompare1);
  for (a = 0; a < vocab_hash1_size1; a++) vocab_hash1[a] = -1;
  for (a = 0; a < vocab_size1; a++) {
    // Words occuring less than min_count1 times will be discarded from the vocab
    if (vocab[a].cn < min_count1) {
      vocab_size1--;
      free(vocab[vocab_size1].word);
    } else {
      // Hash will be re-computed, as after the sorting it is not actual
      hash = GetWordHash1(vocab[a].word);
      while (vocab_hash1[hash] != -1) hash = (hash + 1) % vocab_hash1_size1;
      vocab_hash1[hash] = a;
    }
  }
  vocab = (struct vocab_word1 *)realloc(vocab, vocab_size1 * sizeof(struct vocab_word1));
}

// Reduces the vocabulary by removing infrequent tokens
void ReduceVocab1() {
  int a, b = 0;
  unsigned int hash;
  for (a = 0; a < vocab_size1; a++) if (vocab[a].cn > min_reduce1) {
    vocab[b].cn = vocab[a].cn;
    vocab[b].word = vocab[a].word;
    b++;
  } else free(vocab[a].word);
  vocab_size1 = b;
  for (a = 0; a < vocab_hash1_size1; a++) vocab_hash1[a] = -1;
  for (a = 0; a < vocab_size1; a++) {
    // Hash will be re-computed, as it is not actual
    hash = GetWordHash1(vocab[a].word);
    while (vocab_hash1[hash] != -1) hash = (hash + 1) % vocab_hash1_size1;
    vocab_hash1[hash] = a;
  }
  //fflush(stdout);
  min_reduce1++;
}

void LearnVocabFromTrainFile1() {
  char word[MAX_STRING1], last_word[MAX_STRING1], bigram_word[MAX_STRING1 * 2];
  FILE *fin;
  long long a, i, start = 1;
  for (a = 0; a < vocab_hash1_size1; a++) vocab_hash1[a] = -1;
  fin = fopen(train_file1, "rb");
  if (fin == NULL) {
    Rprintf("ERROR: training data file not found!\n");
    return;
  }
  vocab_size1 = 0;
  AddWordToVocab1((char *)"</s>");
  while (1) {
    ReadWord1(word, fin);
    if (feof(fin)) break;
    if (!strcmp(word, "</s>")) {
      start = 1;
      continue;
    } else start = 0;
    train_words1++;
    if ((debug_mode1 > 1) && (train_words1 % 100000 == 0)) {
      Rprintf("Words processed: %lldK     Vocab size: %lldK  %c", train_words1 / 1000, vocab_size1 / 1000, 13);
   //   fflush(stdout);
    }
    i = SearchVocab1(word);
    if (i == -1) {
      a = AddWordToVocab1(word);
      vocab[a].cn = 1;
    } else vocab[i].cn++;
    if (start) continue;
    sprintf(bigram_word, "%s_%s", last_word, word);
    bigram_word[MAX_STRING1 - 1] = 0;
    strcpy(last_word, word);
    i = SearchVocab1(bigram_word);
    if (i == -1) {
      a = AddWordToVocab1(bigram_word);
      vocab[a].cn = 1;
    } else vocab[i].cn++;
    if (vocab_size1 > vocab_hash1_size1 * 0.7) ReduceVocab1();
  }
  SortVocab1();
  if (debug_mode1 > 0) {
    Rprintf("\nVocab size (unigrams + bigrams): %lld\n", vocab_size1);
    Rprintf("Words in train file: %lld\n", train_words1);
  }
  fclose(fin);
}

void TrainModel1() {
  long long pa = 0, pb = 0, pab = 0, oov, i, li = -1, cn = 0;
  char word[MAX_STRING1], last_word[MAX_STRING1], bigram_word[MAX_STRING1 * 2];
  real score;
  FILE *fo, *fin;
  Rprintf("Starting training using file %s\n", train_file1);
  LearnVocabFromTrainFile1();
  fin = fopen(train_file1, "rb");
  fo = fopen(output_file1, "wb");
  word[0] = 0;
  while (1) {
    strcpy(last_word, word);
    ReadWord1(word, fin);
    if (feof(fin)) break;
    if (!strcmp(word, "</s>")) {
      fprintf(fo, "\n");
      continue;
    }
    cn++;
    if ((debug_mode1 > 1) && (cn % 100000 == 0)) {
      Rprintf("Words written: %lldK%c", cn / 1000, 13);
    //  fflush(stdout);
    }
    oov = 0;
    i = SearchVocab1(word);
    if (i == -1) oov = 1; else pb = vocab[i].cn;
    if (li == -1) oov = 1;
    li = i;
    sprintf(bigram_word, "%s_%s", last_word, word);
    bigram_word[MAX_STRING1 - 1] = 0;
    i = SearchVocab1(bigram_word);
    if (i == -1) oov = 1; else pab = vocab[i].cn;
    if (pa < min_count1) oov = 1;
    if (pb < min_count1) oov = 1;
    if (oov) score = 0; else score = (pab - min_count1) / (real)pa / (real)pb * (real)train_words1;
    if (score > threshold1) {
      fprintf(fo, "_%s", word);
      pb = 0;
    } else fprintf(fo, " %s", word);
    pa = pb;
  }
  fclose(fo);
  fclose(fin);
}


void word2phrase(char **rtrain_file,int *rdebug_mode,char **routput_file,int *rmin_count,double *rthreshold) {
/*  
if (argc == 1) {
    printf("WORD2PHRASE tool v0.1a\n\n");
    printf("Options:\n");
    printf("Parameters for training:\n");
    printf("\t-train <file>\n");
    printf("\t\tUse text data from <file> to train the model\n");
    printf("\t-output <file>\n");
    printf("\t\tUse <file> to save the resulting word vectors / word clusters / phrases\n");
    printf("\t-min-count <int>\n");
    printf("\t\tThis will discard words that appear less than <int> times; default is 5\n");
    printf("\t-threshold1 <float>\n");
    printf("\t\t The <float> value represents threshold1 for forming the phrases (higher means less phrases); default 100\n");
    printf("\t-debug <int>\n");
    printf("\t\tSet the debug mode (default = 2 = more info during training)\n");
    printf("\nExamples:\n");
    printf("./word2phrase -train text.txt -output phrases.txt -threshold1 100 -debug 2\n\n");
    return 0;
  }
*/
  if (*rtrain_file[0]!='0') strcpy(train_file1, *rtrain_file);
  if (rdebug_mode[0]!=0) debug_mode1 = rdebug_mode[0];
  if (*routput_file[0]!='0') strcpy(output_file1, *routput_file);
  if (rmin_count[0]!=0) min_count1 = rmin_count[0];
  if (rthreshold[0]!= 0) threshold1=rthreshold[0];
  vocab = (struct vocab_word1 *)calloc(vocab_max_size1, sizeof(struct vocab_word1));
  vocab_hash1 = (int *)calloc(vocab_hash1_size1, sizeof(int));
  TrainModel1();
 
}
