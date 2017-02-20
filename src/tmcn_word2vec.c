#include "R.h"
#include "Rmath.h"
#include "word2vec.h"

void tmcn_word2vec(char *train_file0, char *output_file0,
                   char *binary0, char *dims0, char *threads,
                   char *window0, char *classes0, char *cbow0,
                   char *min_count0, char *iter0, char *neg_samples0)
{
	int i;
  layer1_size = atoll(dims0);
  num_threads = atoi(threads);
  window=atoi(window0);
	binary = atoi(binary0);
	classes = atoi(classes0);
	cbow = atoi(cbow0);
	min_count = atoi(min_count0);
	iter = atoll(iter0);
	negative = atoi(neg_samples0);
	strcpy(train_file, train_file0);
	strcpy(output_file, output_file0);


	alpha = 0.025;
	starting_alpha = alpha;
	word_count_actual = 0;

	vocab = (struct vocab_word *)calloc(vocab_max_size, sizeof(struct vocab_word));
	vocab_hash = (int *)calloc(vocab_hash_size, sizeof(int));
	expTable = (real *)malloc((EXP_TABLE_SIZE + 1) * sizeof(real));
	for (i = 0; i < EXP_TABLE_SIZE; i++) {
    	expTable[i] = exp((i / (real)EXP_TABLE_SIZE * 2 - 1) * MAX_EXP); // Precompute the exp() table
		expTable[i] = expTable[i] / (expTable[i] + 1);                   // Precompute f(x) = x / (x + 1)
	}
	TrainModel();
}


void CWrapper_word2vec(char **train_file, char **output_file,
                       char **binary, char **dims, char **threads,
                       char **window, char **classes, char **cbow, char **min_count, char **iter, char **neg_samples)
{
    tmcn_word2vec(*train_file, *output_file, *binary, *dims, *threads,*window,*classes,*cbow,*min_count,*iter, *neg_samples);
}

