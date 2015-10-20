
#include "distance.h"

void CWrapper_distance(char **file_name, char **word, char **returnw, double *returnd)
{
    distance(*file_name, *word, *returnw, returnd);
}

