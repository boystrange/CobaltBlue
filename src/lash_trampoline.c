
#include <stdio.h>

#include "lash.h"
#include "lash-auto.h"
#include "lash-diag.h"
#include "lash-ndd.h"
#include "auto-io-print.h"
#include "auto-io-dots.h"

uint4 my_ndd_dimension(ndd* ptr)
{
  return ptr->dim;
}

void my_ndd_print_automaton(ndd* ptr)
{
  auto_print(ptr->automaton);
}

void my_ndd_automaton_to_dot(ndd* ptr, char* file_name)
{
  auto_serialize_write_dot_file(ptr->automaton, file_name, LASH_EXP_DIGIT);
}
 
int my_ndd_automaton_size_of(ndd* ptr)
{
  return auto_nb_states(ptr->automaton);
}
