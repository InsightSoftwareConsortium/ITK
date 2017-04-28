/** \file free.c
 * \brief MINC 2.0 "free" functions
 ************************************************************************/
#include "minc2.h"
#include <stdlib.h>

/**
 * Free space allocated for string storage by a MINC function.
 * \param name_ptr A pointer to the space to be freed.
 */
int mifree_name(char *name_ptr)
{
  if (name_ptr == NULL) {
    return (MI_ERROR);
  }
  free(name_ptr);
  return (MI_NOERROR);
}

/**
 * Free list of names
 * not certain we really need this...
*/
int mifree_names(char **name_pptr)
{
  if (name_pptr == NULL) {
    return (MI_ERROR);
  }
  while (*name_pptr != NULL) {
    free(*name_pptr++);
  }
  return (MI_NOERROR);
}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
