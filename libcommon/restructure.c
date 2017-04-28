/** \file restructure.c
 * \brief In-place reordering of multidimensional arrays.
 * \author Bert Vincent
 *
 ************************************************************************/
#include <stdlib.h>
#include <memory.h>

#include "restructure.h"

/** In-place array dimension restructuring.
 *
 * Based on Chris H.Q. Ding, "An Optimal Index Reshuffle Algorithm for
 * Multidimensional Arrays and its Applications for Parallel Architectures"
 * IEEE Transactions on Parallel and Distributed Systems, Vol.12, No.3,
 * March 2001, pp.306-315.
 *
 * I rewrote the algorithm in "C" an generalized it to N dimensions.
 *
 * Guaranteed to do the minimum number of memory moves, but requires
 * that we allocate a bitmap of nelem/8 bytes.  The paper suggests
 * ways to eliminate the bitmap - I'll work on it.
 */

/**
 * Map a set of array coordinates to a linear offset in the array memory.
 */
static size_t
index_to_offset(size_t ndims,
                const size_t sizes[],
                const size_t index[])
{
  size_t offset = index[0];
  size_t i;

  for (i = 1; i < ndims; i++) {
    offset *= sizes[i];
    offset += index[i];
  }
  return (offset);
}

/**
 * Map a linear offset to a set of coordinates in a multidimensional array.
 */
static void
offset_to_index(size_t ndims,
                const size_t sizes[],
                size_t offset,
                size_t index[])
{
  size_t i;

  for (i = ndims - 1; i > 0; i--) {
    index[i] = offset % sizes[i];
    offset /= sizes[i];
  }
  index[0] = offset;
}

/* Trivial bitmap test & set.
 */
#define BIT_TST(bm, i) (bm[(i) / 8] & (1 << ((i) % 8)))
#define BIT_SET(bm, i) (bm[(i) / 8] |= (1 << ((i) % 8)))

#ifndef MAX_ARRAY_DIMS
#define MAX_ARRAY_DIMS 1000
#endif

/** The main restructuring code. This code will reorganize data in
 * a multidimensional array "in place".
 */
void restructure_array(size_t ndims,    /* Dimension count */
                       unsigned char *array, /* Raw data */
                       const size_t *lengths_perm, /* Permuted lengths */
                       size_t el_size,  /* Element size, in bytes */
                       const int *map, /* Mapping array */
                       const int *dir) /* Direction array, in permuted order */
{
  size_t index[MAX_ARRAY_DIMS];      /* Raw indices */
  size_t index_perm[MAX_ARRAY_DIMS]; /* Permuted indices */
  size_t lengths[MAX_ARRAY_DIMS];    /* Raw (unpermuted) lengths */
  unsigned char *temp;
  size_t offset_start;
  size_t offset_next;
  size_t offset;
  unsigned char *bitmap;
  size_t total;
  size_t i;

  if ((temp = malloc(el_size)) == NULL) {
    return;
  }

  /**
   * Permute the lengths from their "output" configuration back into
   * their "raw" or native order:
   **/
  for (i = 0; i < ndims; i++) {
    lengths[map[i]] = lengths_perm[i];
  }

  /**
   * Calculate the total size of the array, in elements.
   **/
  total = 1;
  for (i = 0; i < ndims; i++) {
    total *= lengths[i];
  }

  /**
   * Allocate a bitmap with enough space to hold one bit for each
   * element in the array.
   **/
  bitmap = calloc((total + 8 - 1) / 8, 1); /* bit array */
  if (bitmap == NULL) {
    free(temp);
    return;
  }

  for (offset_start = 0; offset_start < total; offset_start++) {

    /**
     * Look for an unset bit - that's where we start the next
     * cycle.
     **/

    if (!BIT_TST(bitmap, offset_start)) {

      /**
       * Found a cycle we have not yet performed.
       **/

      offset_next = -1;   /* Initialize. */

#ifdef _DEBUG
      printf("%ld", offset_start);
#endif /* DEBUG */

      /**
       * Save the first element in this cycle.
       **/

      memcpy(temp, array + (offset_start * el_size), el_size);

      /**
       * We've touched this location.
       **/

      BIT_SET(bitmap, offset_start);

      offset = offset_start;

      /**
       * Do until the cycle repeats.
       **/

      while (offset_next != offset_start) {

        /**
         * Compute the index from the offset and permuted length.
         **/

        offset_to_index(ndims, lengths_perm, offset, index_perm);

        /**
         * Permute the index into the alternate arrangement.
         **/

        for (i = 0; i < ndims; i++) {
          if (dir[i] < 0) {
            index[map[i]] = lengths[map[i]] - index_perm[i] - 1;
          } else {
            index[map[i]] = index_perm[i];
          }
        }

        /**
         * Calculate the next offset from the permuted index.
         **/

        offset_next = index_to_offset(ndims, lengths, index);
#ifdef DEBUG
        if (offset_next >= total) {
          printf("Fatal - offset %ld out of bounds!\n", offset_next);
          printf("lengths %lld,%lld,%lld\n",
                 lengths[0],lengths[1],lengths[2]);
          printf("index %lld,%lld,%lld\n",
                 index_perm[0], index_perm[0], index_perm[2]);
          //TODO: report MEMORY error somehow
          exit(-1);
        }
#endif
        /**
         * If we are not at the end of the cycle...
         **/

        if (offset_next != offset_start) {

          /**
           * Note that we've touched a new location.
           **/

          BIT_SET(bitmap, offset_next);

#ifdef _DEBUG
          printf(" - %ld", offset_next);
#endif /* DEBUG */

          /**
           * Move from old to new location.
           **/

          memcpy(array + (offset * el_size),
                 array + (offset_next * el_size),
                 el_size);

          /**
           * Advance offset to the next location in the cycle.
           **/

          offset = offset_next;
        }
      }

      /**
       * Store the first value in the cycle, which we saved in
       * 'tmp', into the last offset in the cycle.
       **/

      memcpy(array + (offset * el_size), temp, el_size);

#ifdef _DEBUG
      printf("\n");
#endif /* DEBUG */
    }
  }

  free(bitmap);               /* Get rid of the bitmap. */
  free(temp);
}

