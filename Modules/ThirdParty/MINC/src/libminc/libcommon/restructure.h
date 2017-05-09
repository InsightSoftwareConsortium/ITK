/*
 * \file restructure.h
 * \brief Just declares the prototype of restructure_array().
 */
#ifndef MINC_RESTRUCTURE_H
#define MINC_RESTRUCTURE_H

/** Reorganize data in a multidimensional array "in place".
 *  Uses temporary buffer of nelem/8 bytes
 */
void restructure_array(size_t ndims,
                      unsigned char *array, 
                      const size_t *lengths_perm,
                      size_t el_size,
                      const int *map,
                      const int *dir);

#endif /*MINC_RESTRUCTURE_H*/