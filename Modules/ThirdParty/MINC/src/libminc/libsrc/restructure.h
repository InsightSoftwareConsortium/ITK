/*
 * \file restructure.h
 * \brief Just declares the prototype of restructure_array().
 */
extern void restructure_array(size_t ndims,
                              unsigned char *array, 
                              const size_t *lengths_perm,
                              size_t el_size,
                              const int *map,
                              const int *dir);

