/*
  fsm@robots.ox.ac.uk
*/
#include "vnl_determinant.h"

template <class T>
T vnl_determinant(T const *row0, T const *row1) {
  T const *rows[]={row0, row1};
  return rows[0][0]*rows[1][1] - rows[0][1]*rows[1][0];
}

template <class T>
T vnl_determinant(T const *row0, T const *row1, T const *row2) {
  T const *rows[]={row0, row1, row2};
  return // the extra '+' makes it work nicely with emacs indentation.
    + rows[0][0]*rows[1][1]*rows[2][2]
    - rows[0][0]*rows[1][2]*rows[2][1]
    + rows[1][0]*rows[0][2]*rows[2][1]
    - rows[1][0]*rows[0][1]*rows[2][2]
    + rows[2][0]*rows[0][1]*rows[1][2]
    - rows[2][0]*rows[0][2]*rows[1][1];
}

template <class T>
T vnl_determinant(T const *row0, T const *row1, T const *row2, T const *row3) {
  T const *rows[]={row0, row1, row2, row3};
  return
    + rows[0][0]*rows[1][1]*rows[2][2]*rows[3][3]
    - rows[0][0]*rows[1][1]*rows[2][3]*rows[3][2]
    - rows[0][0]*rows[2][1]*rows[1][2]*rows[3][3]
    + rows[0][0]*rows[2][1]*rows[1][3]*rows[3][2]
    + rows[0][0]*rows[3][1]*rows[1][2]*rows[2][3]
    - rows[0][0]*rows[3][1]*rows[1][3]*rows[2][2]
    - rows[1][0]*rows[0][1]*rows[2][2]*rows[3][3]
    + rows[1][0]*rows[0][1]*rows[2][3]*rows[3][2]
    + rows[1][0]*rows[2][1]*rows[0][2]*rows[3][3]
    - rows[1][0]*rows[2][1]*rows[0][3]*rows[3][2]
    - rows[1][0]*rows[3][1]*rows[0][2]*rows[2][3]
    + rows[1][0]*rows[3][1]*rows[0][3]*rows[2][2]
    + rows[2][0]*rows[0][1]*rows[1][2]*rows[3][3]
    - rows[2][0]*rows[0][1]*rows[1][3]*rows[3][2]
    - rows[2][0]*rows[1][1]*rows[0][2]*rows[3][3]
    + rows[2][0]*rows[1][1]*rows[0][3]*rows[3][2]
    + rows[2][0]*rows[3][1]*rows[0][2]*rows[1][3]
    - rows[2][0]*rows[3][1]*rows[0][3]*rows[1][2]
    - rows[3][0]*rows[0][1]*rows[1][2]*rows[2][3]
    + rows[3][0]*rows[0][1]*rows[1][3]*rows[2][2]
    + rows[3][0]*rows[1][1]*rows[0][2]*rows[2][3]
    - rows[3][0]*rows[1][1]*rows[0][3]*rows[2][2]
    - rows[3][0]*rows[2][1]*rows[0][2]*rows[1][3]
    + rows[3][0]*rows[2][1]*rows[0][3]*rows[1][2];
}

//--------------------------------------------------------------------------------

#define VNL_DETERMINANT_INSTANTIATE(T) \
template T vnl_determinant(T const *, T const *); \
template T vnl_determinant(T const *, T const *, T const *); \
template T vnl_determinant(T const *, T const *, T const *, T const *);

