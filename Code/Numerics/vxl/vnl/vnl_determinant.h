#ifndef vnl_determinant_h_
#define vnl_determinant_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_determinant.h

//: \file
//  \brief Direct evaluation of determinants.
//  \author fsm@robots.ox.ac.uk
//: 2x2 matrix
template <class T> T vnl_determinant(T const *row0, 
                                     T const *row1);

//: 3x3 matrix
template <class T> T vnl_determinant(T const *row0,
                                     T const *row1,
                                     T const *row2);

//: 4x4 matrix
template <class T> T vnl_determinant(T const *row0, 
                                     T const *row1,
                                     T const *row2,
                                     T const *row3);

#endif // vnl_determinant_h_
