/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkMatrix_h
#define __itkMatrix_h


#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_matrix_fixed.h"


namespace itk
{

/** \class Matrix
 * \brief A templated class holding a M x N size Matrix
 * This class derives from a vnl_matrix_fixed in order 
 * to make all the vnl mathematical methods available.
 * 
 */

template<class T, unsigned int NRows=3, unsigned int NColumns=3>
class Matrix : public vnl_matrix_fixed<T,NRows,NColumns> {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef Matrix  Self;
  

   /**
   * Standard "Superclass" typedef.
   */
  typedef vnl_matrix_fixed<T,NRows,NColumns>  Superclass;


  /**
   * Matrix by Vector multiplication. 
   */
  Vector<T,NRows> operator*(const Vector<T,NColumns> & vector) const;
 
  /**
   * Matrix by Point multiplication. 
   */
  Point<T,NRows> operator*(const Point<T,NColumns> & vector) const;
 
  /**
   * Matrix by CovariantVector multiplication. 
   */
  CovariantVector<T,NRows> operator*(
                 const CovariantVector<T,NColumns> & vector) const;
 
  /**
   * Assignment Operator
   */
  inline const Self & operator=( const vnl_matrix<T> & matrix);

  /**
   * Return the inverse matrix
   */
  inline vnl_matrix<T> GetInverse( void ) const;
 
  /**
   * Return the transposed matrix
   */
  inline vnl_matrix<T> GetTranspose( void ) const;
 
};


  
} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMatrix.txx"
#endif


#endif 
