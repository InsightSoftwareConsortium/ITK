/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolution.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMSolution_h
#define __itkFEMSolution_h

namespace itk {
namespace fem {




/**
 * \class Solution
 * \brief Provides functions to access the values of the solution vector.
 *
 * The actual code of these functions as well as storage for
 * the data is implemented in LinearSystemWrapper class.
 *
 * \sa LinearSystemWrapper
 */
class Solution
{
public:

  /** Standard "Self" typedef.*/
  typedef Solution Self;
  /** Standard "Superclass" typedef. */
  typedef Solution Superclass;
  /**  Pointer to an object. */
  typedef Self* Pointer;
  /**  Const pointer to an object. */
  typedef const Self* ConstPointer;

  /** Floating point storage type used within a class */
  typedef double Float;

  /**
   * Returns value of i-th element in a solution vector. This value
   * is calculated generalized displacement of the i-th degree of
   * freedom in a FEM problem.
   *
   * \note If the solution vector doesn't exist (problem was not yet solved),
   *       or the index i is out of range, the function returns 0.0;
   */
  virtual Float GetValue(unsigned int i) = 0;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolution_h
