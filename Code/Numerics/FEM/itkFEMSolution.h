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
   * freedom in a FEM problem. Note that in general there may be several
   * solution vectors. You can select which one do you want by passing
   * the second parameter.
   *
   * \param i element index in solution vector
   * \param SolutionIndex index of solution vector to get value from
   *
   * \note If the solution vector doesn't exist (problem was not yet solved),
   *       or the index i is out of range, the function returns 0.0.
   */
  virtual Float GetSolutionValue(unsigned int i, unsigned int SolutionIndex = 0) const = 0;

  /**
   * Virtual destructor should properly destroy the object and clean up any
   * memory allocated for matrix and vector storage.
   */
  virtual ~Solution() {};

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolution_h
