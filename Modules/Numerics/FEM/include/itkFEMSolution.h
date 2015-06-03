/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMSolution_h
#define itkFEMSolution_h

#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Solution
 * \brief Provides functions to access the values of the solution vector.
 *
 * The actual code of these functions as well as storage for
 * the data is implemented in LinearSystemWrapper class.
 *
 * \sa LinearSystemWrapper
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Solution
{
public:

  /** Standard "Self" typedef. */
  typedef Solution Self;
  /** Standard "Superclass" typedef. */
  typedef Solution Superclass;
  /**  Pointer to an object. */
  typedef Self *Pointer;
  /**  Const pointer to an object. */
  typedef const Self *ConstPointer;

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
   * \param solutionIndex index of solution vector to get value from
   *
   * \note If the solution vector doesn't exist (problem was not yet solved),
   *       or the index i is out of range, the function returns 0.0.
   */
  virtual Float GetSolutionValue(unsigned int i, unsigned int solutionIndex = 0) const = 0;

  /**
   * Virtual destructor should properly destroy the object and clean up any
   * memory allocated for matrix and vector storage.
   */
  virtual ~Solution();
};
}
}  // end namespace itk::fem

#endif // #ifndef itkFEMSolution_h
