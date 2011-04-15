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
#ifndef __itkFEMLoadBase_h
#define __itkFEMLoadBase_h

#include "itkFEMElementBase.h"

namespace itk {
namespace fem {

/**
 * \class Load
 * \brief General abstract load base class.
 *
 * All other load classes that can be used in a FEM system are defined by deriving this one.
 * The load class defines an external load that acts on the system. For each specific subtype
 * of load, a separate load abstract class should be derived. For example we have LoadElement,
 * which defines the base for all loads that act on a specific element in a system.
 * \ingroup ITK-FEM
 */
class Load : public FEMLightObject
{
  FEM_ABSTRACT_CLASS(Load,FEMLightObject)
public:

  /** Array class that holds special pointers to the load objects */
  typedef FEMPArray<Self> ArrayType;

  /**
   * Sets the pointer to solution vector. This function is automatically
   * called by the Solver class on every load object.
   *
   * Some types of external Loads may need access to previous values of
   * solution vector. If a derived class needs that, it should implement
   * this function, and store the passed pointer accordingly. If the result
   * vector is not required, the functionn should be left unimplemented,
   * so that only the dummy implementation in base class is called.
   *
   * \param ptr Pointer to the object of Solution class.
   */
  virtual void SetSolution(Solution::ConstPointer) {}
  virtual Solution::ConstPointer GetSolution( ) { return 0;}

};

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadBase_h
