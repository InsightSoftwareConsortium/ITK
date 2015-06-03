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
#ifndef itkFEMLoadBase_h
#define itkFEMLoadBase_h

#include "itkFEMElementBase.h"
#include "itkFEMSolution.h"
#include "itkFEMPArray.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Load
 * \brief General abstract load base class.
 *
 * All other load classes that can be used in a FEM system are defined by deriving this one.
 * The load class defines an external load that acts on the system. For each specific subtype
 * of load, a separate load abstract class should be derived. For example we have LoadElement,
 * which defines the base for all loads that act on a specific element in a system.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Load : public FEMLightObject
{
public:
  /** Standard class typedefs. */
  typedef Load                     Self;
  typedef FEMLightObject           Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Load, FEMLightObject);

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
   * param ptr Pointer to the object of Solution class.
   */
  virtual void SetSolution(Solution::ConstPointer itkNotUsed(ptr)) { }
  virtual Solution::ConstPointer GetSolution()
  {
    return ITK_NULLPTR;
  }
  /**
  * Get the element containing the degree of freedom
  * on which the force is being applied.
  */
  const Element * GetElement() const
    {
    return m_Element;
    }

  /**
   * Get the element containing the degree of freedom
   * on which the force is being applied.
   */
  void SetElement( const Element * el)
    {
    this->m_Element = el;
    }

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;
  /**
   * Pointer to an element in a system that contains the DOF
   * on which the external force is applied.
   */
  Element::ConstPointer m_Element;


};
}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadBase_h
