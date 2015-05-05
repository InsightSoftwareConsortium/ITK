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
#ifndef itkFEMLoadBC_h
#define itkFEMLoadBC_h

#include "itkFEMLoadBase.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadBC
 * \brief Generic essential (Dirichlet) boundary conditions.
 *
 * Objects of this class specify, which DOFs in a system are fixed.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadBC : public Load
{
public:
  /** Standard class typedefs. */
  typedef LoadBC                   Self;
  typedef Load                     Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadBC, Load);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /** Default constructor */
  LoadBC() : m_DegreeOfFreedom(0), m_Value()
  {
  }

  /** Set the number of degrees of freedom*/
  void SetDegreeOfFreedom(int dof);

  /** Get the number of degrees of freedom*/
  int GetDegreeOfFreedom() const;

  /** Set the boundary condition using vector representation*/
  void SetValue(const vnl_vector<Element::Float> val);

  /** Get the boundary condition as vector representation*/
  vnl_vector<Element::Float> GetValue() const;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
   * Local DOF number within the Element object.
   */
  unsigned int m_DegreeOfFreedom;

  /**
   * Value which the DOF is being fixed.
   *
   * \note This is a vector so that implementation of BC on isotropic elements
   *       is easy. Which value is applied to the master force vector is
   *       defined by optional dim parameter (defaults to 0) in AssembleF
   *       function in solver.
   */
  vnl_vector<Element::Float> m_Value;
};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadBC_h
