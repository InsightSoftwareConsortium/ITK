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

#ifndef itkFEMLoadGrav_h
#define itkFEMLoadGrav_h

#include "itkFEMLoadElementBase.h"
#include "ITKFEMExport.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadGrav
 * \brief Abstract gravity load class.
 *
 * This load is integrated over a whole element. The load vector is returned in a
 * Fg member function defined in a derived class. The Fg function accepts a vector
 * specifying a point in global coordinate system and returns a load vector
 * defined at the point. Derived LoadClasses must define this function.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadGrav : public LoadElement
{
public:
  /** Standard class typedefs. */
  typedef LoadGrav                 Self;
  typedef LoadElement              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadGrav, LoadElement);

  virtual vnl_vector<Float> GetGravitationalForceAtPoint(vnl_vector<Float> ) = 0;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};

/**
 * \class LoadGravConst
 * \brief Constant gravity load class.
 *
 * This is a special case of LoadGrav. The load vector is the same on
 * every point in space.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadGravConst : public LoadGrav
{
public:
  /** Standard class typedefs. */
  typedef LoadGravConst            Self;
  typedef LoadGrav                 Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadGravConst, LoadGrav);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  virtual vnl_vector<Float> GetGravitationalForceAtPoint(vnl_vector<Float> ) ITK_OVERRIDE;

  /**
   * Set the gravity force that exists at every point
   */
  void SetForce(const vnl_vector<itk::fem::Element::Float> force);

  /**
   * Get the gravity force that exists at every point
   */
  vnl_vector<itk::fem::Element::Float> & GetForce();
  const vnl_vector<itk::fem::Element::Float> & GetForce() const;

  /** Apply the load to the specified element */
  virtual void ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) ITK_OVERRIDE;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  vnl_vector<Float> m_GravityForce;
};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadGrav_h
