/*=========================================================================
 *
 *  Copyright NumFOCUS
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
  /** Standard class type aliases. */
  using Self = LoadGrav;
  using Superclass = LoadElement;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadGrav, LoadElement);

  virtual vnl_vector<Float> GetGravitationalForceAtPoint(vnl_vector<Float>) = 0;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
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
  /** Standard class type aliases. */
  using Self = LoadGravConst;
  using Superclass = LoadGrav;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadGravConst, LoadGrav);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  ::itk::LightObject::Pointer
  CreateAnother() const override;

  vnl_vector<Float> GetGravitationalForceAtPoint(vnl_vector<Float>) override;

  /**
   * Set the gravity force that exists at every point
   */
  void
  SetForce(const vnl_vector<itk::fem::Element::Float> force);

  /**
   * Get the gravity force that exists at every point
   */
  vnl_vector<itk::fem::Element::Float> &
  GetForce();
  const vnl_vector<itk::fem::Element::Float> &
  GetForce() const;

  /** Apply the load to the specified element */
  void
  ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  vnl_vector<Float> m_GravityForce;
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMLoadGrav_h
