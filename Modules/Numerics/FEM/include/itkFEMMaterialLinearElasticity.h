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

#ifndef itkFEMMaterialLinearElasticity_h
#define itkFEMMaterialLinearElasticity_h

#include "itkFEMMaterialBase.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class MaterialLinearElasticity
 * \brief Linear elasticity material class
 *
 * This class includes material and other kind of properties required to
 * define material properties of finite elements applied to linear
 * elasticity problems in FEM toolkit.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT MaterialLinearElasticity : public Material
{
public:
  /** Standard class type aliases. */
  using Self = MaterialLinearElasticity;
  using Superclass = Material;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaterialLinearElasticity, Material);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  ::itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Default constructor only initializes the members.
   */
  MaterialLinearElasticity();

  /**
   * Set cross-sectional area
   */
  void
  SetCrossSectionalArea(double area);

  /**
   * Get cross-sectional area
   */
  double
  GetCrossSectionalArea() const;

  /**
   * Set youngs/elastic modulus
   */
  void
  SetYoungsModulus(double modulus);

  /**
   * Get youngs/elastic modulus
   */
  double
  GetYoungsModulus() const;

  /**
   * Set thickness - for 2D plane stress/strain problems
   */
  void
  SetThickness(double t);

  /**
   * Get thickness - for 2D plane stress/strain problems
   */
  double
  GetThickness() const;

  /**
   * Set Moment of inertia - for beam elements
   */
  void
  SetMomentOfInertia(double iner);

  /**
   * Get Moment of inertia - for beam elements
   */
  double
  GetMomentOfInertia() const;

  /**
   * Set poisson's ratio
   */
  void
  SetPoissonsRatio(double poi);

  /**
   * Get poisson's ratio
   */
  double
  GetPoissonsRatio() const;

  /**
   * Set density heat product
   */
  void
  SetDensityHeatProduct(double dhp);

  /**
   * Get density heat product
   */
  double
  GetDensityHeatProduct() const;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /* Data members of MaterialLinearElasticity class */

  /**
   * Young modulus
   */
  double m_YoungModulus{ 100.0 };

  /**
   * Cross section area of a line element
   */
  double m_CrossSectionalArea{ 1.0 }; //

  /**
   * Moment of inertia
   */
  double m_MomentOfInertia{ 1.0 };

  /**
   * Poisson's ratio
   */
  double m_PoissonRatio{ 0.2 };

  /**
   * Thickness
   */
  double m_Thickness{ 1.0 };

  /*
   * ... we can add properties here as required without the influence on the already defined elements
   */

  /**
   * Density times Heat Capacity
   */
  double m_DensityHeatCapacity{ 1.0 };
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMMaterialLinearElasticity_h
