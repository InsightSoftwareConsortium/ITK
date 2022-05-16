/*=========================================================================
 *
 * Copyright NumFOCUS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMElement3DMembrane1DOF_h
#define itkFEMElement3DMembrane1DOF_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DMembrane1DOF
 * \brief Class that is used to define a membrane energy problem in 3D space.
 *
 * This class only defines the physics of the problem. Use his class together
 * with element classes that specify the geometry to fully define the element.
 *
 * You can specify one template parameter:
 *
 *   TBaseClass - Class from which Element3DMembrane1DOF is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element base class.
 * \ingroup ITKFEM
 */
template <typename TBaseClass = Element>
class ITK_TEMPLATE_EXPORT Element3DMembrane1DOF : public TBaseClass
{
public:
  /** Standard class type aliases. */
  using Self = Element3DMembrane1DOF;
  using Superclass = TBaseClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DMembrane1DOF, TBaseClass);

  // Repeat the required type alias and enums from parent class
  using typename Superclass::Float;
  using typename Superclass::MatrixType;
  using typename Superclass::VectorType;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DMembrane1DOF();

  // ////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the physics of the problem.
   */

  /**
   * Compute the B matrix.
   */
  void
  GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const override;

  /**
   * Compute the D matrix.
   */
  void
  GetMaterialMatrix(MatrixType & D) const override;

  /**
   * Compute the mass matrix specific for 3D membrane problems.
   */
  void
  GetMassMatrix(MatrixType & Me) const override;

  /**
   * 3D membrane elements have 3 DOFs per node.
   */
  unsigned int
  GetNumberOfDegreesOfFreedomPerNode() const override
  {
    return 3;
  }

  /** Get the Stiffness matrix */
  void
  GetStiffnessMatrix(MatrixType & Ke) const override;

  /**
   * Get/Set the material properties for the element
   */
  Material::ConstPointer
  GetMaterial() const override
  {
    return dynamic_cast<const Material *>(m_Mat.GetPointer());
  }

  void
  SetMaterial(Material::ConstPointer mat_) override
  {
    m_Mat = dynamic_cast<const MaterialLinearElasticity *>(mat_.GetPointer());
  }

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Pointer to material properties of the element
   */
  MaterialLinearElasticity::ConstPointer m_Mat;
};

} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMElement3DMembrane1DOF.hxx"
#endif

#endif // itkFEMElement3DMembrane1DOF_h
