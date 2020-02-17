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

#ifndef itkFEMElement2DC1Beam_h
#define itkFEMElement2DC1Beam_h

#include "itkFEMElementStd.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element2DC1Beam
 * \brief 1D Beam (spring that also bends) finite element in 2D space.
 *
 * The Displacements at each node are modelled with two translational,
 * and one rotational, degree of freedom.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element2DC1Beam : public ElementStd<2, 2>
{
public:
  /** Standard class type aliases. */
  using Self = Element2DC1Beam;
  using TemplatedParentClass = ElementStd<2, 2>;
  using Superclass = TemplatedParentClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC1Beam, TemplatedParentClass);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  ::itk::LightObject::Pointer
  CreateAnother() const override;


  /**
   * Default constructor only clears the internal storage
   */
  Element2DC1Beam();

  /**
   * Construct an element by specifying two nodes and material
   */
  Element2DC1Beam(Node::ConstPointer n1_, Node::ConstPointer n2_, Material::ConstPointer mat_);

  // ////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the physics of the problem.
   */

  /** Get the Stiffness matrix */
  void
  GetStiffnessMatrix(MatrixType & Ke) const override;

  /** Get the Mass matrix */
  void
  GetMassMatrix(MatrixType & Me) const override;

  /** Get the Strain Displacement matrix */
  void
  GetStrainDisplacementMatrix(MatrixType &, const MatrixType &) const override
  {}

  /** Get the Material matrix */
  void
  GetMaterialMatrix(MatrixType &) const override
  {}

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum
  {
    DefaultIntegrationOrder = 1
  };

  /** Get the Integration point and weight */
  void
  GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order = 0) const override;

  unsigned int
  GetNumberOfIntegrationPoints(unsigned int order) const override;

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the geometry of an element
   */

  /** Return the shape functions used to interpolate across the element */
  VectorType
  ShapeFunctions(const VectorType & pt) const override;

  /** Return the shape functions derivatives in the shapeD matrix */
  void
  ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const override;

  /** Convert from global to local coordinates */
  bool
  GetLocalFromGlobalCoordinates(const VectorType &, VectorType &) const override
  {
    return false;
  }

  /** Return the determinate of the Jacobian */
  Float
  JacobianDeterminant(const VectorType & pt, const MatrixType * pJ) const override;

  /** Get the degrees of freedom for each node */
  unsigned int
  GetNumberOfDegreesOfFreedomPerNode() const override
  {
    return 3;
  }

  /**
   * Get/Set the material properties for the element
   */
  Material::ConstPointer
  GetMaterial() const override
  {
    return dynamic_cast<const Material *>(m_mat);
  }

  void
  SetMaterial(Material::ConstPointer mat_) override
  {
    m_mat = dynamic_cast<const MaterialLinearElasticity *>(mat_.GetPointer());
  }

  /** No edges to populate in this class */
  void
  PopulateEdgeIds() override
  { /* empty */
  }

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /**
   * Pointer to material properties of the element
   */
  const MaterialLinearElasticity * m_mat{ nullptr };
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement2DC1Beam_h
