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
  /** Standard class typedefs. */
  typedef Element2DC1Beam          Self;
  typedef ElementStd<2, 2>         TemplatedParentClass;
  typedef TemplatedParentClass     Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element2DC1Beam, TemplatedParentClass);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;


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
  virtual void GetStiffnessMatrix(MatrixType & Ke) const ITK_OVERRIDE;

  /** Get the Mass matrix */
  virtual void GetMassMatrix(MatrixType & Me) const ITK_OVERRIDE;

  /** Get the Strain Displacement matrix */
  virtual void GetStrainDisplacementMatrix(MatrixType &, const MatrixType &) const ITK_OVERRIDE
  {
  }

  /** Get the Material matrix */
  virtual void GetMaterialMatrix(MatrixType &) const ITK_OVERRIDE
  {
  }

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 1 };

  /** Get the Integration point and weight */
  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order = 0) const ITK_OVERRIDE;

  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const ITK_OVERRIDE;

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the geometry of an element
   */

  /** Return the shape functions used to interpolate across the element */
  virtual VectorType ShapeFunctions(const VectorType & pt) const ITK_OVERRIDE;

  /** Return the shape functions derivatives in the shapeD matrix */
  virtual void ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const ITK_OVERRIDE;

  /** Convert from global to local coordinates */
  virtual bool GetLocalFromGlobalCoordinates(const VectorType &, VectorType &) const ITK_OVERRIDE
  {
    return false;
  }

  /** Return the determinate of the Jacobian */
  virtual Float JacobianDeterminant(const VectorType & pt, const MatrixType *pJ) const ITK_OVERRIDE;

  /** Get the degrees of freedom for each node */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode(void) const ITK_OVERRIDE
  {
    return 3;
  }

  /**
   * Get/Set the material properties for the element
   */
  virtual Material::ConstPointer GetMaterial(void) const ITK_OVERRIDE
  {
    return dynamic_cast<const Material *>(m_mat);
  }

  virtual void SetMaterial(Material::ConstPointer mat_) ITK_OVERRIDE
  {
    m_mat =
      dynamic_cast<const MaterialLinearElasticity *>( mat_.GetPointer() );
  }

  /** No edges to populate in this class */
  virtual void PopulateEdgeIds(void) ITK_OVERRIDE { /* empty */ }

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:

  /**
   * Pointer to material properties of the element
   */
  const MaterialLinearElasticity *m_mat;

};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement2DC1Beam_h
