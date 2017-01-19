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

#ifndef itkFEMElement1DStress_h
#define itkFEMElement1DStress_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk
{
namespace fem
{
/**
 * \class Element1DStress
 * \brief Class that is used to define linear elasticity problem in 1D space.
 *
 * This class only defines the physics of the problem. Use this class together
 * with element classes that specify the geometry to fully define the element.
 *
 * You can specify one template parameter:
 *
 *   TBaseClass - Class from which Element1DStress is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element base class.
 * \ingroup ITKFEM
 */
template <typename TBaseClass = Element>
class ITK_TEMPLATE_EXPORT Element1DStress : public TBaseClass
{
public:
  /** Standard class typedefs. */
  typedef Element1DStress          Self;
  typedef TBaseClass               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element1DStress, TBaseClass);

  // Repeat the required typedefs and enums from parent class
  typedef typename Superclass::Float      Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;

  /**
   * Default constructor only clears the internal storage
   */
  Element1DStress();

  // ////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the physics of the problem.
   */

  /**
   * Compute the B matrix.
   */
  virtual void GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const ITK_OVERRIDE;

  /**
   * Compute the D matrix.
   */
  virtual void GetMaterialMatrix(MatrixType & D) const ITK_OVERRIDE;

  /**
   * Element stiffness matrix is reimplemented here, because we want to
   * be able to use this class to implement 1D stress problem in any
   * number of dimensions i.e. Bar1D, Bar2D, Bar3D.
   */
  virtual void GetStiffnessMatrix(MatrixType & Ke) const ITK_OVERRIDE;

  /**
   * 1D stress elements have 2 DOFs per node. In reality there is
   * only one, but it usually makes sense to separate it into the
   * components that correspond to specific dimensions in space.
   * So the number of DOFs per node is equal to the number of
   * spatial dimensions.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode(void) const ITK_OVERRIDE
  {
    return 2;
  }

  /**
   * Get/Set the material properties for the element
   */
  virtual Material::ConstPointer GetMaterial(void) const ITK_OVERRIDE
  {
    return m_mat;
  }

  virtual void SetMaterial(Material::ConstPointer mat_) ITK_OVERRIDE
  {
    m_mat = dynamic_cast<const MaterialLinearElasticity *>(mat_.GetPointer());
  }

protected:

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
   * Pointer to material properties of the element
   */
  const MaterialLinearElasticity * m_mat;

};  // class Element1DStress

}
}  // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElement1DStress.hxx"
#endif

#endif  // #ifndef itkFEMElement1DStress_h
