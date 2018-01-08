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

#ifndef itkFEMElement3DStrain_h
#define itkFEMElement3DStrain_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DStrain
 * \brief Class that is used to define linear elasticity problem in 3D space.
 *
 * This class only defines the physics of the problem. Use his class together
 * with element classes that specify the geometry to fully define the element.
 *
 * This class defines the physics to define element strain.
 *
 * You can specify one template parameter:
 *
 *   TBaseClass - Class from which Element3DStrain is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element base class.
 * \ingroup ITKFEM
 */
template <typename TBaseClass = Element>
class ITK_TEMPLATE_EXPORT Element3DStrain : public TBaseClass
{
public:
  /** Standard class typedefs. */
  typedef Element3DStrain          Self;
  typedef TBaseClass               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DStrain, TBaseClass);

  // Repeat the required typedefs and enums from parent class
  typedef typename Superclass::Float      Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DStrain();

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the physics of the problem.
   */

  /**
   * Compute the B matrix.
   */
  void GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const ITK_OVERRIDE;

  /**
   * Compute the D matrix.
   */
  void GetMaterialMatrix(MatrixType & D) const ITK_OVERRIDE;

  /**
   * 3D strain elements have 3 DOFs per node.
   */
  unsigned int GetNumberOfDegreesOfFreedomPerNode(void) const ITK_OVERRIDE
  {
    return 3;
  }

  /**
   * Get/Set the material properties for the element
   */
  Material::ConstPointer GetMaterial(void) const ITK_OVERRIDE
  {
    return m_mat;
  }

  void SetMaterial(Material::ConstPointer mat_) ITK_OVERRIDE
  {
    m_mat =
      dynamic_cast<const MaterialLinearElasticity *>( mat_.GetPointer() );
  }

protected:

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
   * Pointer to material properties of the element
   */
  const MaterialLinearElasticity *m_mat;

};  // class Element3DStrain

}
}  // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElement3DStrain.hxx"
#endif

#endif  // #ifndef itkFEMElement3DStrain_h
