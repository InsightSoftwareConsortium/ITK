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
#ifndef itkFEMElement3DC0LinearTetrahedron_h
#define itkFEMElement3DC0LinearTetrahedron_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"
// to make some checks in GetLocalFromGlobalCoordinates
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_matrix_inverse.h>

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTetrahedron
 * \brief 4-noded, linear, C0 continuous finite element in 3D space.
 *
 * The ordering of the nodes should be defined in the following order:
 *
 *            (1,0,1)
 *               3
 *               *
 *              /|\
 *             / | \
 *            /  |  \
 * (0,0,0) 0 *-- | --* 2 (2,0,0)
 *            \  |  /
 *             \ | /
 *              \|/
 *               *
 *               1
 *            (1,1,0)
 *
 *
 * This is an abstract class. Specific concrete implementations of this
 * It must be combined with the physics component of the problem.
 * This has already been done in the following classes:
 *
 * \sa Element3DC0LinearTetrahedronMembrane
 * \sa Element3DC0LinearTetrahedronStrain
 *
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearTetrahedron : public ElementStd<4, 3>
{
public:
  /** Standard class type aliases. */
  using Self = Element3DC0LinearTetrahedron;
  using TemplatedParentClass = ElementStd<4, 3>;
  using Superclass = TemplatedParentClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTetrahedron, TemplatedParentClass);

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
  GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const override;

  /** Get the number of integration points */
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
  GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  PopulateEdgeIds() override;
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement3DC0LinearTetrahedron_h
