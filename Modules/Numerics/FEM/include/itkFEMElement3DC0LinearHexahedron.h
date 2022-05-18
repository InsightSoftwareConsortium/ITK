/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMElement3DC0LinearHexahedron_h
#define itkFEMElement3DC0LinearHexahedron_h

#include "itkFEMElementStd.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearHexahedron
 * \brief 8-noded, linear, C0 continuous finite element in 3D space.
 *
 *
 * The ordering of the nodes should be defined in the following order:
 *
 *                  5 (0,1,1)                6 (1,1,1)
 *                  *------------------------*
 *                 /|                       /|
 *                / |                      / |
 *               /  |                     /  |
 *              /   |                    /   |
 *             /    |         (1,1,0) 7 /    |
 *  (0,1,0)  4*------------------------*     |
 *            |    1* ---------------- | ----*2 (1,0,1)
 *            |    / (0,0,1)           |    /
 *            |   /                    |   /
 *            |  /                     |  /
 *            | /                      | /
 *            |/                       |/
 *            *------------------------*
 *            0                        3
 *          (0,0,0)                  (1,0)
 *
 *
 * This is an abstract class. Specific concrete implementations of this
 * It must be combined with the physics component of the problem.
 * This has already been done in the following classes:
 *
 * \sa Element3DC0LinearHexahedronMembrane
 * \sa Element3DC0LinearHexahedronStrain
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearHexahedron : public ElementStd<8, 3>
{
public:
  /** Standard class type aliases. */
  using Self = Element3DC0LinearHexahedron;
  using TemplatedParentClass = ElementStd<8, 3>;
  using Superclass = TemplatedParentClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearHexahedron, TemplatedParentClass);

  // ////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  void
  GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const override;

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

  /**
   * Methods used in computing parametric/local coordinates given global coordinates.
   */
  void
  InterpolationFunctions(const VectorType & pcoords, VectorType & sf) const;

  void
  InterpolationDerivs(const VectorType & pcoords, VectorType & derivs) const;

  Float
  Determinant3x3(const VectorType & c1, const VectorType & c2, const VectorType & c3) const;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  PopulateEdgeIds() override;
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMElement3DC0LinearHexahedron_h
