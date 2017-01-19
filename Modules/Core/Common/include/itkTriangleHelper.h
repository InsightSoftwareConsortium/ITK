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
#ifndef itkTriangleHelper_h
#define itkTriangleHelper_h

#include "itkCrossHelper.h"

namespace itk
{
/** \class TriangleHelper
 * \brief A convenience class for computation of various triangle elements in
 *        2D or 3D.
 * \ingroup ITKCommon
 */
template< typename TPoint >
class ITK_TEMPLATE_EXPORT TriangleHelper
{
public:
  typedef TriangleHelper                   Self;
  typedef TPoint                           PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  typedef typename PointType::VectorType   VectorType;
  typedef CrossHelper< VectorType >        CrossVectorType;

  itkStaticConstMacro(PointDimension, unsigned int, PointType::PointDimension);

  /** \brief return true if (iA,iB,iC) forms an Obtuse angle (above 90
    degrees)*/
  static bool IsObtuse(const PointType & iA, const PointType & iB, const PointType & iC);

  /** \brief Compute Normal vector to the triangle formed by (iA,iB,iC)*/
  static VectorType ComputeNormal(const PointType & iA,
                                  const PointType & iB,
                                  const PointType & iC);

  /** \brief Compute cotangent(iA,iB,iC)*/
  static CoordRepType Cotangent(const PointType & iA,
                                const PointType & iB,
                                const PointType & iC);

  /** \brief Compute barycenter, with given weights*/
  static PointType ComputeBarycenter(
    const CoordRepType & iA1, const PointType & iP1,
    const CoordRepType & iA2, const PointType & iP2,
    const CoordRepType & iA3, const PointType & iP3);

  /** \brief Compute angles (iA,iB,iC)*/
  static CoordRepType ComputeAngle(const PointType & iP1, const PointType & iP2,
                                   const PointType & iP3);

  /** \brief Compute center of mass*/
  static PointType ComputeGravityCenter(
    const PointType & iP1,
    const PointType & iP2,
    const PointType & iP3);

  /** \brief Compute circum center*/
  static PointType ComputeCircumCenter(
    const PointType & iP1,
    const PointType & iP2,
    const PointType & iP3);

  /** \brief Compute circum center constrained to be inside the triangle.*/
  static PointType ComputeConstrainedCircumCenter(const PointType & iP1,
                                                  const PointType & iP2, const PointType & iP3);

  /** \brief Compute Area.*/
  static CoordRepType ComputeArea(const PointType & iP1, const PointType & iP2, const PointType & iP3);

  static CoordRepType ComputeMixedArea( const PointType& iP1, const PointType& iP2, const PointType &iP3 );
};
}

#include "itkTriangleHelper.hxx"
#endif
