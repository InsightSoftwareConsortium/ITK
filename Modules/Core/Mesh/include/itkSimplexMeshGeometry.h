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
#ifndef itkSimplexMeshGeometry_h
#define itkSimplexMeshGeometry_h


#include "itkIntTypes.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include <set>
#include "ITKMeshExport.h"

namespace itk
{
/**
 * \class SimplexMeshGeometry
 * \brief handle geometric properties for vertices of a simplx mesh
 *
 * It stores and recomputes geometric properties of simplex mesh
 * vertices, i.e. the normal vector, the barycentric coordinates of
 * the point related to its three neighbor vertices, simplex angle
 * circumsphere and circumcirlce radius and center.
 *
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
class ITKMesh_EXPORT SimplexMeshGeometry
{
public:

  typedef itk::Point< double, 3 >               PointType;
  typedef itk::Vector< double, 3 >              VectorType;
  typedef itk::CovariantVector< double, 3 >     CovariantVectorType;
  typedef itk::FixedArray< IdentifierType, 3 >  IndexArray;
  typedef itk::FixedArray< PointType, 3 >       PointArray;
  typedef std::set< IdentifierType >            NeighborSetType;

  SimplexMeshGeometry();

  ~SimplexMeshGeometry();

  void CopyFrom( const SimplexMeshGeometry & input );

  void CopyNeigborSet( const NeighborSetType * nset );

  /** Definition of some attributes for
   * faster deformable model computation
   */

  /* stores the indices of the three direct neighbors */
  IndexArray neighborIndices;

  /* stores the coordinates of the three direct neighbors */
  PointArray neighbors;

  /* stores the mean curvature of the mesh in the point */
  double meanCurvature;

  /* coordinates of the corresponding point */
  PointType pos;

  /* coordinates of the corresponding point in previous iteration */
  PointType oldPos;

  /** barycentric coordinates of corresponding point with respect
   * to its three direct neighbors
   */
  PointType eps;

  /**
   * reference metric params
   */
  PointType referenceMetrics;

  /* normal vector of corresponding point */
  CovariantVectorType normal;

  /** stores external force component for
   * current deformable model iteration
   */
  VectorType externalForce;

  /** stores internal force component for
   * current deformable model iteration
   */
  VectorType internalForce;

  /**
   * store the location of the closest attractor to this point
   */
  PointType closestAttractor;

  /**
   * stores the index of the closest attractor to this point
   */
  IdentifierType closestAttractorIndex;

  /* stores circum circle radius */
  double circleRadius;

  /* stores circum circle center */
  PointType circleCenter;

  /* stores circum sphere radius */
  double sphereRadius;

  /* stores circum sphere center */
  //  PointType sphereCenter;

  /* stores distance to foot point */
  double distance;

  /* stores angle */
  double phi;

  /* stores the neighbor set */
  NeighborSetType *neighborSet;

  /* stores multiplier for interactive deformable model filter */
  double multiplier;

  IdentifierType forceIndex;

  /**
   * Computes the center and radius of the circum circle of the
   * three neighbor points and of the circum sphere
   */
  void ComputeGeometry();

protected:
}; // end of class SimplexMeshGeometry
} //end of namespace itk

#endif
