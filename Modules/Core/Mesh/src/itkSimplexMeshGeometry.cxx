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
#include "itkSimplexMeshGeometry.h"
#include "itkMath.h"

#include "vxl_version.h"
#if VXL_VERSION_DATE_FULL > 20040406
#include "vnl/vnl_cross.h"
#define cross_3d vnl_cross_3d
#endif

namespace itk
{
SimplexMeshGeometry
::SimplexMeshGeometry()
{
  double    c = 1.0 / 3.0;
  PointType p;

  p.Fill(0.0);

  pos.Fill(0);
  oldPos.Fill(0);
  referenceMetrics.Fill(c);
  eps.Fill(c);
  normal.Fill(0);
  externalForce.Fill(0);
  internalForce.Fill(0);
  closestAttractor.Fill(0);
  circleRadius = 0;
  circleCenter.Fill(0);
  sphereRadius = 0;
  distance = 0;
  phi = 0;
  multiplier = 0.0;
  forceIndex = 0;

  neighborIndices.Fill( NumericTraits< IdentifierType >::max() );
  neighbors.Fill(p);
  meanCurvature = c;

  neighborSet = ITK_NULLPTR;
  closestAttractorIndex = 0;
}

SimplexMeshGeometry
::~SimplexMeshGeometry()
{
  delete this->neighborSet;
  this->neighborSet = ITK_NULLPTR;
}

void
SimplexMeshGeometry
::ComputeGeometry()
{
  VectorType b, c, cXb, tmp;

  //compute the circum circle (center and radius)
  b = this->neighbors[2] - this->neighbors[0];
  c = this->neighbors[1] - this->neighbors[0];

  cXb.SetVnlVector( cross_3d< double >( c.GetVnlVector(), b.GetVnlVector() ) );

  tmp.SetVnlVector( b.GetSquaredNorm()
                    * cross_3d< double >( cXb.GetVnlVector(), c.GetVnlVector() )
                    + c.GetSquaredNorm()
                    * cross_3d< double >( b.GetVnlVector(), cXb.GetVnlVector() ) );

  double cXbSquaredNorm = 2 * cXb.GetSquaredNorm();

  circleRadius = tmp.GetNorm() / ( cXbSquaredNorm );
  tmp[0] /= ( cXbSquaredNorm );
  tmp[1] /= ( cXbSquaredNorm );
  tmp[2] /= ( cXbSquaredNorm );
  circleCenter = this->neighbors[0] + tmp;

  // Compute the circum sphere (center and radius) of a point
  VectorType d, dXc, bXd, sphereTmp, denom;

  d = pos - this->neighbors[0];
  dXc.SetVnlVector( cross_3d< double >( d.GetVnlVector(), c.GetVnlVector() ) );
  bXd.SetVnlVector( cross_3d< double >( b.GetVnlVector(), d.GetVnlVector() ) );

  sphereTmp.SetVnlVector( d.GetSquaredNorm() * cXb.GetVnlVector()
                          + b.GetSquaredNorm() * dXc.GetVnlVector()
                          + c.GetSquaredNorm() * bXd.GetVnlVector() );

  double val = 2 * ( c[0] * ( b[1] * d[2] - b[2] * d[1] )
                     - c[1] * ( b[0] * d[2] - b[2] * d[0] )
                     + c[2] * ( b[0] * d[1] - b[1] * d[0] ) );

  // fix for points which lay on their neighbors plane
  // necessary ??
  if (Math::AlmostEquals( val, 0.0 ))
    {
    val = 1.0; //  itkAssertInDebugAndIgnoreInReleaseMacro (val != 0 );
    }

  sphereRadius = sphereTmp.GetNorm() / val;

  if ( sphereRadius < 0 )
    {
    sphereRadius = -1 * sphereRadius;
    }
}

void
SimplexMeshGeometry::
CopyFrom( const SimplexMeshGeometry & input )
{
  for( unsigned int i = 0; i < 3; i++ )
    {
    this->neighborIndices[i] = input.neighborIndices[i];
    this->neighbors[i] = input.neighbors[i];
    }
  this->meanCurvature = input.meanCurvature;
  this->pos = input.pos;
  this->oldPos = input.oldPos;
  this->eps = input.eps;
  this->referenceMetrics = input.referenceMetrics;
  this->normal = input.normal;
  this->externalForce = input.externalForce;
  this->internalForce = input.internalForce;
  this->closestAttractor = input.closestAttractor;
  this->closestAttractorIndex = input.closestAttractorIndex;
  this->circleRadius = input.circleRadius;
  this->circleCenter = input.circleCenter;
  this->sphereRadius = input.sphereRadius;
  // this->sphereCenter = input.sphereCenter;
  this->distance = input.distance;
  this->phi = input.phi;
  this->multiplier = input.multiplier;
  this->forceIndex = input.forceIndex;
  this->CopyNeigborSet( input.neighborSet );
}

void
SimplexMeshGeometry
::CopyNeigborSet( const NeighborSetType * nset )
{
  delete this->neighborSet;
  if( nset )
    {
    this->neighborSet = new NeighborSetType;
    this->neighborSet->insert( nset->begin(), nset->end() );
    }
  else
    {
    this->neighborSet = ITK_NULLPTR;
    }
}

}  // end namespace itk
