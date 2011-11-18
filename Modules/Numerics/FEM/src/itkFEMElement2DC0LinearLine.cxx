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

#include "itkFEMElement2DC0LinearLine.h"

namespace itk
{
namespace fem
{
void
Element2DC0LinearLine
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{

  // default integration order
  if( ( order == 0 ) || (order >= Element::gaussMaxOrder) )
    {
    order = DefaultIntegrationOrder;
    }

  pt.set_size(1);

  pt[0] = gaussPoint[order][i];
  w = gaussWeight[order][i];
}

unsigned int
Element2DC0LinearLine
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order
  if( ( order == 0 ) || (order >= Element::gaussMaxOrder) )
    {
    order = DefaultIntegrationOrder;
    }

  return order;
}

Element2DC0LinearLine::VectorType
Element2DC0LinearLine
::ShapeFunctions(const VectorType & pt) const
{
  /* Linear Line element has two shape functions  */
  VectorType shapeF(2);

  shapeF[0] = 0.5 - pt[0] / 2.0;
  shapeF[1] = 0.5 + pt[0] / 2.0;

  return shapeF;
}

void
Element2DC0LinearLine
::ShapeFunctionDerivatives(const VectorType &, MatrixType & shapeD) const
{
  shapeD.set_size(1, 2);

  shapeD[0][0] = -0.5;
  shapeD[0][1] =  0.5;
}

void
Element2DC0LinearLine
::Jacobian(const VectorType &, MatrixType & J, const MatrixType *) const
{
  // Since the line element defines only one global coordinate
  // and lives in 2D space, we need to provide a custom Jacobian.
  J.set_size(1, 1);

  // Get the length of the element
  // Note: This simple implementation is only valid for linear line elements.
  //       For higher order elements we must integrate to obtain the exact
  //       element length
  Float l = ( this->m_node[1]->GetCoordinates() - this->m_node[0]->GetCoordinates() ).magnitude();
  J[0][0] = l / 2;
}

bool
Element2DC0LinearLine
::GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & pcoords) const
{
  VectorType closestPoint(3);

  pcoords[1] = 0.0;
  pcoords[2] = 0.0;

  // get the length of the element
  const Float l = ( this->m_node[1]->GetCoordinates() - this->m_node[0]->GetCoordinates() ).magnitude();

  // tolerance is based on the length of the element
  const Float tol = l * l * 1e-8;

  const VectorType a1 = this->m_node[0]->GetCoordinates();
  const VectorType a2 = this->m_node[1]->GetCoordinates();

  // DistanceToLine sets pcoords[0] to a value t, 0 <= t <= 1
  const Float dist2 = this->DistanceToLine(globalPt, a1, a2, pcoords[0], closestPoint);

  // if the distance specified is more than the tolerance
  if( ( dist2 <= tol ) && ( pcoords[0] >= 0.0 ) && ( pcoords[0] <= 1.0 ) )
    {
    return true;
    }
  return false;
}

// ----------------------------------------------------------------------------
// Compute distance to finite line. Returns parametric coordinate t
// and point location on line.
itk::fem::Element::Float Element2DC0LinearLine::DistanceToLine(
  const VectorType & x, const VectorType & p1, const VectorType & p2, Float & t,
  VectorType & closestPoint) const
{
  Float      denom, num;
  VectorType p21(3);
  VectorType closest;
  Float      tolerance;

  //
  //   Determine appropriate vectors
  //
  p21[0] = p2[0] - p1[0];
  p21[1] = p2[1] - p1[1];
  p21[2] = p2[2] - p1[2];

  //
  //   Get parametric location
  //
  num = p21[0] * (x[0] - p1[0]) + p21[1] * (x[1] - p1[1]) + p21[2] * (x[2] - p1[2]);
  denom = p21[0] * p21[0] + p21[1] * p21[1] + p21[2] * p21[2];

  // trying to avoid an expensive fabs
  tolerance = 1e-5 * num;
  if( tolerance < 0.0 )
    {
    tolerance = -tolerance;
    }
  if( -tolerance < denom && denom < tolerance )  // numerically bad!
    {
    closest = p1; // arbitrary, point is (numerically) far away
    }
  //
  // If parametric coordinate is within 0<=p<=1, then the point is closest to
  // the line.  Otherwise, it's closest to a point at the end of the line.
  //
  else if( denom <= 0.0 || (t = num / denom) < 0.0 )
    {
    closest = p1;
    }
  else if( t > 1.0 )
    {
    closest = p2;
    }
  else
    {
    closest = p21;
    p21[0] = p1[0] + t * p21[0];
    p21[1] = p1[1] + t * p21[1];
    p21[2] = p1[2] + t * p21[2];
    }

  closestPoint[0] = closest[0];
  closestPoint[1] = closest[1];
  closestPoint[2] = closest[2];
  Float dist = (x[0] - closestPoint[0]) * (x[0] - closestPoint[0])
    + (x[1] - closestPoint[1]) * (x[1] - closestPoint[1]) + (x[2] - closestPoint[2]) * (x[2] - closestPoint[2]);
  return dist;
}

void Element2DC0LinearLine::PopulateEdgeIds(void)
{
  this->m_EdgeIds.resize(0);

  std::vector<int> edgePtIds;
  edgePtIds.resize(2);

  // edge 0
  edgePtIds[0] = 0;
  edgePtIds[1] = 1;
  this->m_EdgeIds.push_back(edgePtIds);
}

void Element2DC0LinearLine::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
