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

#include "itkFEMElement3DC0LinearTetrahedron.h"

namespace itk
{
namespace fem
{

void
Element3DC0LinearTetrahedron
::GetIntegrationPointAndWeight(unsigned int, VectorType & pt, Float & w, unsigned int) const
{
  // FIXME: Write rules for other integration orders
  // for tetrahedral elements a single point should suffice
  // http://www.cs.rpi.edu/~flaherje/pdf/fea6.pdf
  pt.set_size(3);

  Float d = 1.0 / std::sqrt(3.0);

  pt[0] = d;
  pt[1] = d;
  pt[2] = d;

  w = 1.0;
}

unsigned int
Element3DC0LinearTetrahedron
::GetNumberOfIntegrationPoints(unsigned int) const
{
  return 1;
}

Element3DC0LinearTetrahedron::VectorType
Element3DC0LinearTetrahedron
::ShapeFunctions(const VectorType & pt) const
{
  /* Linear tetrahedral element has four shape functions  */
  VectorType shapeF(4);

  /**
   * Linear tetrahedral element has local coordinates
   * (0,0,0), (1,0,0), (0,1,0), (0,0,1)
   */

  /** given local point x=(r,s,t), where 0 <= r,s,t <= 1 */

  /** N_1 = 1 - r - s - t; */
  shapeF[0] = 1 - pt[0] - pt[1] - pt[2];

  /** N_2 = r */
  shapeF[1] = pt[0];

  /** N_3 = s */
  shapeF[2] = pt[1];

  /** N_4 = t */
  shapeF[3] = pt[2];

  return shapeF;
}

void
Element3DC0LinearTetrahedron
::ShapeFunctionDerivatives(const VectorType &, MatrixType & shapeD) const
{
  /** functions at directions r and s.  */
  shapeD.set_size(3, 4);
  shapeD.fill(0.0);
  /** d(N_1) / d(r,s,t) = -1 */
  for( int j = 0; j < 3; j++ )
    {
    shapeD[j][0] = -1;
    }
  /** d(N_2) / dr, d(N_3) / ds, d(N_4) / dt = 1 */
  for( int j = 1; j < 4; j++ )
    {
    shapeD[j - 1][j] = 1;
    }
}

bool
Element3DC0LinearTetrahedron
::GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const
{
  Float x = globalPt[0];
  Float y = globalPt[1];
  Float z = globalPt[2];

  Float x0, x1, x2, x3;
  Float y0, y1, y2, y3;
  Float z0, z1, z2, z3;
  Float A;

  localPt.set_size(3);
  localPt.fill(0.0);

  x0 = this->m_node[0]->GetCoordinates()[0];
  y0 = this->m_node[0]->GetCoordinates()[1];
  z0 = this->m_node[0]->GetCoordinates()[2];

  x1 = this->m_node[1]->GetCoordinates()[0];
  y1 = this->m_node[1]->GetCoordinates()[1];
  z1 = this->m_node[1]->GetCoordinates()[2];

  x2 = this->m_node[2]->GetCoordinates()[0];
  y2 = this->m_node[2]->GetCoordinates()[1];
  z2 = this->m_node[2]->GetCoordinates()[2];

  x3 = this->m_node[3]->GetCoordinates()[0];
  y3 = this->m_node[3]->GetCoordinates()[1];
  z3 = this->m_node[3]->GetCoordinates()[2];

  A = ( x1 - x0 ) * ( ( y2 - y0 ) * ( z3 - z0 ) - ( z2 - z0 ) * ( y3 - y0 ) )
    - ( x2 - x0 ) * ( ( y1 - y0 ) * ( z3 - z0 ) - ( z1 - z0 ) * ( y3 - y0 ) )
    + ( x3 - x0 ) * ( ( y1 - y0 ) * ( z2 - z0 ) - ( z1 - z0 ) * ( y2 - y0 ) );

  localPt[0] = 1 / A
    * (
      ( x - x0 ) * ( ( y2 - y0 ) * ( z3 - z0 ) - ( z2 - z0 ) * ( y3 - y0 ) )
      - ( y - y0 ) * ( ( x2 - x0 ) * ( z3 - z0 ) - ( z2 - z0 ) * ( x3 - x0 ) )
      + ( z - z0 ) * ( ( x2 - x0 ) * ( y3 - y0 ) - ( y2 - y0 ) * ( x3 - x0 ) )
      );

  localPt[1] = 1 / A
    * (
      -( x - x0 ) * ( ( y1 - y0 ) * ( z3 - z0 ) - ( z1 - z0 ) * ( y3 - y0 ) )
      + ( y - y0 ) * ( ( x1 - x0 ) * ( z3 - z0 ) - ( z1 - z0 ) * ( x3 - x0 ) )
      - ( z - z0 ) * ( ( x1 - x0 ) * ( y3 - y0 ) - ( y1 - y0 ) * ( x3 - x0 ) )
      );

  localPt[2] = 1 / A
    * (
      ( x - x0 ) * ( ( y1 - y0 ) * ( z2 - z0 ) - ( z1 - z0 ) * ( y2 - y0 ) )
      - ( y - y0 ) * ( ( x1 - x0 ) * ( z2 - z0 ) - ( z1 - z0 ) * ( x2 - x0 ) )
      + ( z - z0 ) * ( ( x1 - x0 ) * ( y2 - y0 ) - ( y1 - y0 ) * ( x2 - x0 ) )
      );

  const double FEM_TETRA_EPSILON = 1e-5;

  if( localPt[0] < ( 0.0 - FEM_TETRA_EPSILON )
      || localPt[0] > ( 1.0 + FEM_TETRA_EPSILON )
      || localPt[1] < ( 0.0 - FEM_TETRA_EPSILON )
      || localPt[1] > ( 1.0 + FEM_TETRA_EPSILON )
      || localPt[2] < ( 0.0 - FEM_TETRA_EPSILON )
      || localPt[2] > ( 1.0 + FEM_TETRA_EPSILON )
      || ( ( localPt[0] + localPt[1] + localPt[2] ) > ( 1.0 + FEM_TETRA_EPSILON ) ) )
    {
    return false;
    }
  else
    {
    return true;
    }
}

void Element3DC0LinearTetrahedron::PopulateEdgeIds(void)
{
  this->m_EdgeIds.resize(0);

  std::vector<int> edgePtIds;
  edgePtIds.resize(2);

  // edge 0
  edgePtIds[0] = 0;
  edgePtIds[1] = 1;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 1
  edgePtIds[0] = 1;
  edgePtIds[1] = 2;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 2
  edgePtIds[0] = 2;
  edgePtIds[1] = 0;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 3
  edgePtIds[0] = 0;
  edgePtIds[1] = 3;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 4
  edgePtIds[0] = 1;
  edgePtIds[1] = 3;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 5
  edgePtIds[0] = 2;
  edgePtIds[1] = 3;
  this->m_EdgeIds.push_back(edgePtIds);
}

void
Element3DC0LinearTetrahedron::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
