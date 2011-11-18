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

#include "itkFEMElement3DC0LinearHexahedron.h"

namespace itk
{
namespace fem
{
void
Element3DC0LinearHexahedron
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{
  if( order == 0 || order > 9 )
    {
    order = 2;
    }

  pt.set_size(3);
  pt[0] = gaussPoint[order][i % order];
  pt[1] = gaussPoint[order][( i / order ) % order];
  pt[2] = gaussPoint[order][( i / ( order * order ) )];

  w =
    gaussWeight[order][i
                       % order]
    * gaussWeight[order][( i / order ) % order] * gaussWeight[order][( i / ( order * order ) )];
}

unsigned int
Element3DC0LinearHexahedron
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order=2
  if( order == 0 || order > 9 )
    {
    order = 2;
    }

  return order * order * order;
}

Element3DC0LinearHexahedron::VectorType
Element3DC0LinearHexahedron
::ShapeFunctions(const VectorType & pt) const
{
  /* Linear hexahedral element has eight shape functions  */
  VectorType shapeF(8);

  /**
   * Linear hexahedral element has local coordinates
   *  (-1,-1,-1), (1,-1,-1), (1,1,-1), (-1,1,-1), (-1,-1,1), (1,-1,1), (1,1,1), (-1,1,1)
   */

  /* given local point x=(r,s,t), where -1 <= r,s,t <= 1 and */

  /** N_1 = ((1-r) * (1-s) * (1-t)) / 8 */
  shapeF[0] = ( 1 - pt[0] ) * ( 1 - pt[1] ) * ( 1 - pt[2] ) * 0.125;

  /** N_2 = ((1+r) * (1-s) * (1-t)) / 8 */
  shapeF[1] = ( 1 + pt[0] ) * ( 1 - pt[1] ) * ( 1 - pt[2] ) * 0.125;

  /** N_3 = ((1+r) * (1+s) * (1-t)) / 8 */
  shapeF[2] = ( 1 + pt[0] ) * ( 1 + pt[1] ) * ( 1 - pt[2] ) * 0.125;

  /** N_4 = ((1-r) * (1+s) * (1-t)) / 8 */
  shapeF[3] = ( 1 - pt[0] ) * ( 1 + pt[1] ) * ( 1 - pt[2] ) * 0.125;

  /** N_5 = ((1-r) * (1-s) * (1+t)) / 8 */
  shapeF[4] = ( 1 - pt[0] ) * ( 1 - pt[1] ) * ( 1 + pt[2] ) * 0.125;

  /** N_6 = ((1+r) * (1-s) * (1+t)) / 8 */
  shapeF[5] = ( 1 + pt[0] ) * ( 1 - pt[1] ) * ( 1 + pt[2] ) * 0.125;

  /** N_7 = ((1+r) * (1+s) * (1+t)) / 8 */
  shapeF[6] = ( 1 + pt[0] ) * ( 1 + pt[1] ) * ( 1 + pt[2] ) * 0.125;

  /** N_8 = ((1-r) * (1+s) * (1+t)) / 8 */
  shapeF[7] = ( 1 - pt[0] ) * ( 1 + pt[1] ) * ( 1 + pt[2] ) * 0.125;

  return shapeF;
}

void
Element3DC0LinearHexahedron
::ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const
{
  /** functions at directions r and s.  */
  shapeD.set_size(3, 8);

  // d(N_1) / d(r)
  shapeD[0][0] = ( -1 ) * ( 1 - pt[1] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_1) / d(s)
  shapeD[1][0] = ( -1 ) * ( 1 - pt[0] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_1) / d(t)
  shapeD[2][0] = ( -1 ) * ( 1 - pt[0] ) * ( 1 - pt[1] ) * 0.125;

  // d(N_2) / d(r)
  shapeD[0][1] = ( +1 ) * ( 1 - pt[1] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_2) / d(s)
  shapeD[1][1] = ( -1 ) * ( 1 + pt[0] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_2) / d(t)
  shapeD[2][1] = ( -1 ) * ( 1 + pt[0] ) * ( 1 - pt[1] ) * 0.125;

  // d(N_3) / d(r)
  shapeD[0][2] = ( +1 ) * ( 1 + pt[1] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_3) / d(s)
  shapeD[1][2] = ( +1 ) * ( 1 + pt[0] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_3) / d(t)
  shapeD[2][2] = ( -1 ) * ( 1 + pt[0] ) * ( 1 + pt[1] ) * 0.125;

  // d(N_4) / d(r)
  shapeD[0][3] = ( -1 ) * ( 1 + pt[1] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_4) / d(s)
  shapeD[1][3] = ( +1 ) * ( 1 - pt[0] ) * ( 1 - pt[2] ) * 0.125;

  // d(N_4) / d(t)
  shapeD[2][3] = ( -1 ) * ( 1 - pt[0] ) * ( 1 + pt[1] ) * 0.125;

  // d(N_5) / d(r)
  shapeD[0][4] = ( -1 ) * ( 1 - pt[1] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_5) / d(s)
  shapeD[1][4] = ( -1 ) * ( 1 - pt[0] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_5) / d(t)
  shapeD[2][4] = ( +1 ) * ( 1 - pt[0] ) * ( 1 - pt[1] ) * 0.125;

  // d(N_6) / d(r)
  shapeD[0][5] = ( +1 ) * ( 1 - pt[1] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_6) / d(s)
  shapeD[1][5] = ( -1 ) * ( 1 + pt[0] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_6) / d(t)
  shapeD[2][5] = ( +1 ) * ( 1 + pt[0] ) * ( 1 - pt[1] ) * 0.125;

  // d(N_7) / d(r)
  shapeD[0][6] = ( +1 ) * ( 1 + pt[1] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_7) / d(s)
  shapeD[1][6] = ( +1 ) * ( 1 + pt[0] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_7) / d(t)
  shapeD[2][6] = ( +1 ) * ( 1 + pt[0] ) * ( 1 + pt[1] ) * 0.125;

  // d(N_8) / d(r)
  shapeD[0][7] = ( -1 ) * ( 1 + pt[1] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_8) / d(s)
  shapeD[1][7] = ( +1 ) * ( 1 - pt[0] ) * ( 1 + pt[2] ) * 0.125;

  // d(N_8) / d(t)
  shapeD[2][7] = ( +1 ) * ( 1 - pt[0] ) * ( 1 + pt[1] ) * 0.125;
}

bool
Element3DC0LinearHexahedron
::GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const
{
  int   MAX_ITERATIONS = 10;
  Float CONVERGED = 1.0e-03;
  Float DIVERGED = 1.0e06;

  int        iteration, converged;
  VectorType params(3);
  VectorType fcol(3), rcol(3), scol(3), tcol(3), closestPoint(3);
  int        i, j;
  Float      d;
  VectorType pt;
  VectorType derivs(24);
  VectorType weights(8);

  //  set initial position for Newton's method
  localPt[0] = localPt[1] = localPt[2] = params[0] = params[1] = params[2] = 0.5;
  //  enter iteration loop
  for( iteration = converged = 0;
       !converged && (iteration < MAX_ITERATIONS);  iteration++ )
    {
    //  calculate element interpolation functions and derivatives
    this->InterpolationFunctions(localPt, weights);
    this->InterpolationDerivs(localPt, derivs);
    //  calculate newton functions
    for( i = 0; i < 3; i++ )
      {
      fcol[i] = rcol[i] = scol[i] = tcol[i] = 0.0;
      }
    for( i = 0; i < 8; i++ )
      {
      pt = this->m_node[i]->GetCoordinates();
      for( j = 0; j < 3; j++ )
        {
        fcol[j] += pt[j] * weights[i];
        rcol[j] += pt[j] * derivs[i];
        scol[j] += pt[j] * derivs[i + 8];
        tcol[j] += pt[j] * derivs[i + 16];
        }
      }
    for( i = 0; i < 3; i++ )
      {
      fcol[i] -= globalPt[i];
      }

    //  compute determinants and generate improvements
    d = this->Determinant3x3(rcol, scol, tcol);
    if( fabs(d) < 1.e-20 )
      {
      return false;
      }

    localPt[0] = params[0] - this->Determinant3x3(fcol, scol, tcol) / d;
    localPt[1] = params[1] - this->Determinant3x3(rcol, fcol, tcol) / d;
    localPt[2] = params[2] - this->Determinant3x3(rcol, scol, fcol) / d;

    //  check for convergence
    if( ( (fabs(localPt[0] - params[0]) ) < CONVERGED) &&
        ( (fabs(localPt[1] - params[1]) ) < CONVERGED) &&
        ( (fabs(localPt[2] - params[2]) ) < CONVERGED) )
      {
      converged = 1;
      }

    // Test for bad divergence (S.Hirschberg 11.12.2001)
    else if( (fabs(localPt[0]) > DIVERGED) ||
             (fabs(localPt[1]) > DIVERGED) ||
             (fabs(localPt[2]) > DIVERGED) )
      {
      return false;
      }

    //  if not converged, repeat
    else
      {
      params[0] = localPt[0];
      params[1] = localPt[1];
      params[2] = localPt[2];
      }
    }

  //  if not converged, set the parametric coordinates to arbitrary values
  //  outside of element
  if( !converged )
    {
    return false;
    }

  this->InterpolationFunctions(localPt, weights);

  if( localPt[0] >= -0.001 && localPt[0] <= 1.001 &&
      localPt[1] >= -0.001 && localPt[1] <= 1.001 &&
      localPt[2] >= -0.001 && localPt[2] <= 1.001 )
    {
    closestPoint[0] = globalPt[0]; closestPoint[1] = globalPt[1]; closestPoint[2] = globalPt[2];
    return true;
    }
  else
    {
    VectorType pc(3);
    for( i = 0; i < 3; i++ ) // only approximate, not really true for warped hexa
      {
      if( localPt[i] < 0.0 )
        {
        pc[i] = 0.0;
        }
      else if( localPt[i] > 1.0 )
        {
        pc[i] = 1.0;
        }
      else
        {
        pc[i] = localPt[i];
        }
      }
    return 0;
    }
}

void Element3DC0LinearHexahedron::InterpolationFunctions(
  const VectorType & pcoords, VectorType & sf) const
{
  Float rm, sm, tm;

  rm = 1. - pcoords[0];
  sm = 1. - pcoords[1];
  tm = 1. - pcoords[2];

  sf[0] = rm * sm * tm;
  sf[1] = pcoords[0] * sm * tm;
  sf[2] = pcoords[0] * pcoords[1] * tm;
  sf[3] = rm * pcoords[1] * tm;
  sf[4] = rm * sm * pcoords[2];
  sf[5] = pcoords[0] * sm * pcoords[2];
  sf[6] = pcoords[0] * pcoords[1] * pcoords[2];
  sf[7] = rm * pcoords[1] * pcoords[2];
}

// ----------------------------------------------------------------------------
void Element3DC0LinearHexahedron::InterpolationDerivs(
  const VectorType & pcoords, VectorType & derivs) const
{
  Float rm, sm, tm;

  rm = 1. - pcoords[0];
  sm = 1. - pcoords[1];
  tm = 1. - pcoords[2];

  // r-derivatives
  derivs[0] = -sm * tm;
  derivs[1] = sm * tm;
  derivs[2] = pcoords[1] * tm;
  derivs[3] = -pcoords[1] * tm;
  derivs[4] = -sm * pcoords[2];
  derivs[5] = sm * pcoords[2];
  derivs[6] = pcoords[1] * pcoords[2];
  derivs[7] = -pcoords[1] * pcoords[2];

  // s-derivatives
  derivs[8] = -rm * tm;
  derivs[9] = -pcoords[0] * tm;
  derivs[10] = pcoords[0] * tm;
  derivs[11] = rm * tm;
  derivs[12] = -rm * pcoords[2];
  derivs[13] = -pcoords[0] * pcoords[2];
  derivs[14] = pcoords[0] * pcoords[2];
  derivs[15] = rm * pcoords[2];

  // t-derivatives
  derivs[16] = -rm * sm;
  derivs[17] = -pcoords[0] * sm;
  derivs[18] = -pcoords[0] * pcoords[1];
  derivs[19] = -rm * pcoords[1];
  derivs[20] = rm * sm;
  derivs[21] = pcoords[0] * sm;
  derivs[22] = pcoords[0] * pcoords[1];
  derivs[23] = rm * pcoords[1];
}

itk::fem::Element::Float Element3DC0LinearHexahedron::Determinant3x3(const VectorType & c1,
                                                                     const VectorType & c2,
                                                                     const VectorType & c3) const
{
  return c1[0] * c2[1] * c3[2] + c2[0] * c3[1] * c1[2] + c3[0] * c1[1] * c2[2]
         - c1[0] * c3[1] * c2[2] - c2[0] * c1[1] * c3[2] - c3[0] * c2[1] * c1[2];
}

void Element3DC0LinearHexahedron::PopulateEdgeIds(void)
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
  edgePtIds[0] = 3;
  edgePtIds[1] = 2;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 3
  edgePtIds[0] = 0;
  edgePtIds[1] = 3;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 4
  edgePtIds[0] = 4;
  edgePtIds[1] = 5;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 5
  edgePtIds[0] = 5;
  edgePtIds[1] = 6;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 6
  edgePtIds[0] = 6;
  edgePtIds[1] = 7;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 7
  edgePtIds[0] = 7;
  edgePtIds[1] = 4;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 8
  edgePtIds[0] = 0;
  edgePtIds[1] = 4;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 9
  edgePtIds[0] = 1;
  edgePtIds[1] = 5;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 10
  edgePtIds[0] = 2;
  edgePtIds[1] = 6;
  this->m_EdgeIds.push_back(edgePtIds);

  // edge 11
  edgePtIds[0] = 3;
  edgePtIds[1] = 7;
  this->m_EdgeIds.push_back(edgePtIds);
}

void
Element3DC0LinearHexahedron::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
