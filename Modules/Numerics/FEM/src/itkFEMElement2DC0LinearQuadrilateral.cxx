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

#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "vnl/vnl_math.h"

namespace itk
{
namespace fem
{
void
Element2DC0LinearQuadrilateral
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{
  // default integration order
  if( ( order == 0 ) || (order >= Element::gaussMaxOrder) )
    {
    order = DefaultIntegrationOrder;
    }

  pt.set_size(2);

  pt[0] = gaussPoint[order][i % order];
  pt[1] = gaussPoint[order][i / order];

  w = gaussWeight[order][i % order] * gaussWeight[order][i / order];
}

unsigned int
Element2DC0LinearQuadrilateral
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order
  if( ( order == 0 ) || (order >= Element::gaussMaxOrder) )
    {
    order = DefaultIntegrationOrder;
    }

  return order * order;
}

Element2DC0LinearQuadrilateral::VectorType
Element2DC0LinearQuadrilateral
::ShapeFunctions(const VectorType & pt) const
{
  /* Linear quadrilateral element has four shape functions  */
  VectorType shapeF(4);

  /**
   * Linear quadrilateral element has local coordinates
   * (-1,-1), (1,-1), (1,1), and (-1,1)
   */

  /* given local point x=(r,s), where -1 <= r,s <= 1 and */

  /* shape function 1: ((1 - r) * (1 - s)) / 4  (node 1) */
  shapeF[0] = ( 1 - pt[0] ) * ( 1 - pt[1] ) * .25;

  /* shape function 2: ((1 + r) * (1 - s)) / 4  (node 2) */
  shapeF[1] = ( 1 + pt[0] ) * ( 1 - pt[1] ) * .25;

  /* shape function 3: ((1 + r) * (1 + s)) / 4  (node 3) */
  shapeF[2] = ( 1 + pt[0] ) * ( 1 + pt[1] ) * .25;

  /* shape function 1: ((1 - r) * (1 + s)) / 4  (node 4) */
  shapeF[3] = ( 1 - pt[0] ) * ( 1 + pt[1] ) * .25;

  return shapeF;
}

void
Element2DC0LinearQuadrilateral
::ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const
{
  /** functions at directions r and s.  */
  shapeD.set_size(2, 4);

  /** Derivative w.r.t r for shape function 1 (node 1) */
  shapeD[0][0] = -( 1 - pt[1] ) * .25;

  /** Derivative w.r.t s for shape function 1 (node 1) */
  shapeD[1][0] = -( 1 - pt[0] ) * .25;

  /** Derivative w.r.t r for shape function 2 (node 2) */
  shapeD[0][1] = +( 1 - pt[1] ) * .25;

  /** Derivative w.r.t s for shape function 2 (node 2) */
  shapeD[1][1] = -( 1 + pt[0] ) * .25;

  /** Derivative w.r.t r for shape function 3 (node 3) */
  shapeD[0][2] = +( 1 + pt[1] ) * .25;

  /** Derivative w.r.t s for shape function 3 (node 3) */
  shapeD[1][2] = +( 1 + pt[0] ) * .25;

  /** Derivative w.r.t r for shape function 4 (node 4) */
  shapeD[0][3] = -( 1 + pt[1] ) * .25;

  /** Derivative w.r.t s for shape function 4 (node 4) */
  shapeD[1][3] = +( 1 - pt[0] ) * .25;
}

bool
Element2DC0LinearQuadrilateral
::GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const
{
  Float x1, x2, x3, x4, y1, y2, y3, y4, xce, yce, xb, yb, xcn, ycn,
        A, J1, J2, x0, y0, dx, dy, be, bn, ce, cn;

  localPt.set_size(2);
  localPt.fill(0.0);

  x1 = this->m_node[0]->GetCoordinates()[0];   y1 = this->m_node[0]->GetCoordinates()[1];
  x2 = this->m_node[1]->GetCoordinates()[0];   y2 = this->m_node[1]->GetCoordinates()[1];
  x3 = this->m_node[2]->GetCoordinates()[0];   y3 = this->m_node[2]->GetCoordinates()[1];
  x4 = this->m_node[3]->GetCoordinates()[0];   y4 = this->m_node[3]->GetCoordinates()[1];

  xb = x1 - x2 + x3 - x4;
  yb = y1 - y2 + y3 - y4;

  xce = x1 + x2 - x3 - x4;
  yce = y1 + y2 - y3 - y4;

  xcn = x1 - x2 - x3 + x4;
  ycn = y1 - y2 - y3 + y4;

  A  = 0.5 * (((x3 - x1) * (y4 - y2)) - ((x4 - x2) * (y3 - y1)));
  J1 = ((x3 - x4) * (y1 - y2)) - ((x1 - x2) * (y3 - y4));
  J2 = ((x2 - x3) * (y1 - y4)) - ((x1 - x4) * (y2 - y3));

  x0 = 0.25 * (x1 + x2 + x3 + x4);
  y0 = 0.25 * (y1 + y2 + y3 + y4);

  dx = globalPt[0] - x0;
  dy = globalPt[1] - y0;

  be =  A - (dx * yb) + (dy * xb);
  bn = -A - (dx * yb) + (dy * xb);
  ce = (dx * yce) - (dy * xce);
  cn = (dx * ycn) - (dy * xcn);

  localPt[0] = (2 * ce) / (-vcl_sqrt((be * be) - (2 * J1 * ce)) - be);
  localPt[1] = (2 * cn) / ( vcl_sqrt((bn * bn) + (2 * J2 * cn)) - bn);

  bool isInside=true;

  if (localPt[0] < -1.0 || localPt[0] > 1.0 || localPt[1] < -1.0 || localPt[1] > 1.0 )
    {
    isInside=false;
    }

  return isInside;

#if 0
  int    MAX_ITERATION = 20;
  double CONVERGED = 1.e-04;
  Float  DIVERGED = 1.0e08;

  int        i, j;
  int        iteration, converged;
  double     params[2];
  VectorType fcol(2), rcol(2), scol(2), cp(3);
  VectorType pt;
  VectorType derivs(8);
  VectorType weights(4);

  localPt[0] = localPt[1] = params[0] = params[1] = 0.5;

  std::cout << "GetLocalFromGlobalCoordinates" << std::endl;
  std::cout << "Global Point " << globalPt[0] << " " << globalPt[1] << std::endl;

  for( i = 0; i < 4; i++ )
    {
    pt = this->m_node[i]->GetCoordinates();
    std::cout << "Node Points " << i << " " << pt[0] << " " << pt[1] << std::endl;
    }
  // Use Newton's method to solve for parametric coordinates
  //
  for( iteration = converged = 0; !converged
       && (iteration < MAX_ITERATION);
       iteration++ )
    {
    //  calculate element interpolation functions and derivatives
    //
    this->InterpolationFunctions(localPt, weights);
    this->InterpolationDerivs(localPt, derivs);
    //  calculate newton functions
    //
    for( i = 0; i < 2; i++ )
      {
      fcol[i] = rcol[i] = scol[i] = 0.0;
      }
    for( i = 0; i < 4; i++ )
      {
      pt = this->m_node[i]->GetCoordinates();
      for( j = 0; j < 2; j++ )
        {
        fcol[j] += pt[j] * weights[i];
        rcol[j] += pt[j] * derivs[i];
        scol[j] += pt[j] * derivs[i + 4];
        }
      }
    for( j = 0; j < 2; j++ )
      {
      fcol[j] -= globalPt[j];
      }

    //  compute determinants and generate improvements
    //
    double det = this->Determinant2x2(rcol, scol);
    if( det < 1e-20 )
      {
      return false;
      }

    localPt[0] = params[0] - this->Determinant2x2(fcol, scol) / det;
    localPt[1] = params[1] - this->Determinant2x2(rcol, fcol) / det;
    std::cout << "Iteration " << iteration << " Local Pt: " << localPt[0] << " " << localPt[1] << std::endl;
    std::cout << "Iteration " << iteration << " Params: " << params[0] << " " << params[1] << std::endl;
    //  check for convergence
    //
    if( ( (fabs(localPt[0] - params[0]) ) < CONVERGED) &&
        ( (fabs(localPt[1] - params[1]) ) < CONVERGED) )
      {
      converged = 1;
      }
    // Test for bad divergence (S.Hirschberg 11.12.2001)
    else if( (fabs(localPt[0]) > DIVERGED) ||
             (fabs(localPt[1]) > DIVERGED) )
      {
      return false;
      }

    //  if not converged, repeat
    //
    else
      {
      params[0] = localPt[0];
      params[1] = localPt[1];
      }
    }

  //  if not converged, set the parametric coordinates to arbitrary values
  //  outside of element
  //
  if( !converged )
    {
    return false;
    }
  return true;
#endif
}

void Element2DC0LinearQuadrilateral::PopulateEdgeIds(void)
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
}

void Element2DC0LinearQuadrilateral::InterpolationFunctions(const VectorType & pcoords, VectorType & sf) const
{
  double rm, sm;

  rm = 1. - pcoords[0];
  sm = 1. - pcoords[1];

  sf[0] = rm * sm;
  sf[1] = pcoords[0] * sm;
  sf[2] = pcoords[0] * pcoords[1];
  sf[3] = rm * pcoords[1];
}

void Element2DC0LinearQuadrilateral::InterpolationDerivs(const VectorType & pcoords, VectorType & derivs) const
{
  double rm, sm;

  rm = 1. - pcoords[0];
  sm = 1. - pcoords[1];

  derivs[0] = -sm;
  derivs[1] = sm;
  derivs[2] = pcoords[1];
  derivs[3] = -pcoords[1];
  derivs[4] = -rm;
  derivs[5] = -pcoords[0];
  derivs[6] = pcoords[0];
  derivs[7] = rm;
}

itk::fem::Element::Float Element2DC0LinearQuadrilateral::Determinant2x2(const VectorType & c1,
                                                                        const VectorType & c2) const
{
  return c1[0] * c2[1] - c2[0] - c1[1];
}

void Element2DC0LinearQuadrilateral::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
