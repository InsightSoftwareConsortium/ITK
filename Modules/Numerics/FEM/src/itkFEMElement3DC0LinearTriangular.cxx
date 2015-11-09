/*=========================================================================
*
* Copyright Insight Software Consortium
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0.txt
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*=========================================================================*/

#include "itkMath.h"

#include "itkFEMElement3DC0LinearTriangular.h"

#include "vnl/algo/vnl_qr.h"

namespace itk
{
namespace fem
{
const Element3DC0LinearTriangular::Float
Element3DC0LinearTriangular
::trigGaussRuleInfo[6][7][4] =
  {
    {   // order=0, never used
              { 0.0 }
    },
    { // order=1
      // <-------------------------- point --------------------------->
      // <-------weight----->
                   {  0.33333333333333333, 0.33333333333333333, 0.33333333333333333, 1.00000000000000000 }
    },

    {   // order=2
                   {  0.66666666666666667, 0.16666666666666667, 0.16666666666666667, 0.33333333333333333 },
                   {  0.16666666666666667, 0.66666666666666667, 0.16666666666666667, 0.33333333333333333 },
                   {  0.16666666666666667, 0.16666666666666667, 0.66666666666666667, 0.33333333333333333 }
    },

    {   // order=3, p=-3 in the book
                   {  0.00000000000000000, 0.50000000000000000, 0.50000000000000000, 0.33333333333333333 },
                   {  0.50000000000000000, 0.00000000000000000, 0.50000000000000000, 0.33333333333333333 },
                   {  0.50000000000000000, 0.50000000000000000, 0.00000000000000000, 0.33333333333333333 }
    },

    {   // order=4, p=6 in the book
                   {  0.10810301816807023, 0.44594849091596489, 0.44594849091596489, 0.22338158967801147 },
                   {  0.44594849091596489, 0.10810301816807023, 0.44594849091596489, 0.22338158967801147 },
                   {  0.44594849091596489, 0.44594849091596489, 0.10810301816807023, 0.22338158967801147 },
                   {  0.81684757298045851, 0.09157621350977074, 0.09157621350977074, 0.10995174365532187 },
                   {  0.09157621350977074, 0.81684757298045851, 0.09157621350977074, 0.10995174365532187 },
                   {  0.09157621350977074, 0.09157621350977074, 0.81684757298045851, 0.10995174365532187 }
    },

    {   // order=5, p=7 in the book
                   {  0.33333333333333333, 0.33333333333333333, 0.33333333333333333, 0.22500000000000000 },
                   {  0.79742698535308732, 0.10128650732345634, 0.10128650732345634, 0.12593918054482715 },
                   {  0.10128650732345634, 0.79742698535308732, 0.10128650732345634, 0.12593918054482715 },
                   {  0.10128650732345634, 0.10128650732345634, 0.79742698535308732, 0.12593918054482715 },
                   {  0.05971587178976982, 0.47014206410511509, 0.47014206410511509, 0.13239415278850618 },
                   {  0.47014206410511509, 0.05971587178976982, 0.47014206410511509, 0.13239415278850618 },
                   {  0.47014206410511509, 0.47014206410511509, 0.05971587178976982, 0.13239415278850618 }
    }
  };

const unsigned int
Element3DC0LinearTriangular
::Nip[6] =
  {
  0, 1, 3, 3, 6, 7
  };

void
Element3DC0LinearTriangular
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{
  // default integration order
  if( order == 0 || order > 5 )
    {
    order = DefaultIntegrationOrder;
    }

  pt.set_size(3);

  /*
   * We provide implementation for 5 different integration rules
   * as defined in chapter 24 - Implementation of Iso-P Truangular
   * Elements, of http://titan.colorado.edu/courses.d/IFEM.d/.
   *
   * One more reference - "Structural analysis with the finite element method - Linear statics" by Eugene onate
   * Section 6.4.2 - Numerical integration over triangles.
   * Note that the order parameter here does not correspond to the
   * actual order of integration, but rather the degree of polynomials
   * that are exactly integrated. In addition, there are two integration
   * rules for polynomials of 2nd degree. In order to allow using both of
   * them, we assign the index number 3 to the second one. Note that this
   * does not mean that the rule is capable of integrating the polynomials
   * of 3rd degree. It's just an index of a rule.
   */
  pt.copy_in(trigGaussRuleInfo[order][i]);

  // We scale the weight by 0.5, to take into account
  // the factor that must be applied when integrating.
  w = 0.5 * trigGaussRuleInfo[order][i][3];
}

unsigned int
Element3DC0LinearTriangular
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order
  if( order == 0 || order > 5 )
    {
    order = DefaultIntegrationOrder;
    }

  return Nip[order];
}

Element3DC0LinearTriangular::VectorType
Element3DC0LinearTriangular
::ShapeFunctions(const VectorType & pt) const
{
  // Linear triangular element has 3 shape functions
  VectorType shapeF(3);

  // Shape functions are equal to coordinates
  shapeF = pt;

  return shapeF;
}

void
Element3DC0LinearTriangular
::ShapeFunctionDerivatives(const VectorType &, MatrixType & shapeD) const
{
  // Matrix of shape functions derivatives is an
  // identity matrix for linear triangular element.
  shapeD.set_size(3, 3);
  shapeD.fill(0.0);
  shapeD[0][0] = 1.0;
  shapeD[1][1] = 1.0;
  shapeD[2][2] = 1.0;
}

bool
Element3DC0LinearTriangular
::GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const
{
  int        i, j;
  VectorType pt1, pt2, pt3, n(3);
  Float      fabsn;
  VectorType rhs(2), c1(2), c2(2);
  Float      det;
  Float      maxComponent;
  int        idx = 0, indices[2];
  VectorType closest, closestPoint1(3), closestPoint2(3), cp(3);

  // Get normal for triangle, only the normal direction is needed, i.e. the
  // normal need not be normalized (unit length)
  //
  pt1 = this->m_node[1]->GetCoordinates();
  pt2 = this->m_node[2]->GetCoordinates();
  pt3 = this->m_node[0]->GetCoordinates();

  this->ComputeNormalDirection(pt1, pt2, pt3, n);

  // Project point to plane
  //
  this->GeneralizedProjectPoint(globalPt, pt1, n, cp);
  // Construct matrices.  Since we have over determined system, need to find
  // which 2 out of 3 equations to use to develop equations. (Any 2 should
  // work since we've projected point to plane.)
  //
  for( maxComponent = 0.0, i = 0; i < 3; i++ )
    {
    // trying to avoid an expensive call to fabs()
    if( n[i] < 0 )
      {
      fabsn = -n[i];
      }
    else
      {
      fabsn = n[i];
      }
    if( fabsn > maxComponent )
      {
      maxComponent = fabsn;
      idx = i;
      }
    }
  for( j = 0, i = 0; i < 3; i++ )
    {
    if( i != idx )
      {
      indices[j++] = i;
      }
    }
  for( i = 0; i < 2; i++ )
    {
    rhs[i] = cp[indices[i]] - pt3[indices[i]];
    c1[i] = pt1[indices[i]] - pt3[indices[i]];
    c2[i] = pt2[indices[i]] - pt3[indices[i]];
    }

  if( (det = this->Determinant2x2(c1, c2) ) == 0.0 )
    {
    localPt[0] = localPt[1] = localPt[2] = 0.0;
    return false;
    }

  localPt[0] = this->Determinant2x2(rhs, c2) / det;
  localPt[1] = this->Determinant2x2(c1, rhs) / det;
  localPt[2] = 1.0 - (localPt[0] + localPt[1]);

  if( localPt[0] >= 0.0 && localPt[0] <= 1.0 &&
      localPt[1] >= 0.0 && localPt[1] <= 1.0 &&
      localPt[2] >= 0.0 && localPt[2] <= 1.0 )
    {
    return true;
    }
  else
    {
    return false;
    }
}

Element3DC0LinearTriangular::Float
Element3DC0LinearTriangular
::JacobianDeterminant(const VectorType & /*HACK pt*/, const MatrixType * /*HACK pJ*/) const
{
  // use heron's formula
  const int na = 0;
  const int nb = 1;
  const int nc = 2;

  const VectorType &A = this->GetNode(na)->GetCoordinates();
  const VectorType &B = this->GetNode(nb)->GetCoordinates();
  const VectorType &C = this->GetNode(nc)->GetCoordinates();
  const VectorType &BA = B - A;
  const VectorType &CA = C - A;
  const VectorType &CB = C - B;
  const float L1 = CB.magnitude();
  const float L2 = CA.magnitude();
  const float L3 = BA.magnitude();

  const float s = ( L1 + L2 + L3 ) * .5;
  Float det = sqrt( s * ( s - L1 ) * ( s - L2 ) * ( s - L3 ) );

/*
// use the formula for tri pqr, area is mag( vec(pq) cross vec(pr) )
  VectorType a=this->GetNode(2)->GetCoordinates()-this->GetNode(0)->GetCoordinates();
  VectorType b=this->GetNode(1)->GetCoordinates()-this->GetNode(0)->GetCoordinates();

  VectorType c;
  c.set_size(3);

  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];

  Float det=0.5*c.magnitude();
  */
//  std::cout << " area " << det << std::endl;
  return det;
}

void
Element3DC0LinearTriangular
::JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType *pJ) const
{
  MatrixType *pJlocal = ITK_NULLPTR;

  // If Jacobian was not provided, we
  // need to compute it here
  if( pJ == ITK_NULLPTR )
    {
    pJlocal = new MatrixType();
    this->Jacobian(pt, *pJlocal);
    pJ = pJlocal;
    }

//  invJ=vnl_svd_inverse<Float>(*pJ);
  invJ = vnl_qr<Float>(*pJ).inverse();

  /*
// Note that inverse of Jacobian is not quadratic matrix
MatrixType invJ2;
invJ2.set_size(3,3);
invJ2.fill(0);

Float idet=1.0/this->JacobianDeterminant( pt, pJ );
invJ2[0][0]=idet*((*pJ)[1][1]-(*pJ)[2][1]);
invJ2[0][1]=idet*((*pJ)[2][1]-(*pJ)[0][1]);
invJ2[0][2]=idet*((*pJ)[0][1]-(*pJ)[1][1]);
invJ2[1][0]=idet*((*pJ)[2][0]-(*pJ)[1][0]);
invJ2[1][1]=idet*((*pJ)[0][0]-(*pJ)[2][0]);
invJ2[1][2]=idet*((*pJ)[1][0]-(*pJ)[0][0]);

std::cout << " pJ " << std::endl;
std::cout << (*pJ) << std::endl;

std::cout << " invJ " << std::endl;
std::cout << (invJ) << std::endl;

std::cout << " invJ2 " << std::endl;
std::cout << (invJ2) << std::endl;*/

  delete pJlocal;
}

void Element3DC0LinearTriangular::ComputeNormalDirection(const VectorType & v1, const VectorType & v2,
                                                         const VectorType & v3, VectorType & n) const
{
  Float ax, ay, az, bx, by, bz;

  // order is important!!! maintain consistency with triangle vertex order
  ax = v3[0] - v2[0]; ay = v3[1] - v2[1]; az = v3[2] - v2[2];
  bx = v1[0] - v2[0]; by = v1[1] - v2[1]; bz = v1[2] - v2[2];

  n[0] = (ay * bz - az * by);
  n[1] = (az * bx - ax * bz);
  n[2] = (ax * by - ay * bx);
}

void Element3DC0LinearTriangular::GeneralizedProjectPoint(const VectorType & x, const VectorType & origin,
                                                          const VectorType & normal, VectorType & xproj) const
{
  double t, xo[3], n2;

  xo[0] = x[0] - origin[0];
  xo[1] = x[1] - origin[1];
  xo[2] = x[2] - origin[2];

  t = normal[0] * xo[0] + normal[1] * xo[1] + normal[2] * xo[2];
  n2 = normal[0] * normal[0] + normal[1] * normal[1] + normal[2] * normal[2];

  if( Math::NotAlmostEquals(n2, 0) )
    {
    xproj[0] = x[0] - t * normal[0] / n2;
    xproj[1] = x[1] - t * normal[1] / n2;
    xproj[2] = x[2] - t * normal[2] / n2;
    }
  else
    {
    xproj[0] = x[0];
    xproj[1] = x[1];
    xproj[2] = x[2];
    }
}

itk::fem::Element::Float Element3DC0LinearTriangular::Determinant2x2(
  const VectorType & c1, const VectorType & c2)  const
{
  return c1[0] * c2[1] - c2[0] * c1[1];
}

void Element3DC0LinearTriangular::PopulateEdgeIds()
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
  edgePtIds[0] = 0;
  edgePtIds[1] = 2;
  this->m_EdgeIds.push_back(edgePtIds);
}

void
Element3DC0LinearTriangular::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
