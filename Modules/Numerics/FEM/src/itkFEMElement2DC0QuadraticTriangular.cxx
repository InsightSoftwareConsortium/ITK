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

#include "itkFEMElement2DC0QuadraticTriangular.h"

#include "itkFEMElement2DC0LinearTriangularMembrane.h"

namespace itk
{
namespace fem
{
void
Element2DC0QuadraticTriangular
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{
  // default integration order
  if( order == 0 || order > 5 )
    {
    order = DefaultIntegrationOrder;
    }

  pt.set_size(3);

  /**
   * We provide implementation for 5 different integration rules
   * as defined in chapter 24 - Implementation of Iso-P Truangular
   * Elements, of http://titan.colorado.edu/courses.d/IFEM.d/.
   *
   * Note that the order parameter here does not correspond to the
   * actual order of integration, but rather the degree of polynomials
   * that are exactly integrated. In addition, there are two integration
   * rules for polynomials of 2nd degree. In order to allow using both of
   * them, we assign the index number 3 to the second one. Note that this
   * does not mean that the rule is capable of integrating the polynomials
   * of 3rd degree. It's just an index of a rule.
   */
  pt.copy_in(Element2DC0LinearTriangular::trigGaussRuleInfo[order][i]);

  // We scale the weight by 0.5, to take into account
  // the factor that must be applied when integrating.
  w = 0.5 * Element2DC0LinearTriangular::trigGaussRuleInfo[order][i][3];
}

unsigned int
Element2DC0QuadraticTriangular
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order
  if( order == 0 || order > 5 )
    {
    order = DefaultIntegrationOrder;
    }

  return Element2DC0LinearTriangular::Nip[order];
}

Element2DC0QuadraticTriangular::VectorType
Element2DC0QuadraticTriangular
::ShapeFunctions(const VectorType & pt) const
{
  // Quadratic triangular element has 6 shape functions
  VectorType shapeF(6);

  // Shape functions are equal to coordinates
  VectorType::element_type p2 = 1.0 - pt[0] - pt[1];

  shapeF[0] = pt[0] * ( 2 * pt[0] - 1 );
  shapeF[1] = pt[1] * ( 2 * pt[1] - 1 );
  shapeF[2] = p2 * ( 2 * p2 - 1 );
  shapeF[3] = 4 * pt[0] * pt[1];
  shapeF[4] = 4 * pt[1] * p2;
  shapeF[5] = 4 * p2 * pt[0];

  return shapeF;
}

void
Element2DC0QuadraticTriangular
::ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const
{
  VectorType::element_type p2 = 1.0 - pt[0] - pt[1];

  shapeD.set_size(3, 6);
  shapeD.fill(0.0);

  shapeD[0][0] = 4 * pt[0] - 1;
  shapeD[0][3] = 4 * pt[1];
  shapeD[0][5] = 4 * p2;

  shapeD[1][1] = 4 * pt[1] - 1;
  shapeD[1][3] = 4 * pt[0];
  shapeD[1][4] = 4 * p2;

  shapeD[2][2] = 4 * p2 - 1;
  shapeD[2][4] = 4 * pt[1];
  shapeD[2][5] = 4 * pt[0];
}

Element2DC0QuadraticTriangular::Float
Element2DC0QuadraticTriangular
::JacobianDeterminant(const VectorType & pt, const MatrixType *pJ) const
{
//  return Superclass::JacobianDeterminant( pt, pJ );

  MatrixType *pJlocal = ITK_NULLPTR;

  // If Jacobian was not provided, we
  // need to compute it here
  if( pJ == ITK_NULLPTR )
    {
    pJlocal = new MatrixType();
    this->Jacobian(pt, *pJlocal);
    pJ = pJlocal;
    }

  Float det = ( ( ( *pJ )[1][0] - ( *pJ )[0][0] ) * ( ( *pJ )[2][1] - ( *pJ )[0][1] ) )
    - ( ( ( *pJ )[0][1] - ( *pJ )[1][1] ) * ( ( *pJ )[0][0] - ( *pJ )[2][0] ) );

  delete pJlocal;

  return det;
}

void
Element2DC0QuadraticTriangular
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

  // Note that inverse of Jacobian is not quadratic matrix
  invJ.set_size(2, 3);

  Float idet = 1.0 / this->JacobianDeterminant(pt, pJ);

  invJ[0][0] = idet * ( ( *pJ )[1][1] - ( *pJ )[2][1] ); invJ[0][1] = idet * ( ( *pJ )[2][1] - ( *pJ )[0][1] );
  invJ[0][2] = idet * ( ( *pJ )[0][1] - ( *pJ )[1][1] );
  invJ[1][0] = idet * ( ( *pJ )[2][0] - ( *pJ )[1][0] ); invJ[1][1] = idet * ( ( *pJ )[0][0] - ( *pJ )[2][0] );
  invJ[1][2] = idet * ( ( *pJ )[1][0] - ( *pJ )[0][0] );

  delete pJlocal;
}

bool Element2DC0QuadraticTriangular::GetLocalFromGlobalCoordinates(
  const VectorType & GlobalPt, VectorType & LocalPt) const
{
  // connectivity is based on how the nodes are stored
  int LinearTris[4][3] = { {0, 3, 5}, {3, 1, 4}, {5, 4, 2}, {4, 5, 3} };

  VectorType pc(3);
  int        i, subId;
  bool       returnStatus = false;
  VectorType tempWeights(3);
  VectorType closest(3);

  itk::fem::Element2DC0LinearTriangularMembrane::Pointer e1;

  e1 = itk::fem::Element2DC0LinearTriangularMembrane::New();
  // four linear triangles are used
  for( i = 0; i < 4; i++ )
    {
    e1->SetNode(0, this->GetNode(LinearTris[i][0]) );
    e1->SetNode(1, this->GetNode(LinearTris[i][1]) );
    e1->SetNode(2, this->GetNode(LinearTris[i][2]) );

    returnStatus = e1->GetLocalFromGlobalCoordinates(GlobalPt, pc);
    if( returnStatus )
      {
      subId = i;
      LocalPt[0] = pc[0];
      LocalPt[1] = pc[1];
      break;
      }
    }

  // adjust parametric coordinates
  if( returnStatus )
    {
    if( subId == 0 )
      {
      LocalPt[0] /= 2.0;
      LocalPt[1] /= 2.0;
      }
    else if( subId == 1 )
      {
      LocalPt[0] = 0.5 + (LocalPt[0] / 2.0);
      LocalPt[1] /= 2.0;
      }
    else if( subId == 2 )
      {
      LocalPt[0] /= 2.0;
      LocalPt[1] = 0.5 + (LocalPt[1] / 2.0);
      }
    else
      {
      LocalPt[0] = 0.5 - LocalPt[0] / 2.0;
      LocalPt[1] = 0.5 - LocalPt[1] / 2.0;
      }
    LocalPt[2] = 1.0 - LocalPt[0] - LocalPt[1];
    }

  return returnStatus;
}

void Element2DC0QuadraticTriangular::PopulateEdgeIds(void)
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

void Element2DC0QuadraticTriangular::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
