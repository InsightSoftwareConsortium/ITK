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

#include "itkFEMElementBase.h"
#include "vnl/algo/vnl_qr.h"

namespace itk
{
namespace fem
{

// ////////////////////////////////////////////////////////////////////////
/**
 * Physics of a problem.
 */

/*
 * Compute and return element stiffnes matrix (Ke) in global coordinate
 * system.
 * The base class provides a general implementation which only computes
 *
 *     b   T
 * int    B(x) D B(x) dx
 *     a
 *
 * using the Gaussian numeric integration method. The function calls
 * GetIntegrationPointAndWeight() / GetNumberOfIntegrationPoints() to obtain
 * the integration points. It also calls the GetStrainDisplacementMatrix()
 * and GetMaterialMatrix() member functions.
 *
 * \param Ke Reference to the resulting stiffnes matrix.
 *
 * \note This is a very generic implementation of the stiffness matrix
 *       that is suitable for any problem/element definition. A specifc
 *       element may override this implementation with its own simple one.
 */
void Element::GetStiffnessMatrix(MatrixType & Ke) const
{
  // B and D matrices
  MatrixType B, D;

  this->GetMaterialMatrix(D);

  unsigned int Nip = this->GetNumberOfIntegrationPoints();

  VectorType ip;
  Float      w;
  MatrixType J;
  MatrixType shapeDgl;
  MatrixType shapeD;

  // Calculate the contribution of 1st int. point to initialize
  // the Ke matrix to proper number of elements.
  this->GetIntegrationPointAndWeight(0, ip, w);
  this->ShapeFunctionDerivatives(ip, shapeD);
  this->Jacobian(ip, J, &shapeD);
  this->ShapeFunctionGlobalDerivatives(ip, shapeDgl, &J, &shapeD);

  this->GetStrainDisplacementMatrix(B, shapeDgl);
  Float detJ = this->JacobianDeterminant(ip, &J);
  Ke = detJ * w * B.transpose() * D * B; // FIXME: write a more efficient way of
                                         // computing this.
  // Add contributions of other int. points to the Ke
  for( unsigned int i = 1; i < Nip; i++ )
    {
    this->GetIntegrationPointAndWeight(i, ip, w);
    this->ShapeFunctionDerivatives(ip, shapeD);
    this->Jacobian(ip, J, &shapeD);
    this->ShapeFunctionGlobalDerivatives(ip, shapeDgl, &J, &shapeD);

    this->GetStrainDisplacementMatrix(B, shapeDgl);
    detJ = this->JacobianDeterminant(ip, &J);
    Ke += detJ * w * B.transpose() * D * B; // FIXME: write a more efficient way
                                            // of computing this.
    }
}

Element::VectorType Element::GetStrainsAtPoint(const VectorType & pt, const Solution & sol, unsigned int index) const
// NOTE: pt should be in local coordinates already
{
  MatrixType B;
  VectorType e, u;
  MatrixType J, shapeD, shapeDgl;

  this->ShapeFunctionDerivatives(pt, shapeD);
  this->Jacobian(pt, J, &shapeD);
  this->ShapeFunctionGlobalDerivatives(pt, shapeDgl, &J, &shapeD);
  this->GetStrainDisplacementMatrix(B, shapeDgl);

  u = this->InterpolateSolution(pt, sol, index);

  e = B * u;

  return e;
}

Element::VectorType Element::GetStressesAtPoint( const VectorType & itkNotUsed(pt),
                                                 const VectorType & e,
                                                 const Solution & itkNotUsed(sol),
                                                 unsigned int itkNotUsed(index) ) const
// NOTE: pt should be in local coordinates already
{
  MatrixType D;
  VectorType sigma;

  this->GetMaterialMatrix(D);

  sigma = D * e;

  return sigma;
}

void Element::GetLandmarkContributionMatrix(float eta, MatrixType & Le) const
{
  // Provides the contribution of a landmark to the element stiffness
  // matrix
  Le = MatrixType(this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0);

  const unsigned int NnDOF = this->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes = this->GetNumberOfNodes();
  const unsigned int NDOF = GetNumberOfDegreesOfFreedom();
  const unsigned int Nip = this->GetNumberOfIntegrationPoints(0);

  Le.set_size(NDOF, NDOF); // resize the target matrix object
  Le.fill(0.0);

  Float      w;
  VectorType ip, shape;
  for( unsigned int i = 0; i < Nip; i++ )
    {
    this->GetIntegrationPointAndWeight(i, ip, w, 0);
    shape = this->ShapeFunctions(ip);
    for( unsigned int ni = 0; ni < Nnodes; ni++ )
      {
      for( unsigned int nj = 0; nj < Nnodes; nj++ )
        {
        Float m = w * shape[ni] * shape[nj];
        for( unsigned int d = 0; d < NnDOF; d++ )
          {
          Le[ni * NnDOF + d][nj * NnDOF + d] += m;
          }
        }
      }
    }

  Le = Le / ( eta );
}

Element::Float Element::GetElementDeformationEnergy(MatrixType & LocalSolution) const
{
  MatrixType U;

  Element::MatrixType Ke;

  this->GetStiffnessMatrix(Ke);

  U = LocalSolution.transpose() * Ke * LocalSolution;

  return U[0][0];
}

void Element::GetMassMatrix(MatrixType & Me) const
{
  /*
   * If the function is not overiden, we compute consistent mass matrix
   * by integrating the shape functions over the element domain. The element
   * density is assumed one. If this is not the case, the GetMassMatrix in a
   * derived class must be overriden and the Me matrix corrected accordingly.
   */
  Me = MatrixType(this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0);

  const unsigned int NnDOF = this->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes = this->GetNumberOfNodes();
  const unsigned int NDOF = GetNumberOfDegreesOfFreedom();
  const unsigned int Nip = this->GetNumberOfIntegrationPoints(0);

  Me.set_size(NDOF, NDOF); // resize the target matrix object
  Me.fill(0.0);

  Float      w;
  VectorType ip, shape;
  MatrixType J, shapeD;
  for( unsigned int i = 0; i < Nip; i++ )
    {
    this->GetIntegrationPointAndWeight(i, ip, w, 0);
    shape = this->ShapeFunctions(ip);
    this->ShapeFunctionDerivatives(ip, shapeD);
    this->Jacobian(ip, J, &shapeD);
    Float detJ = this->JacobianDeterminant(ip, &J);
    for( unsigned int ni = 0; ni < Nnodes; ni++ )
      {
      for( unsigned int nj = 0; nj < Nnodes; nj++ )
        {
        Float m = detJ * w * shape[ni] * shape[nj];
        for( unsigned int d = 0; d < NnDOF; d++ )
          {
          Me[ni * NnDOF + d][nj * NnDOF + d] += m;
          }
        }
      }
    }
}

Element::VectorType
Element::InterpolateSolution(const VectorType & pt, const Solution & sol, unsigned int solutionIndex) const
{
  VectorType vec( GetNumberOfDegreesOfFreedomPerNode() );
  VectorType shapef = this->ShapeFunctions(pt);
  Float      value;

  const unsigned int Nnodes = this->GetNumberOfNodes();
  const unsigned int Ndofs_per_node = this->GetNumberOfDegreesOfFreedomPerNode();
  for( unsigned int f = 0; f < Ndofs_per_node; f++ )
    {
    value = 0.0;
    for( unsigned int n = 0; n < Nnodes; n++ )
      {
      value += shapef[n] * sol.GetSolutionValue(this->GetNode(n)->GetDegreeOfFreedom(f), solutionIndex);
      }

    vec[f] = value;
    }

  return vec;
}

Element::Float
Element::InterpolateSolutionN(const VectorType & pt, const Solution & sol, unsigned int f,
                              unsigned int solutionIndex) const
{
  Float value = 0.0;

  VectorType   shapef = this->ShapeFunctions(pt);
  unsigned int Nnodes = this->GetNumberOfNodes();
  for( unsigned int n = 0; n < Nnodes; n++ )
    {
    value += shapef[n] * sol.GetSolutionValue(this->GetNode(n)->GetDegreeOfFreedom(f), solutionIndex);
    }
  return value;
}

// ////////////////////////////////////////////////////////////////////////
/**
 * Geometry of a problem.
 */

void
Element::Jacobian(const VectorType & pt, MatrixType & J, const MatrixType *pshapeD) const
{
  MatrixType *pshapeDlocal = ITK_NULLPTR;

  // If derivatives of shape functions were not provided, we
  // need to compute them here
  if( pshapeD == ITK_NULLPTR )
    {
    pshapeDlocal = new MatrixType();
    this->ShapeFunctionDerivatives(pt, *pshapeDlocal);
    pshapeD = pshapeDlocal;
    }

  const unsigned int Nn = pshapeD->columns();
  const unsigned int Ndims = this->GetNumberOfSpatialDimensions();

  MatrixType coords(Nn, Ndims);
  for( unsigned int n = 0; n < Nn; n++ )
    {
    VectorType p = this->GetNodeCoordinates(n);
    coords.set_row(n, p);
    }

  J = ( *pshapeD ) * coords;

  // Destroy local copy of derivatives of shape functions, if
  // they were computed.
  delete pshapeDlocal;
}

Element::Float
Element::JacobianDeterminant(const VectorType & pt, const MatrixType *pJ) const
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

  //  Float det=vnl_svd<Float>(*pJ).determinant_magnitude();
  Float det = vnl_qr<Float>(*pJ).determinant();

  delete pJlocal;

  return det;
}

void
Element::JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType *pJ) const
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

  delete pJlocal;
}

void Element::ShapeFunctionGlobalDerivatives(const VectorType & pt,
                                             MatrixType & shapeDgl,
                                             const MatrixType *pJ,
                                             const MatrixType *pshapeD) const
{
  MatrixType *pshapeDlocal = ITK_NULLPTR;
  MatrixType *pJlocal = ITK_NULLPTR;

  // If derivatives of shape functions were not provided, we
  // need to compute them here
  if( pshapeD == ITK_NULLPTR )
    {
    pshapeDlocal = new MatrixType();
    this->ShapeFunctionDerivatives(pt, *pshapeDlocal);
    pshapeD = pshapeDlocal;
    }

  // If Jacobian was not provided, we
  // need to compute it here
  if( pJ == ITK_NULLPTR )
    {
    pJlocal = new MatrixType();
    this->Jacobian(pt, *pJlocal, pshapeD);
    pJ = pJlocal;
    }

  MatrixType invJ;
  this->JacobianInverse(pt, invJ, pJ);

  shapeDgl = invJ * ( *pshapeD );

  delete pJlocal;
  delete pshapeDlocal;
}

Element::VectorType
Element::GetGlobalFromLocalCoordinates(const VectorType & pt) const
{
  unsigned int Nnodes = this->GetNumberOfNodes();
  MatrixType   nc(this->GetNumberOfSpatialDimensions(), Nnodes);
  for( unsigned int n = 0; n < Nnodes; n++ )
    {
    nc.set_column( n, this->GetNodeCoordinates(n) );
    }

  VectorType shapeF = ShapeFunctions(pt);

  return nc * shapeF;
}

// Gauss-Legendre integration rule constants
const Element::Float Element::gaussPoint[gaussMaxOrder + 1][gaussMaxOrder] =
  {
            { 0.0 },
            { 0.000000000000000 },
            { 0.577350269189626, -0.577350269189626 },
            { 0.774596669241483, 0.000000000000000, -0.774596669241483 },
            { 0.861136311594053, 0.339981043584856, -0.339981043584856, -0.861136311594053 },
            { 0.906179845938664, 0.538469310105683, 0.000000000000000, -0.538469310105683, -0.906179845938664 },
            { 0.932469514203152, 0.661209386466264, 0.238619186083197, -0.238619186083197, -0.661209386466264,
            -0.932469514203152 },
            { 0.949107912342759, 0.741531185599394, 0.405845151377397, 0.000000000000000, -0.405845151377397,
            -0.741531185599394, -0.949107912342759 },
            { 0.960289856497536, 0.796666477413627, 0.525532409916329, 0.183434642495650, -0.183434642495650,
            -0.525532409916329, -0.796666477413627, -0.960289856497536 },
            { 0.968160239507626, 0.836031107326636, 0.613371432700590, 0.324253423403809, 0.000000000000000,
            -0.324253423403809, -0.613371432700590, -0.836031107326636, -0.968160239507626 },
            { 0.973906528517172, 0.865063366688985, 0.679409568299024, 0.433395394129247, 0.148874338981631,
            -0.148874338981631, -0.433395394129247, -0.679409568299024, -0.865063366688985, -0.973906528517172 }
  };

const Element::Float Element::gaussWeight[gaussMaxOrder + 1][gaussMaxOrder] =
  {
            { 0.0 },
            { 2.000000000000000 },
            { 1.000000000000000, 1.000000000000000 },
            { 0.555555555555555, 0.888888888888889, 0.555555555555555 },
            { 0.347854845137454, 0.652145154862546, 0.652145154862546, 0.347854845137454 },
            { 0.236926885056189, 0.478628670499366, 0.568888888888889, 0.478628670499366, 0.236926885056189 },
            { 0.171324492379170, 0.360761573048139, 0.467913934572691, 0.467913934572691, 0.360761573048139,
            0.171324492379170 },
            { 0.129484966168869, 0.279705391489277, 0.381830050505119, 0.417959183673469, 0.381830050505119,
            0.279705391489277, 0.129484966168869 },
            { 0.101228536290376, 0.222381034453374, 0.313706645877887, 0.362683783378362, 0.362683783378362,
            0.313706645877887, 0.222381034453374, 0.101228536290376 },
            { 0.081274388361575, 0.180648160694858, 0.260610696402935, 0.312347077040003, 0.330239355001260,
            0.312347077040003, 0.260610696402935, 0.180648160694858, 0.081274388361575 },
            { 0.066671344308688, 0.149451349150581, 0.219086362515982, 0.269266719309996, 0.295524224714753,
            0.295524224714753, 0.269266719309996, 0.219086362515982, 0.149451349150581, 0.066671344308688 }
  };

void Element::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "#IDs: " << this->m_EdgeIds.size() << std::endl;
  for( unsigned int i = 0; i < this->m_EdgeIds.size(); i++ )
    {
    os << indent << "Edge Ids (" << i << "): " << this->m_EdgeIds[i][0];
    os << " " << this->m_EdgeIds[i][1] << std::endl;
    }
}

::itk::LightObject::Pointer Element::Node::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->m_coordinates = this->m_coordinates;
  copyPtr->m_dof = this->m_dof;
  copyPtr->m_elements = this->m_elements;
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void Element::Node::ClearDegreesOfFreedom(void) const
{
  m_dof.clear();
}

void Element::Node::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // os << indent << "DOF: " << this->m_dof << std::endl;
  // os << indent << "Coordinates: " << this->m_coordinates << std::endl;
  // os << indent << "Elements: " << this->m_elements << std::endl;
}

Material::ConstPointer Element::GetMaterial(void) const
{
  return ITK_NULLPTR;
}

void Element::SetMaterial(Material::ConstPointer)
{
}

void Element::SetNode(unsigned int n, Node::Pointer node)
{
  this->SetNode(n,NodeIDType(node.GetPointer()));
}

unsigned int Element::GetNumberOfDegreesOfFreedom(void) const
{
  return this->GetNumberOfNodes() * this->GetNumberOfDegreesOfFreedomPerNode();
}

std::vector<std::vector<int> > Element::GetEdgeIds(void) const
{
  return this->m_EdgeIds;
}

}
}  // end namespace itk::fem
