/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementNewBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElementNewBase.h"
#include "vnl/algo/vnl_svd.h"

namespace itk {
namespace fem {




#ifdef FEM_BUILD_VISUALIZATION
/** Global scale factor for drawing on the DC */
double& ElementNew::DC_Scale=Node::DC_Scale;
#endif




//////////////////////////////////////////////////////////////////////////
/*
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
 * GetIntegrationPoint() / GetNumberOfIntegrationPoints() to obtain the
 * integration points. It also calls the GetStrainDisplacementMatrix()
 * and GetMaterialMatrix() member functions.
 *
 * \param Ke Reference to the resulting stiffnes matrix.
 *
 * \note This is a very generic implementation of the stiffness matrix
 *       that is suitable for any problem/element definition. A specifc
 *       element may override this implementation with its own simple one.
 */
void ElementNew::GetStiffnessMatrix(MatrixType& Ke) const
{
  // Number of DOFs
  const unsigned int N = GetNumberOfDegreesOfFreedom();

  // B and D matrices
  MatrixType B,D;
  this->GetMaterialMatrix( D );

  Ke.resize(N,N); // resize the target matrix object
  Ke.fill(0.0);
  unsigned int Nip=this->GetNumberOfIntegrationPoints();

  VectorType ip;
  MatrixType J;
  MatrixType shapeDgl;
  MatrixType shapeD;

  for(unsigned int i=0; i<Nip; i++)
  {
    ip=this->GetIntegrationPoint(i);
    this->ShapeFunctionDerivatives(ip,shapeD);
    this->Jacobian(ip,J,&shapeD);
    this->ShapeFunctionGlobalDerivatives(ip,shapeDgl,&J,&shapeD);

    this->GetStrainDisplacementMatrix( B, shapeDgl );
    Float detJ=this->JacobianDeterminant( ip, &J );
    Ke+=detJ*GetWeightAtIntegrationPoint(i)*B.transpose()*D*B; // FIXME: write a more efficient way of computing this.
  }
}




void ElementNew::GetMassMatrix( MatrixType& Me ) const
{
  /*
   * If the function is not overiden, we return 0 matrix. This means that
   * by default the elements are static.
   */
  Me = MatrixType( this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0 );
}




ElementNew::MatrixType
ElementNew::Me() const
{
  /*
   * If the function is not overiden, we return 0 matrix. This means that
   * by default the elements are static.
   */
  return MatrixType( this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0 );
}




ElementNew::VectorType
ElementNew::InterpolateSolution( const VectorType& pt, const Solution& sol ) const
{

  VectorType vec( GetNumberOfDegreesOfFreedomPerNode() );
  VectorType shapef = this->ShapeFunctions(pt);
  Float value;

  const unsigned int Nnodes=this->GetNumberOfNodes();
  const unsigned int Ndofs_per_node=this->GetNumberOfDegreesOfFreedomPerNode();

  for(unsigned int f=0; f<Ndofs_per_node; f++)
  {
    value=0.0;

    for(unsigned int n=0; n<Nnodes; n++)
    {
      value+=shapef[n] * sol.GetSolutionValue( this->GetDegreeOfFreedomAtNode(n,f) );
    }

    vec[f]=value;

  }

  return vec;

}




ElementNew::Float
ElementNew::InterpolateSolution( const VectorType& pt, const Solution& sol, unsigned int f ) const
{

  Float value=0.0;

  VectorType shapef = this->ShapeFunctions(pt);
  unsigned int Nnodes=this->GetNumberOfNodes();
  for(unsigned int n=0; n<Nnodes; n++)
  {
    value+=shapef[n] * sol.GetSolutionValue( this->GetDegreeOfFreedomAtNode(n,f) );
  }
  return value;

}





//////////////////////////////////////////////////////////////////////////
/*
 * Geometry of a problem.
 */

void
ElementNew::Jacobian( const VectorType& pt, MatrixType& J, const MatrixType* pshapeD ) const
{
  MatrixType* pshapeDlocal=0;

  // If derivatives of shape functions were not provided, we
  // need to compute them here
  if(pshapeD==0)
  {
    pshapeDlocal=new MatrixType();
    this->ShapeFunctionDerivatives( pt, *pshapeDlocal );
    pshapeD=pshapeDlocal;
  }

  const unsigned int Nn=pshapeD->columns();
  const unsigned int Ndims=pshapeD->rows();

  MatrixType coords(Nn, Ndims);

  for( unsigned int n=0; n<Nn; n++ )
  {
    VectorType p=this->GetNodalCoordinates(n);
    coords.set_row(n,p);
  }

  J=(*pshapeD)*coords;

  // Destroy local copy of derivatives of shape functions, if
  // they were computed.
  delete pshapeDlocal;

}




ElementNew::Float
ElementNew::JacobianDeterminant( const VectorType& pt, const MatrixType* pJ ) const
{
  MatrixType* pJlocal=0;

  // If Jacobian was not provided, we
  // need to compute it here
  if(pJ==0)
  {
    pJlocal=new MatrixType();
    this->Jacobian( pt, *pJlocal );
    pJ=pJlocal;
  }

  Float det=vnl_svd<Float>(*pJ).determinant_magnitude();

  delete pJlocal;

  return det;

}




void
ElementNew::JacobianInverse( const VectorType& pt, MatrixType& invJ, const MatrixType* pJ ) const
{

  MatrixType* pJlocal=0;

  // If Jacobian was not provided, we
  // need to compute it here
  if(pJ==0)
  {
    pJlocal=new MatrixType();
    this->Jacobian( pt, *pJlocal );
    pJ=pJlocal;
  }

  invJ=vnl_svd_inverse<Float>(*pJ);

  delete pJlocal;

}




void ElementNew::ShapeFunctionGlobalDerivatives( const VectorType& pt, MatrixType& shapeDgl, const MatrixType* pJ, const MatrixType* pshapeD ) const
{

  MatrixType* pshapeDlocal=0;
  MatrixType* pJlocal=0;

  // If derivatives of shape functions were not provided, we
  // need to compute them here
  if(pshapeD==0)
  {
    pshapeDlocal=new MatrixType();
    this->ShapeFunctionDerivatives( pt, *pshapeDlocal );
    pshapeD=pshapeDlocal;
  }

  // If Jacobian was not provided, we
  // need to compute it here
  if(pJ==0)
  {
    pJlocal=new MatrixType();
    this->Jacobian( pt, *pJlocal, pshapeD );
    pJ=pJlocal;
  }

  MatrixType invJ;
  this->JacobianInverse( pt, invJ, pJ );

  shapeDgl=invJ*(*pshapeD);

  delete pJlocal;
  delete pshapeDlocal;

}




ElementNew::VectorType
ElementNew::GetGlobalFromLocalCoordinates( const VectorType& pt ) const
{
  MatrixType nc;

  unsigned int Nnodes=this->GetNumberOfNodes();
  for(unsigned int n=0; n<Nnodes; n++)
  {
    nc.set_column( n,this->GetNodalCoordinates(n) );
  }
  
  VectorType shapeF = ShapeFunctions(pt);

  return nc*shapeF;

}




//////////////////////////////////////////////////////////////////////////
/*
 * DOF management
 */

void ElementNew::ClearDegreesOfFreedom(void)
{
  for(unsigned int i=0;i<GetNumberOfDegreesOfFreedom();i++)
  {
    SetDegreeOfFreedom(i,InvalidDegreeOfFreedomID);
  }

}

ElementNew::DegreeOfFreedomIDType ElementNew::m_DOFCounter;




}} // end namespace itk::fem
