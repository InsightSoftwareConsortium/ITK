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
#include "itkFEMUtility.h"
#include "vnl/algo/vnl_svd.h"

namespace itk {
namespace fem {




#ifdef FEM_BUILD_VISUALIZATION

/** Global scale factor for drawing on the DC */
double& ElementNew::Node::DC_Scale=itk::fem::Node::DC_Scale;

/** Global scale factor for drawing on the DC */
double& ElementNew::DC_Scale=ElementNew::Node::DC_Scale;

/**
 * draws the node on DC
 */
void ElementNew::Node::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{
  // We can only draw 2D nodes here
  if(m_coordinates.size()!=2) { return; }

  
  // Normally we draw a white circle.
  CPen pen(PS_SOLID, 0, (COLORREF) RGB(0,0,0) );
  CBrush brush( RGB(255,255,255) );

  if(m_dof.size()>2)
  {
    // If there are more that 2 DOFs at this node, we
    // draw a black circle.
    pen.CreatePen(PS_SOLID, 0, (COLORREF) RGB(255,255,255) );
    brush.CreateSolidBrush( RGB(0,0,0) );
  }

  CPen* pOldpen=pDC->SelectObject(&pen);
  CBrush* pOldbrush=pDC->SelectObject(&brush);

  int x1=m_coordinates[0]*DC_Scale;
  int y1=m_coordinates[1]*DC_Scale;
  x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;

  CPoint r1=CPoint(0,0);
  CPoint r=CPoint(5,5);

  pDC->DPtoLP(&r1);
  pDC->DPtoLP(&r);
  r=r-r1;

  pDC->Ellipse(x1-r.x, y1-r.y, x1+r.x, y1+r.y);

  pDC->SelectObject(pOldbrush);
  pDC->SelectObject(pOldpen);

}

#endif




/*
 * Read the Node from the input stream
 */
void ElementNew::Node::Read(  std::istream& f, void* info )
{
  unsigned int n;

  /*
   * First call the parent's read function
   */
  Self::Superclass::Read(f,info);

  /*
   * Read and set node coordinates
   */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  this->m_coordinates.resize(n);
  SkipWhiteSpace(f); f>>this->m_coordinates; if(!f) goto out;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"ElementNew::Node::Read()","Error reading FEM node!");
  }

}




/*
 * Write the Node to the output stream
 */
void ElementNew::Node::Write( std::ostream& f, int ofid ) const 
{

  /*
   * If not set already, se set the ofid
   */
  if (ofid<0) 
  {
    ofid=OFID;
  }

  /**
   * First call the parent's write function
   */
  Self::Superclass::Write(f,ofid);

  /**
   * Write actual data (node, and properties numbers)
   */
  
  /* write the value of dof */
  f<<"\t"<<this->m_coordinates.size();
  f<<" "<<this->m_coordinates<<"\t% Node coordinates"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"ElementNew::Node::Write()","Error writing FEM node!");
  }

}




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
      value+=shapef[n] * sol.GetSolutionValue( this->GetNode(n)->GetDegreeOfFreedom(f) );
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
    value+=shapef[n] * sol.GetSolutionValue( this->GetNode(n)->GetDegreeOfFreedom(f) );
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
    VectorType p=this->GetNodeCoordinates(n);
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
    nc.set_column( n,this->GetNodeCoordinates(n) );
  }
  
  VectorType shapeF = ShapeFunctions(pt);

  return nc*shapeF;

}





#ifndef FEM_USE_SMART_POINTERS
namespace { static ElementNew::Node::Baseclass::Pointer NewNodeObect() { return new ElementNew::Node; } }
const int ElementNew::Node::OFID=FEMObjectFactory<ElementNew::Node::Baseclass>::Register( NewNodeObect, "Node" );
#else
namespace { static ElementNew::Node::Baseclass::Pointer NewNodeObect() { return ElementNew::Node::New(); } }
const int ElementNew::Node::OFID=FEMObjectFactory<ElementNew::Node::Baseclass>::Register( NewNodeObect, "Node" );
#endif




}} // end namespace itk::fem
