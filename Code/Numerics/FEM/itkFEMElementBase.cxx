/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBase.cxx
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

#include "itkFEMElementBase.h"
#include "vnl/algo/vnl_svd.h"

namespace itk {
namespace fem {




#ifdef FEM_BUILD_VISUALIZATION

/** Global scale factor for drawing on the DC */
double Element::DC_Scale=1000.0;

/** Global scale factor for drawing on the DC */
double &Element::Node::DC_Scale=Element::DC_Scale;


/**
 * draws the node on DC
 */
void Element::Node::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{
  // We can only draw 2D nodes here
  if(m_coordinates.size()!=2) { return; }

  
  // Normally we draw a white circle.
  CPen pen(PS_SOLID, 0, (COLORREF) RGB(0,0,0) );
  CBrush brush( RGB(255,255,255) );

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
void Element::Node::Read(  std::istream& f, void* info )
{
  unsigned int n;

  /*
   * First call the parent's read function
   */
  Superclass::Read(f,info);

  /*
   * Read and set node coordinates
   */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  this->m_coordinates.resize(n);
  SkipWhiteSpace(f); f>>this->m_coordinates; if(!f) goto out;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Element::Node::Read()","Error reading FEM node!");
  }

}




/*
 * Write the Node to the output stream
 */
void Element::Node::Write( std::ostream& f ) const 
{
  /**
   * First call the parent's write function
   */
  Superclass::Write(f);

  /**
   * Write actual data (node, and properties numbers)
   */
  
  /* write the value of dof */
  f<<"\t"<<this->m_coordinates.size();
  f<<" "<<this->m_coordinates<<"\t% Node coordinates"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Element::Node::Write()","Error writing FEM node!");
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
void Element::GetStiffnessMatrix(MatrixType& Ke) const
{
  // B and D matrices
  MatrixType B,D;
  this->GetMaterialMatrix( D );

  unsigned int Nip=this->GetNumberOfIntegrationPoints();

  VectorType ip;
  Float w;
  MatrixType J;
  MatrixType shapeDgl;
  MatrixType shapeD;

  // Calculate the contribution of 1st int. point to initialize
  // the Ke matrix to proper number of elements.
  this->GetIntegrationPointAndWeight(0,ip,w);
  this->ShapeFunctionDerivatives(ip,shapeD);
  this->Jacobian(ip,J,&shapeD);
  this->ShapeFunctionGlobalDerivatives(ip,shapeDgl,&J,&shapeD);

  this->GetStrainDisplacementMatrix( B, shapeDgl );
  Float detJ=this->JacobianDeterminant( ip, &J );
  Ke=detJ*w*B.transpose()*D*B; // FIXME: write a more efficient way of computing this.

  // Add contributions of other int. points to the Ke
  for(unsigned int i=1; i<Nip; i++)
  {
    this->GetIntegrationPointAndWeight(i,ip,w);
    this->ShapeFunctionDerivatives(ip,shapeD);
    this->Jacobian(ip,J,&shapeD);
    this->ShapeFunctionGlobalDerivatives(ip,shapeDgl,&J,&shapeD);

    this->GetStrainDisplacementMatrix( B, shapeDgl );
    Float detJ=this->JacobianDeterminant( ip, &J );
    Ke+=detJ*w*B.transpose()*D*B; // FIXME: write a more efficient way of computing this.
  }
}

Element::VectorType Element::GetStrainsAtPoint(const VectorType& pt, const Solution& sol, unsigned int index) const
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
  
  e = B*u;

  return e;
}

Element::VectorType Element::GetStressesAtPoint(const VectorType& itkNotUsed(pt),
                                                const VectorType& e,
                                                const Solution& itkNotUsed(sol),
                                                unsigned int itkNotUsed(index)) const
  // NOTE: pt should be in local coordinates already
{
  MatrixType D;
  VectorType sigma;

  this->GetMaterialMatrix(D);
  
  sigma = D*e;

  return sigma;
}

void Element::GetLandmarkContributionMatrix(float eta, MatrixType& Le) const
{
  // Provides the contribution of a landmark to the element stiffness
  // matrix
  Le = MatrixType( this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0 );

  const unsigned int NnDOF=this->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=this->GetNumberOfNodes();
  const unsigned int NDOF = GetNumberOfDegreesOfFreedom();
  const unsigned int Nip=this->GetNumberOfIntegrationPoints(0);

  Le.resize(NDOF,NDOF); // resize the target matrix object
  Le.fill(0.0);

  Float w;
  VectorType ip, shape;

  for(unsigned int i=0; i<Nip; i++)
  {
    this->GetIntegrationPointAndWeight(i,ip,w,0);
    shape=this->ShapeFunctions(ip);
    
    for(unsigned int ni=0; ni<Nnodes; ni++)
    {
      for(unsigned int nj=0; nj<Nnodes; nj++)
      {
        Float m=w*shape[ni]*shape[nj];
        for(unsigned int d=0; d<NnDOF; d++)
        {
          Le[ni*NnDOF+d][nj*NnDOF+d]+=m;
        }
      }
    }
  }

  Le = Le / (eta * eta);
}


Element::Float Element::GetElementDeformationEnergy( MatrixType& LocalSolution ) const
{

  MatrixType U;

  Element::MatrixType Ke;
  this->GetStiffnessMatrix(Ke);

  U=LocalSolution.transpose()*Ke*LocalSolution;

  return U[0][0];
}

void Element::GetMassMatrix( MatrixType& Me ) const
{
  /*
   * If the function is not overiden, we compute consistent mass matrix
   * by integrating the shape functions over the element domain. The element
   * density is assumed one. If this is not the case, the GetMassMatrix in a
   * derived class must be overriden and the Me matrix corrected accordingly.
   */
  Me = MatrixType( this->GetNumberOfDegreesOfFreedom(), this->GetNumberOfDegreesOfFreedom(), 0.0 );

  const unsigned int NnDOF=this->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=this->GetNumberOfNodes();
  const unsigned int NDOF = GetNumberOfDegreesOfFreedom();
  const unsigned int Nip=this->GetNumberOfIntegrationPoints(0);

  Me.resize(NDOF,NDOF); // resize the target matrix object
  Me.fill(0.0);

  Float w;
  VectorType ip, shape;
  MatrixType J, shapeD;

  for(unsigned int i=0; i<Nip; i++)
  {
    this->GetIntegrationPointAndWeight(i,ip,w,0);
    shape=this->ShapeFunctions(ip);
    this->ShapeFunctionDerivatives(ip,shapeD);
    this->Jacobian(ip,J,&shapeD);
    Float detJ=this->JacobianDeterminant( ip, &J );
    
    for(unsigned int ni=0; ni<Nnodes; ni++)
    {
      for(unsigned int nj=0; nj<Nnodes; nj++)
      {
        Float m=detJ*w*shape[ni]*shape[nj];
        for(unsigned int d=0; d<NnDOF; d++)
        {
          Me[ni*NnDOF+d][nj*NnDOF+d]+=m;
        }
      }
    }
  }

}




Element::VectorType
Element::InterpolateSolution( const VectorType& pt, const Solution& sol, unsigned int solutionIndex  ) const
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
      value+=shapef[n] * sol.GetSolutionValue( this->GetNode(n)->GetDegreeOfFreedom(f) , solutionIndex);
    }

    vec[f]=value;

  }

  return vec;

}




Element::Float
Element::InterpolateSolutionN( const VectorType& pt, const Solution& sol, unsigned int f , unsigned int solutionIndex ) const
{

  Float value=0.0;

  VectorType shapef = this->ShapeFunctions(pt);
  unsigned int Nnodes=this->GetNumberOfNodes();
  for(unsigned int n=0; n<Nnodes; n++)
  {
    value+=shapef[n] * sol.GetSolutionValue( this->GetNode(n)->GetDegreeOfFreedom(f), solutionIndex );
  }
  return value;

}





//////////////////////////////////////////////////////////////////////////
/*
 * Geometry of a problem.
 */

void
Element::Jacobian( const VectorType& pt, MatrixType& J, const MatrixType* pshapeD ) const
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
  const unsigned int Ndims=this->GetNumberOfSpatialDimensions();

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




Element::Float
Element::JacobianDeterminant( const VectorType& pt, const MatrixType* pJ ) const
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
Element::JacobianInverse( const VectorType& pt, MatrixType& invJ, const MatrixType* pJ ) const
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




void Element::ShapeFunctionGlobalDerivatives( const VectorType& pt, MatrixType& shapeDgl, const MatrixType* pJ, const MatrixType* pshapeD ) const
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




Element::VectorType
Element::GetGlobalFromLocalCoordinates( const VectorType& pt ) const
{
  unsigned int Nnodes=this->GetNumberOfNodes();
  MatrixType nc(this->GetNumberOfSpatialDimensions(),Nnodes);
  for(unsigned int n=0; n<Nnodes; n++)
  {
    nc.set_column( n,this->GetNodeCoordinates(n) );
  }
  
  VectorType shapeF = ShapeFunctions(pt);

  return nc*shapeF;

}




// Gauss-Legendre integration rule constants
const Element::Float Element::gaussPoint[gaussMaxOrder+1][gaussMaxOrder]=
{
  { 0.0 },
  { 0.000000000000000 },
  { 0.577350269189626,-0.577350269189626 },
  { 0.774596669241483, 0.000000000000000,-0.774596669241483 },
  { 0.861136311594053, 0.339981043584856,-0.339981043584856,-0.861136311594053 },
  { 0.906179845938664, 0.538469310105683, 0.000000000000000,-0.538469310105683,-0.906179845938664},
  { 0.932469514203152, 0.661209386466264, 0.238619186083197,-0.238619186083197,-0.661209386466264,-0.932469514203152 },
  { 0.949107912342759, 0.741531185599394, 0.405845151377397, 0.000000000000000,-0.405845151377397,-0.741531185599394,-0.949107912342759 },
  { 0.960289856497536, 0.796666477413627, 0.525532409916329, 0.183434642495650,-0.183434642495650,-0.525532409916329,-0.796666477413627,-0.960289856497536 },
  { 0.968160239507626, 0.836031107326636, 0.613371432700590, 0.324253423403809, 0.000000000000000,-0.324253423403809,-0.613371432700590,-0.836031107326636,-0.968160239507626 },
  { 0.973906528517172, 0.865063366688985, 0.679409568299024, 0.433395394129247, 0.148874338981631,-0.148874338981631,-0.433395394129247,-0.679409568299024,-0.865063366688985,-0.973906528517172 }
};

const Element::Float Element::gaussWeight[gaussMaxOrder+1][gaussMaxOrder]=
{
  { 0.0 },
  { 2.000000000000000 },
  { 1.000000000000000, 1.000000000000000 },
  { 0.555555555555555, 0.888888888888889, 0.555555555555555 },
  { 0.347854845137454, 0.652145154862546, 0.652145154862546, 0.347854845137454 },
  { 0.236926885056189, 0.478628670499366, 0.568888888888889, 0.478628670499366, 0.236926885056189 },
  { 0.171324492379170, 0.360761573048139, 0.467913934572691, 0.467913934572691, 0.360761573048139, 0.171324492379170 },
  { 0.129484966168869, 0.279705391489277, 0.381830050505119, 0.417959183673469, 0.381830050505119, 0.279705391489277, 0.129484966168869 },
  { 0.101228536290376, 0.222381034453374, 0.313706645877887, 0.362683783378362, 0.362683783378362, 0.313706645877887, 0.222381034453374, 0.101228536290376 },
  { 0.081274388361575, 0.180648160694858, 0.260610696402935, 0.312347077040003, 0.330239355001260, 0.312347077040003, 0.260610696402935, 0.180648160694858, 0.081274388361575 },
  { 0.066671344308688, 0.149451349150581, 0.219086362515982, 0.269266719309996, 0.295524224714753, 0.295524224714753, 0.269266719309996, 0.219086362515982, 0.149451349150581, 0.066671344308688 }
};



// Register Node class with FEMObjectFactory
FEM_CLASS_REGISTER(Node);




}} // end namespace itk::fem
