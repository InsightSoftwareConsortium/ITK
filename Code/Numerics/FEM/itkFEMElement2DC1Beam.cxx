/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC1Beam.cxx
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

#include "itkFEMElement2DC1Beam.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




Element2DC1Beam
::Element2DC1Beam() : Superclass(), m_mat(0)
{
}

Element2DC1Beam
::Element2DC1Beam(  NodeIDType n1_, NodeIDType n2_, Material::ConstPointer m_ )
{
  // Set the geometrical points
  this->SetNode( 0, n1_ );
  this->SetNode( 1, n2_ );

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  if( (m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*m_)) == 0 )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element2DC0LinearLineStress::Element2DC0LinearLineStress()");
  }
}





void
Element2DC1Beam
::GetIntegrationPointAndWeight( unsigned int i, VectorType& pt, Float& w, unsigned int order ) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  pt.resize(1);

  pt[0]=gaussPoint[order][i];
  w=gaussWeight[order][i];
}

unsigned int
Element2DC1Beam
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // FIXME: range checking

  // default integration order
  if (order==0) { order=DefaultIntegrationOrder; }

  return order;
}




Element2DC1Beam::VectorType
Element2DC1Beam
::ShapeFunctions( const VectorType& pt ) const
{
  // 2D Beam element has four shape functions, but we only
  // define two of them, since we're only interested in
  // in interpolating the displacements, not the rotation
  VectorType shapeF(2);

  shapeF[0] = 0.25*(1-pt[0])*(1-pt[0])*(2+pt[0]);
  shapeF[1] = 0.25*(1+pt[0])*(1+pt[0])*(2-pt[0]);

  return shapeF;
}




void
Element2DC1Beam
::ShapeFunctionDerivatives( const VectorType&, MatrixType& shapeD ) const
{
  // FIXME: write proper implementation, since we need the 2nd
  //        order derivatives
  shapeD.resize(1,2);
  shapeD.fill(0.0);
}




Element2DC1Beam::Float
Element2DC1Beam
::JacobianDeterminant( const VectorType&, const MatrixType* ) const
{
  // FIXME: this is only temporary implementation, so that GenericBodyLoads
  //        implementation works. Write the proper geometric definition
  //        of an element.

  // Get the length of the element
  // Note: This simple implementation is only valid for linear line elements.
  //       For higher order elements we must integrate to obtain the exact
  //       element length
  Float l=(this->m_node[1]->GetCoordinates() - this->m_node[0]->GetCoordinates()).magnitude();
  return l/2;
}




void 
Element2DC1Beam
::GetStiffnessMatrix( MatrixType& Ke ) const
{

const unsigned int NDOF=this->GetNumberOfDegreesOfFreedom();

MatrixType k(NDOF,NDOF);
MatrixType kb(NDOF,NDOF);

Float x=m_node[1]->GetCoordinates()[0]-m_node[0]->GetCoordinates()[0];
Float y=m_node[1]->GetCoordinates()[1]-m_node[0]->GetCoordinates()[1];
Float l=sqrt(x*x+y*y);

  k[0][0]= 1; k[0][1]= 0; k[0][2]= 0; k[0][3]=-1; k[0][4]= 0; k[0][5]= 0;
  k[1][0]= 0; k[1][1]= 0; k[1][2]= 0; k[1][3]= 0; k[1][4]= 0; k[1][5]= 0;
  k[2][0]= 0; k[2][1]= 0; k[2][2]= 0; k[2][3]= 0; k[2][4]= 0; k[2][5]= 0;
  k[3][0]=-1; k[3][1]= 0; k[3][2]= 0; k[3][3]= 1; k[3][4]= 0; k[3][5]= 0;
  k[4][0]= 0; k[4][1]= 0; k[4][2]= 0; k[4][3]= 0; k[4][4]= 0; k[4][5]= 0;
  k[5][0]= 0; k[5][1]= 0; k[5][2]= 0; k[5][3]= 0; k[5][4]= 0; k[5][5]= 0;

  kb=(m_mat->E*m_mat->A/l)*k;

  k[0][0]= 0; k[0][1]= 0;   k[0][2]= 0;     k[0][3]= 0; k[0][4]= 0;   k[0][5]= 0;
  k[1][0]= 0; k[1][1]= 6;   k[1][2]= 3*l;   k[1][3]= 0; k[1][4]=-6;   k[1][5]= 3*l;
  k[2][0]= 0; k[2][1]= 3*l; k[2][2]= 2*l*l; k[2][3]= 0; k[2][4]=-3*l; k[2][5]= l*l;
  k[3][0]= 0; k[3][1]= 0;   k[3][2]= 0;     k[3][3]= 0; k[3][4]= 0;   k[3][5]= 0;
  k[4][0]= 0; k[4][1]= -6;  k[4][2]= -3*l;  k[4][3]= 0; k[4][4]= 6;   k[4][5]=-3*l;
  k[5][0]= 0; k[5][1]= 3*l; k[5][2]= l*l;   k[5][3]= 0; k[5][4]=-3*l; k[5][5]= 2*l*l;

  kb+=(2*m_mat->E*m_mat->I/(l*l*l))*k;

Float c=x/l;
Float s=y/l;

  k[0][0]= c; k[0][1]= s; k[0][2]= 0; k[0][3]= 0; k[0][4]= 0; k[0][5]= 0;
  k[1][0]=-s; k[1][1]= c; k[1][2]= 0; k[1][3]= 0; k[1][4]= 0; k[1][5]= 0;
  k[2][0]= 0; k[2][1]= 0; k[2][2]= 1; k[2][3]= 0; k[2][4]= 0; k[2][5]= 0;
  k[3][0]= 0; k[3][1]= 0; k[3][2]= 0; k[3][3]= c; k[3][4]= s; k[3][5]= 0;
  k[4][0]= 0; k[4][1]= 0; k[4][2]= 0; k[4][3]=-s; k[4][4]= c; k[4][5]= 0;
  k[5][0]= 0; k[5][1]= 0; k[5][2]= 0; k[5][3]= 0; k[5][4]= 0; k[5][5]= 1;

  Ke=k.transpose()*kb*k;

}




void
Element2DC1Beam
::GetMassMatrix( MatrixType& Me ) const
{

const unsigned int NDOF=this->GetNumberOfDegreesOfFreedom();
MatrixType m(NDOF,NDOF,0.0);
MatrixType mb(NDOF,NDOF,0.0);
MatrixType k(NDOF,NDOF,0.0);

Float x=m_node[1]->GetCoordinates()[0]-m_node[0]->GetCoordinates()[0];
Float y=m_node[1]->GetCoordinates()[1]-m_node[0]->GetCoordinates()[1];
Float l=sqrt(x*x+y*y);

  m[0][0]=2.0; m[0][3]=1.0;
  m[3][0]=1.0; m[3][3]=2.0;

  m=(this->m_mat->RhoC*this->m_mat->A*l/6.0)*m;

  mb[1][1]=156.0; mb[1][2]=22.0*l; mb[1][4]=54.0; mb[1][5]=-13.0*l;
  mb[2][1]=22.0*l; mb[2][2]=4.0*l*l; mb[2][4]=13.0*l; mb[2][5]=-3.0*l*l;
  mb[4][1]=54.0; mb[4][2]=13.0*l; mb[4][4]=156.0; mb[4][5]=-22.0*l;
  mb[5][1]=-13.0*l; mb[5][2]=-3.0*l*l; mb[5][4]=-22.0*l; mb[5][5]=4.0*l*l;

  mb=(this->m_mat->RhoC*this->m_mat->A*l/420.0)*mb;

  m=m+mb;

  Float c=x/l;
  Float s=y/l;

  k[0][0]= c; k[0][1]= s; k[0][2]= 0; k[0][3]= 0; k[0][4]= 0; k[0][5]= 0;
  k[1][0]=-s; k[1][1]= c; k[1][2]= 0; k[1][3]= 0; k[1][4]= 0; k[1][5]= 0;
  k[2][0]= 0; k[2][1]= 0; k[2][2]= 1; k[2][3]= 0; k[2][4]= 0; k[2][5]= 0;
  k[3][0]= 0; k[3][1]= 0; k[3][2]= 0; k[3][3]= c; k[3][4]= s; k[3][5]= 0;
  k[4][0]= 0; k[4][1]= 0; k[4][2]= 0; k[4][3]=-s; k[4][4]= c; k[4][5]= 0;
  k[5][0]= 0; k[5][1]= 0; k[5][2]= 0; k[5][3]= 0; k[5][4]= 0; k[5][5]= 1;

  Me=k.transpose()*m*k;

}




void
Element2DC1Beam
::Read( std::istream& f, void* info )
{
  int n;
  /*
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::MaterialArrayPointer mats=static_cast<ReadInfoType*>(info)->m_mat;


  /* first call the parent's read function */
  Superclass::Read(f,info);

  try
  {
    /*
     * Read and set the material pointer
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialLinearElasticity*>( &*mats->Find(n));

  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element2DC1Beam::Read()",e.m_baseClassName,e.m_GN);
  }

  // Check if the material object was of correct class
  if(!m_mat)
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element2DC1Beam::Read()");
  }

out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DC1Beam::Read()","Error reading FEM element!");
  }
}




void
Element2DC1Beam
::Write( std::ostream& f ) const
{
  // First call the parent's write function
  Superclass::Write(f);

  /*
   * then write the actual data (material number)
   * We also add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialLinearElasticity ID\n";

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element1DStress::Write()","Error writing FEM element!");
  }
}




#ifdef FEM_BUILD_VISUALIZATION
void 
Element2DC1Beam
::Draw(CDC* pDC, Solution::ConstPointer sol) const
{
  int x1=GetNodeCoordinates(0)[0]*DC_Scale;
  int y1=GetNodeCoordinates(0)[1]*DC_Scale;
  int x2=GetNodeCoordinates(1)[0]*DC_Scale;
  int y2=GetNodeCoordinates(1)[1]*DC_Scale;

  x1+=sol->GetSolutionValue(this->GetNode(0)->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetNode(0)->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->GetNode(1)->GetDegreeOfFreedom(0))*DC_Scale;
  y2+=sol->GetSolutionValue(this->GetNode(1)->GetDegreeOfFreedom(1))*DC_Scale;

  CPen pen(PS_SOLID, 0.1*Node::DC_Scale, (COLORREF) 0);
  CPen* pOldPen=pDC->SelectObject(&pen);

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);

  pDC->SelectObject(pOldPen);
}
#endif




FEM_CLASS_REGISTER(Element2DC1Beam)




}} // end namespace itk::fem
