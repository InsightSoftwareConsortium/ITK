/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBeam2D.cxx
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

#include "itkFEMElementBeam2D.h"
#include "itkFEMLoadPoint.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMUtility.h"
#include "itkFEMObjectFactory.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * Construct a Beam2D element by specifying two nodes and propertites
 */
Beam2D::Beam2D(  Node::ConstPointer n1_, Node::ConstPointer n2_, Material::ConstPointer mat_ )
{
  /**
   * Initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * If the node class was incorrect a bad_cast exception is thrown.
   */
  try
  {
    m_node[0]=&dynamic_cast<const NodeXYrotZ&>(*n1_);
    m_node[1]=&dynamic_cast<const NodeXYrotZ&>(*n2_);
    m_mat=&dynamic_cast<const MaterialStandard&>(*mat_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Beam2D::Beam2D()");
  }


}


  
  
/**
 * Returns the stiffness matrix for the element.
 */
vnl_matrix<Beam2D::Float> Beam2D::Ke() const {

vnl_matrix<Float> k(NDOF,NDOF);
vnl_matrix<Float> kb(NDOF,NDOF);

Float x=m_node[1]->X-m_node[0]->X;
Float y=m_node[1]->Y-m_node[0]->Y;
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

  return k.transpose()*kb*k;

}




/**
 * Draw the element on the device context.
 */
#ifdef FEM_BUILD_VISUALIZATION
void Beam2D::Draw(CDC* pDC, Solution::ConstPointer sol) const {

  int x1=m_node[0]->X*DC_Scale;
  int y1=m_node[0]->Y*DC_Scale;
  int x2=m_node[1]->X*DC_Scale;
  int y2=m_node[1]->Y*DC_Scale;

  x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(3))*DC_Scale;
  y2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(4))*DC_Scale;

  CPen pen(PS_SOLID, 0.1*Node::DC_Scale, (COLORREF) 0);
  CPen* pOldPen=pDC->SelectObject(&pen);

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);

  pDC->SelectObject(pOldPen);

}
#endif




/**
 * Read the element from input stream
 */
void Beam2D::Read( std::istream& f, void* info )
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::NodeArrayPointer nodes=static_cast<ReadInfoType*>(info)->m_node;
  ReadInfoType::MaterialArrayPointer mats=static_cast<ReadInfoType*>(info)->m_mat;

  /** first call the parent's read function */
  Superclass::Read(f,info);

  try
  {
    /** read and set the properties pointer */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));
  
    /** read and set first GNN */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[0]=dynamic_cast<const NodeXYrotZ*>( &*nodes->Find(n));

    /** read and set second GNN */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[1]=dynamic_cast<const NodeXYrotZ*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Beam2D::Read()",e.m_baseClassName,e.m_GN);
  }

out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Beam2D::Read()","Error reading FEM element!");
  }

}




/** 
 * Write the element to the output stream
 */
void Beam2D::Write( std::ostream& f ) const {

  /** First call the parent's write function */
  Superclass::Write(f);

  /**
   * Then write the actual data (node, and material numbers).
   * We add some comments in the output file.
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node[0]->GN<<"\t% NodeXYrotZ 1 ID\n";
  f<<"\t"<<m_node[1]->GN<<"\t% NodeXYrotZ 2 ID\n";

  /** Check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Beam2D::Write()","Error writing FEM element!");
  }

}

FEM_CLASS_REGISTER(Beam2D)




}} // end namespace itk::fem
