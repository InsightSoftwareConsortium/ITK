/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBar2D.cxx
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

#include "itkFEMElementBar2D.h"
#include "itkFEMLoadPoint.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMUtility.h"
#include "itkFEMObjectFactory.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * Construct a Bar2D element by specifying two nodes and material propertites
 */
Bar2D::Bar2D( Node::ConstPointer n1_, Node::ConstPointer n2_, Material::ConstPointer const mat_ )
{
  /**
   * Initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * if the node class was incorrect a bad_cast exception is thrown
   */
  try
  {
    m_node[0]=&dynamic_cast<const NodeXY&>(*n1_);
    m_node[1]=&dynamic_cast<const NodeXY&>(*n2_);
    m_mat=&dynamic_cast<const MaterialStandard&>(*mat_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Bar2D::Bar2D()");
  }

}



  
/**
 * Return the stiffness matrix for the element.
 */
vnl_matrix<Bar2D::Float> Bar2D::Ke() const {

vnl_matrix<Float> k(NDOF,NDOF);

Float x=m_node[1]->X-m_node[0]->X;
Float y=m_node[1]->Y-m_node[0]->Y;
Float L=m_mat->E*m_mat->A/(sqrt(x*x+y*y)*(x*x+y*y));

  k[0][0]= x*x; k[0][1]= x*y; k[0][2]=-x*x; k[0][3]=-x*y;
  k[1][0]= y*x; k[1][1]= y*y; k[1][2]=-y*x; k[1][3]=-y*y;
  k[2][0]=-x*x; k[2][1]=-x*y; k[2][2]= x*x; k[2][3]= x*y;
  k[3][0]=-y*x; k[3][1]=-y*y; k[3][2]= y*x; k[3][3]= y*y;

  return L*k;

}



/**
 * Draw the element on the device context.
 */
#ifdef FEM_BUILD_VISUALIZATION
void Bar2D::Draw(CDC* pDC, Solution::ConstPointer sol) const {

  int x1=m_node[0]->X*DC_Scale;
  int y1=m_node[0]->Y*DC_Scale;
  int x2=m_node[1]->X*DC_Scale;
  int y2=m_node[1]->Y*DC_Scale;

  x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(2))*DC_Scale;
  y2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(3))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);

}
#endif




/**
 * Read the element from input stream
 */
void Bar2D::Read( std::istream& f, void* info )
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  Node::ArrayType::Pointer nodes=static_cast<ReadInfoType*>(info)->m_node;
  Material::ArrayType::Pointer mats=static_cast<ReadInfoType*>(info)->m_mat;

  /** first call the parent's read function */
  Superclass::Read(f,info);

  try
  {
    /** read and set the material pointer */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<MaterialStandard*>( &*mats->Find(n));

    /** read and set first GNN */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[0]=dynamic_cast<NodeXY*>( &*nodes->Find(n));

    /** read and set second GNN */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[1]=dynamic_cast<NodeXY*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Bar2D::Read()",e.m_baseClassName,e.m_GN);
  }

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Bar2D::Read()","Error reading FEM element!");
  }

}




/**
 * Write the element to the output stream
 */
void Bar2D::Write( std::ostream& f ) const {

  /**  first call the parent's write function */
  Superclass::Write(f);

  /** 
   * then write the actual data (node, and material numbers)
   * we add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node[0]->GN<<"\t% NodeXY 1 ID\n";
  f<<"\t"<<m_node[1]->GN<<"\t% NodeXY 2 ID\n";

  /** check for errors */
  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Bar2D::Write()","Error writing FEM element!");
  }

}

FEM_CLASS_REGISTER(Bar2D)




}} // end namespace itk::fem
