/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementC1IsoCurve2D.cxx
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

#include "itkFEMElementC1IsoCurve2D.h"

namespace itk {
namespace fem {



/**
 * Construct a C1IsoCurve2D element by specifying two nodes and material propertites
 */
C1IsoCurve2D::C1IsoCurve2D(  Node::ConstPointer nn1_,
              Node::ConstPointer cn_,
              Node::ConstPointer pn1_,
              Node::ConstPointer pn2_, 
              Material::ConstPointer mat_ )
{
  /**
   * Initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * if the node class was incorrect a bad_cast exception is thrown
   */
  try
  {
    cur_node=&dynamic_cast<const Node2DIsotropic&>(*cn_);
    neg_node1=&dynamic_cast<const Node2DIsotropic&>(*nn1_);
    pos_node1=&dynamic_cast<const Node2DIsotropic&>(*pn1_);
    pos_node2=&dynamic_cast<const Node2DIsotropic&>(*pn2_);
    mat=&dynamic_cast<const MaterialStandard&>(*mat_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"C1IsoCurve2D::C1IsoCurve2D()");
  }

  ControlVec.resize(N(),NI());
  ControlVec[0][0]=neg_node1->X;
  ControlVec[0][1]=neg_node1->Y;
  ControlVec[1][0]=cur_node->X;
  ControlVec[1][1]=cur_node->Y;
  ControlVec[2][0]=pos_node1->X;
  ControlVec[2][1]=pos_node1->Y;
  ControlVec[3][0]=pos_node2->X;
  ControlVec[3][1]=pos_node2->Y;

}




/**
 * Hermite interpolation -- control vector determined by the 4 nodes of the element
 */
vnl_vector<Node::Float> C1IsoCurve2D::
InterpolateWithShapeFunctions(Float s){

vnl_vector<Float> ShapeFuncs(N(),0.0);

  ShapeFuncs[0]=(-0.5*s*s*s+s*s-0.5*s);
  ShapeFuncs[1]= (3./2.*s*s*s- 5./2.*s*s+1.);
  ShapeFuncs[2]=(-3./2.*s*s*s+2.*s*s+0.5*s);
  ShapeFuncs[3]=(0.5*s*s*s-0.5*s*s);

/** convert nodal values to matrix form */
  ControlVec[0][0]=neg_node1->X;
  ControlVec[0][1]=neg_node1->Y;
  ControlVec[1][0]=cur_node->X;
  ControlVec[1][1]=cur_node->Y;
  ControlVec[2][0]=pos_node1->X;
  ControlVec[2][1]=pos_node1->Y;
  ControlVec[3][0]=pos_node2->X;
  ControlVec[3][1]=pos_node2->Y; 

  return ShapeFuncs*ControlVec;
}           




/**
 * Read the element from input stream
 */
void C1IsoCurve2D::Read( std::istream& f, void* info )
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  Node::ArrayType::Pointer nodes=static_cast<ReadInfoType*>(info)->m_node;
  Material::ArrayType::Pointer mats=static_cast<ReadInfoType*>(info)->m_mat;


  /** first call the parent's read function - reads GN */
  Superclass::Read(f,info);
  
  /** read and set the material pointer */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));

  try
  {
    /** read and set all four nodes */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    neg_node1=dynamic_cast<const Node2DIsotropic*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    cur_node=dynamic_cast<const Node2DIsotropic*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    pos_node1=dynamic_cast<const Node2DIsotropic*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    pos_node2=dynamic_cast<const Node2DIsotropic*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"C1IsoCurve2D::Read()",e.m_baseClassName,e.m_GN);
  }

  ControlVec.resize(N(),NI());
  ControlVec[0][0]=neg_node1->X;
  ControlVec[0][1]=neg_node1->Y;
  ControlVec[1][0]=cur_node->X;
  ControlVec[1][1]=cur_node->Y;
  ControlVec[2][0]=pos_node1->X;
  ControlVec[2][1]=pos_node1->Y;
  ControlVec[3][0]=pos_node2->X;
  ControlVec[3][1]=pos_node2->Y; 

out:

  if( !f ) {
    throw FEMExceptionIO(__FILE__,__LINE__,"Bar2D::Read()","Error reading FEM element!");
  }

}

  
  
/** 
 * Write the element to the output stream
 */
void C1IsoCurve2D::Write( std::ostream& f, int ofid ) const {

  /** If not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** First call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * Then write the actual data (node, and material numbers).
   * We add some comments in the output file.
   */
  f<<"\t"<<mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<cur_node->GN<<"\t% node 1 ID\n";

  /** Check for errors */
  if (!f) {
    throw FEMExceptionIO(__FILE__,__LINE__,"C1IsoCurve2D::Write()","Error writing FEM element!");
  }
}


/**
 * Return the stiffness matrix for the element.
 */
vnl_matrix<Node::Float> C1IsoCurve2D::Ke() const {

Float alpha=mat->E, beta=mat->I;

static vnl_matrix<Float> k(NDOF,NDOF);
static vnl_matrix<Float> k1(NDOF,NDOF);
static vnl_matrix<Float> k2(NDOF,NDOF);

  k1[0][0]=  4.; k1[0][1]= -7. ; k1[0][2]=2.   ; k1[0][3]=1.;
  k1[1][0]= -7.; k1[1][1]= 136.; k1[1][2]=-131.; k1[1][3]=2.;
  k1[2][0]=  2.; k1[2][1]=-131.; k1[2][2]= 136.; k1[2][3]=-7.;
  k1[3][0]=  1.; k1[3][1]=   2.; k1[3][2]= -7. ; k1[3][3]=4.;
  
  k2[0][0]=  2.; k2[0][1]= -5. ; k2[0][2]=  4.; k2[0][3]=-1.;
  k2[1][0]= -5.; k2[1][1]=  14.; k2[1][2]=-13.; k2[1][3]= 4.;
  k2[2][0]=  4.; k2[2][1]= -13.; k2[2][2]= 14.; k2[2][3]=-5.;
  k2[3][0]= -1.; k2[3][1]=  4. ; k2[3][2]= -5.; k2[3][3]= 2.;

  k=alpha/120.*k1+beta/2.*k2;
  return k;

}

FEM_CLASS_REGISTER(C1IsoCurve2D)




}} //end namespace itk::fem
