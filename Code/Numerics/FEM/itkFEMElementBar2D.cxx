/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBar2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

/** disable stupid debug warnings in MSVC++ */
#pragma warning(disable: 4786)

#include "itkFEMElementBar2D.h"
#include "itkFEMLoadPoint.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMUtility.h"
#include "itkFEMObjectFactory.h"
#include "vnl/vnl_vector_fixed.h"
#include <cmath>

namespace itk {
namespace fem {




/**
 * Construct a Bar2D element by specifying two nodes and material propertites
 */
  Bar2D::Bar2D( Node::ConstPointer n1_, Node::ConstPointer n2_, Material::ConstPointer const mat_ ) :
  /**
   * initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * if the node class was incorrect a bad_cast exception is thrown
   */
  m_node1(&dynamic_cast<const NodeXY&>(*n1_)),
  m_node2(&dynamic_cast<const NodeXY&>(*n2_)),
  m_mat(&dynamic_cast<const MaterialStandard&>(*mat_))
{
}



  
/**
 * Return the stiffness matrix for the element.
 */
vnl_matrix<Bar2D::Float> Bar2D::Ke() const {

vnl_matrix<Float> k(NDOF,NDOF);

Float x=m_node2->X-m_node1->X;
Float y=m_node2->Y-m_node1->Y;
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
#ifdef _FEM_Build_Visualization_Routines_
void Bar2D::Draw(CDC* pDC) const {

  int x1=m_node1->X*DC_Scale+m_node1->uX.value*DC_Scale;
  int y1=m_node1->Y*DC_Scale+m_node1->uY.value*DC_Scale;
  int x2=m_node2->X*DC_Scale+m_node2->uX.value*DC_Scale;
  int y2=m_node2->Y*DC_Scale+m_node2->uY.value*DC_Scale;

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

  /** read and set the material pointer */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  if ( !(m_mat=dynamic_cast<MaterialStandard*>( &*mats->Find(n)) ) )
  {
    throw std::runtime_error("Global element properties number not found!");
  }

  /** read and set first GNN */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  if ( !(m_node1=dynamic_cast<NodeXY*>( &*nodes->Find(n)) ) )
  {
    throw std::runtime_error("Global node number not found!");
  }

  /** read and set second GNN */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  if ( !(m_node2=dynamic_cast<NodeXY*>( &*nodes->Find(n)) ) ) 
  {
    throw std::runtime_error("Global node number not found!");
  }

out:

  if( !f ) {
    throw std::runtime_error("Error reading element!");
  }

}




/**
 * Return the force vector for the element
 */
vnl_vector<Bar2D::Float> Bar2D::Fe(LoadElementPointer l) const {
  if ( LoadPoint::Pointer l0=dynamic_cast<LoadPoint*>(&*l) ) {
    /**
     * Handle point loads 
     */
    vnl_vector_fixed<Float,2> n1(m_node1->X,m_node1->Y);
    vnl_vector_fixed<Float,2> n2(m_node2->X,m_node2->Y);
    Float l=(n1-n2).magnitude();
    Float l1=(l0->point-n1).magnitude();
    Float l2=(l0->point-n2).magnitude();
    vnl_vector_fixed<Float,2> F1=l0->Fp*l2/l;
    vnl_vector_fixed<Float,2> F2=l0->Fp*l1/l;

    vnl_vector<Float> F(4);
    F.update(F1,0);
    F.update(F2,2);
    return F;

  } else if ( LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l) ) {
    /**
     * Handle gravity loads
     */
    vnl_vector_fixed<Float,2> n1(m_node1->X,m_node1->Y);
    vnl_vector_fixed<Float,2> n2(m_node2->X,m_node2->Y);
    Float l=(n1-n2).magnitude();
    vnl_vector_fixed<Float,2> dn=(n2-n1)/l;

    double a=0;
    double b=l;
    int n=10;  /** number of integration points (even numbers only) */
    vnl_vector_fixed<Float,2> F1,F2;

    {
      double scale, t, tl, tu;
      int i, m, ibase;

      /**  Begin integration  */
      scale = (b - a)/2.0;
      m = n/2;
      ibase = m*m;
      F1.fill(0.0);
      F2.fill(0.0);
            
      for (i=1; i <= m; i++)  {
        t = GaussIntegrate::z[ibase + i - 1];
        tl = (a*(1.0 + t) + (1.0 - t)*b)/2.0;
        tu = (a*(1.0 - t) + (1.0 + t)*b)/2.0;
        F1 = F1 + GaussIntegrate::w[ibase + i - 1]*(l0->Fg(n1+dn*tl)*(l-tl)/l  +l0->Fg(n1+dn*tu)*(l-tu)/l);
        F2 = F2 + GaussIntegrate::w[ibase + i - 1]*(l0->Fg(n1+dn*tl)*tl/l  +l0->Fg(n1+dn*tu)*tu/l);
      }

      F1=scale*F1;
      F2=scale*F2;

    }

    vnl_vector<Float> F(4);
    F.update(F1,0);
    F.update(F2,2);
    return F;

  }
  else {
    /** we can't handle this load, pass it over to the parent class */
    return Superclass::Fe(l);
  }
}


  
  
/** 
 * Write the element to the output stream
 */
void Bar2D::Write( std::ostream& f, int ofid ) const {

  /** if not set already, se set the ofid */
  if (ofid<0) { ofid=OFID; }

  /**  first call the parent's write function */
  Superclass::Write(f,ofid);

  /** 
   * then write the actual data (node, and material numbers)
   * we add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node1->GN<<"\t% NodeXY 1 ID\n";
  f<<"\t"<<m_node2->GN<<"\t% NodeXY 2 ID\n";

  /** check for errors */
  if( !f ) {
    throw std::runtime_error("Error writing element!");
  }

}

FEM_CLASS_REGISTER(Bar2D)




}} // end namespace itk::fem
