/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementQuadC02D.cxx
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

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElementQuadC02D.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * Constructor for class QuadC02D
 */
QuadC02D::QuadC02D(  Node::ConstPointer n1_,
          Node::ConstPointer n2_,
          Node::ConstPointer n3_,
          Node::ConstPointer n4_,
          Material::ConstPointer p_)
{
  try
  {
    /**
     * Initialize the pointers to nodes and check that
     * we were given the pointers to the right node class.
     * if the node class was incorrect a bad_cast exception is thrown
     */
    m_node1=&dynamic_cast<const NodeXY&>(*n1_);
    m_node2=&dynamic_cast<const NodeXY&>(*n2_);
    m_node3=&dynamic_cast<const NodeXY&>(*n3_);
    m_node4=&dynamic_cast<const NodeXY&>(*n4_);
    m_mat=&dynamic_cast<const MaterialStandard&>(*p_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"QuadC02D::QuadC02D()");
  }

}




/**
 * Returns the stiffness matrix for QuadC02D element
 */
vnl_matrix<QuadC02D::Float> QuadC02D::Ke() const 
{
  vnl_matrix<Float> MatKe(8,8), I(2,2), shapeD(4,2), shapeINVD(4,2),
            J(2,2), D(3,3), B(3,8), DB(3,8);

  Float detJ;
  
  /** Gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-pt, -pt}, {pt, -pt}, {pt, pt}, {-pt, pt}};

  /** Material properties matrix */
  Float disot = (m_mat->E*(1-m_mat->ni))/((1+m_mat->ni)*(1-2*m_mat->ni));
    
  D(0,0) = disot;
  D(0,1) = disot * (m_mat->ni) / (1 - m_mat->ni);
  D(0,2) = 0;

  D(1,0) = D(0,1);
  D(1,1) = disot;
  D(1,2) = 0;

  D(2,0) = 0;
  D(2,1) = 0;
  D(2,2) = disot * (1-2*m_mat->ni)/(2*(1-m_mat->ni));
  
  Float x[2];
  int i, j;


  /** Initialize stiffness matrix */
  MatKe.fill(0.0);

  /** For each integration point */
  for (int k=0; k<4; k++) {
    /** Get the k-th integration point */
    x[0] = GPoints[k][0];
    x[1] = GPoints[k][1];


    /**
     * Computes the Jacobian matrix and its determinant
     * at the k-th integration point
     */
    J = ComputeJacobianMatrixAt(x);
    detJ = JacobianMatrixDeterminant(J);

    /** Computes the inverse of the Jacobian matrix */
    I = ComputeJacobianInverse(J, detJ);

    /** Computes the shape function derivatives at integration point */
    shapeD = ComputeShapeFunctionDerivativesAt(x);

    /**
     * Computes the shape function derivatives in Cartesian coordinates
     * at integration point
     */
    shapeINVD = ComputeShapeFunctionCartDerivatives(I, shapeD);

    /** Computes the strain (B) matrix */
    B = ComputeBMatrix(shapeINVD);

    /** Computes the matrix multiplication DB */
    DB = ComputeDBMatrix(D,B);

    /**
     * Add the contribution of k-th integration point to
     * the stiffness matrix
     */

    /** For each row of the stiffness matrix */
    for (i=0; i<8; i++) {

      /** For each column of the stiffness matrix */
      for (j=0; j<8; j++) {

        /** Compute MatKe(i,j) */
        Float temp = 0;
        for (int k=0; k<3; k++) {
          temp += B[k][i] * DB[k][j];
        }

        MatKe[i][j] += (detJ * temp);
      }
    }
  }

  return MatKe;
}



/**
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void QuadC02D::Draw(CDC* pDC) const 
{

  int x1=m_node1->X*DC_Scale+m_node1->uX.value*DC_Scale;
  int y1=m_node1->Y*DC_Scale+m_node1->uY.value*DC_Scale;
  
  int x2=m_node2->X*DC_Scale+m_node2->uX.value*DC_Scale;
  int y2=m_node2->Y*DC_Scale+m_node2->uY.value*DC_Scale;
  
  int x3=m_node3->X*DC_Scale+m_node3->uX.value*DC_Scale;
  int y3=m_node3->Y*DC_Scale+m_node3->uY.value*DC_Scale;
  
  int x4=m_node4->X*DC_Scale+m_node4->uX.value*DC_Scale;
  int y4=m_node4->Y*DC_Scale+m_node4->uY.value*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x4,y4);
  pDC->LineTo(x1,y1);

}
#endif


/**
 * Returns a vector with global point p corresponding to
 * local point x in the cell.
 */
vnl_vector<QuadC02D::Float>
QuadC02D::ComputePositionAt(Float x[]) const
{
  vnl_vector<Float> p(2);
  
  vnl_vector<Float> shapeF = ComputeShapeFunctionsAt(x); 
  
  p[0] = (m_node1->X * shapeF[0])
     + (m_node2->X * shapeF[1])
     + (m_node3->X * shapeF[2])
     + (m_node4->X * shapeF[3]);

  p[1] = (m_node1->Y * shapeF[0])
     + (m_node2->Y * shapeF[1])
     + (m_node3->Y * shapeF[2])
     + (m_node4->Y * shapeF[3]);

  return p;
}




/**
 * Returns the Jacobian matrix at point x, which is given
 * with respect to the local coordinate system.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeJacobianMatrixAt(Float x[]) const
{
  vnl_matrix<Float> J(2,2);

  /**
   * Get the derivatives of the shape functions at given
   * point x
   */
  vnl_matrix<Float> shapeD = ComputeShapeFunctionDerivativesAt(x);

  /**
   * Compute the elements of the Jacobian matrix
   * for each coordinate of a node w.r.t. global coordinate system
   */
  J[0][0] = (shapeD[0][0] * m_node1->X)
        + (shapeD[1][0] * m_node2->X)
        + (shapeD[2][0] * m_node3->X)
        + (shapeD[3][0] * m_node4->X);

  J[0][1] = (shapeD[0][1] * m_node1->X)
        + (shapeD[1][1] * m_node2->X)
        + (shapeD[2][1] * m_node3->X)
        + (shapeD[3][1] * m_node4->X);

  J[1][0] = (shapeD[0][0] * m_node1->Y)
        + (shapeD[1][0] * m_node2->Y)
        + (shapeD[2][0] * m_node3->Y)
        + (shapeD[3][0] * m_node4->Y);
    
  J[1][1] = (shapeD[0][1] * m_node1->Y)
        + (shapeD[1][1] * m_node2->Y)
        + (shapeD[2][1] * m_node3->Y)
        + (shapeD[3][1] * m_node4->Y);

  return J;
}




/**
 * Returns a vector with the value of the shape functions
 * at point x, which is given with respect to the local
 * coordinate system.
 */
vnl_vector<QuadC02D::Float>
QuadC02D::ComputeShapeFunctionsAt(Float x[]) const
{
  /** Linear quadrilateral element has four shape functions  */
  vnl_vector<Float> shapeF(4);
  
  /**
   * Linear quadrilateral element has local coordinates
   * (-1,-1), (1,-1), (1,1), and (-1,1)
   */
  
  /** given local point x=(r,s), where -1 <= r,s <= 1 and */

  /** shape function 1: ((1 - r) * (1 - s)) / 4  (node 1) */
  shapeF[0] = (1 - x[0]) * (1 - x[1]) * .25;

  /** shape function 2: ((1 + r) * (1 - s)) / 4  (node 2) */
  shapeF[1] = (1 + x[0]) * (1 - x[1]) * .25;

  /** shape function 3: ((1 + r) * (1 + s)) / 4  (node 3) */
  shapeF[2] = (1 + x[0]) * (1 + x[1]) * .25;

  /** shape function 1: ((1 - r) * (1 + s)) / 4  (node 4) */
  shapeF[3] = (1 - x[0]) * (1 + x[1]) * .25;

  return shapeF;
}



/**
 * Return a matrix with the value of the derivatives of
 * the shape functions at point x, which is given with
 * respect to the local coordinate system.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeShapeFunctionDerivativesAt(Float x[]) const
{
  /** functions at directions r and s.  */
  vnl_matrix<Float> shapeD(4,2);

  /** Derivative w.r.t r for shape function 1 (node 1) */
  shapeD[0][0] = -(1 - x[1]) * .25;

  /** Derivative w.r.t s for shape function 1 (node 1) */
  shapeD[0][1] = -(1 - x[0]) * .25;

  /** Derivative w.r.t r for shape function 2 (node 2) */
  shapeD[1][0] = +(1 - x[1]) * .25;

  /** Derivative w.r.t s for shape function 2 (node 2) */
  shapeD[1][1] = -(1 + x[0]) * .25;

  /** Derivative w.r.t r for shape function 3 (node 3) */
  shapeD[2][0] = +(1 + x[1]) * .25;

  /** Derivative w.r.t s for shape function 3 (node 3) */
  shapeD[2][1] = +(1 + x[0]) * .25;

  /** Derivative w.r.t r for shape function 4 (node 4) */
  shapeD[3][0] = -(1 + x[1]) * .25;

  /** Derivative w.r.t s for shape function 4 (node 4) */
  shapeD[3][1] = +(1 - x[0]) * .25;

  return shapeD;
}



/**
 * Returns computes the determinant of the Jacobian Matrix
 * at a given point (r,s) with respect to the local
 * coordinate system.
 */
QuadC02D::Float
QuadC02D::JacobianMatrixDeterminant(const vnl_matrix<Float>& J) const
{
  /** Computes the determinant of the Jacobian matrix */
  return (J[0][0] * J[1][1]) - (J[1][0] * J[0][1]);
}



/**
 * Returns the inverse transformation matrix from the master element to a
 * quadrilateral element.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeJacobianInverse(const vnl_matrix<Float>& J, Float detJ) const
{
  vnl_matrix<Float> I(2,2);

  /** Computes the inverse of the shape functions derivatives */
  I[0][0] =  J[1][1] / detJ;
  I[0][1] = -J[0][1] / detJ;
  I[1][0] = -J[1][0] / detJ;
  I[1][1] =  J[0][0] / detJ;

  return I;
}



/**
 * Return a matrix with the cartesian derivatives of the shape functions.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>& I,
        const vnl_matrix<Float>& shapeD) const
{
  vnl_matrix<Float> shapeINVD(4,2);

  for (int i=0; i<4; i++) {
    shapeINVD[i][0] = (shapeD[i][0] * I[0][0]) + (shapeD[i][1] * I[1][0]);
    shapeINVD[i][1] = (shapeD[i][1] * I[0][1]) + (shapeD[i][1] * I[1][1]);
  }

  return shapeINVD;
}



/**
 * Return the strain matrix.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeBMatrix(const vnl_matrix<Float>& shapeINVD) const
{
  vnl_matrix<Float> B(3,8);
  int p;

  /** Computes the inverse shape function derivatives */
  for (int i=0; i<4; i++) {
    /** Computes B index */
    p = i << 1;

    /** Compute B elements */
    B[0][p]   = shapeINVD[i][0];
    B[0][p+1] = 0;
    B[1][p]   = 0;
    B[1][p+1] = shapeINVD[i][1];
    B[2][p]   = shapeINVD[i][1];
    B[2][p+1] = shapeINVD[i][0];
  }

  return B;
}




/**
 * Return the result of multiplying the elastic constant
 * matrix by the strain matrix.
 */
vnl_matrix<QuadC02D::Float>
QuadC02D::ComputeDBMatrix(const vnl_matrix<Float>& D, const vnl_matrix<Float>& B) const
{
  vnl_matrix<Float> DB(3,8);

  for (int i=0; i<3; i++) {
    for (int j=0; j<8; j++) {
      DB[i][j] = 0;
      for (int k=0; k<3; k++) {
        DB[i][j] += D[i][k] * B[k][j];
      }
    }
  }

  return DB;
}



/**
 * Return the force vector for QuadC02D element
 */
vnl_vector<QuadC02D::Float>
QuadC02D::Fe(LoadElementPointer l) const {
    

  if ( LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l) ) {
    /**
     * Handle gravity loads
     */
    vnl_vector<Float> pt(2), f(2);

    /** Computes point at which gravity load acts on */
    pt[0] = (m_node1->X + m_node2->X + m_node3->X + m_node4->X)/4;
    pt[1] = (m_node1->Y + m_node2->Y + m_node3->Y + m_node4->Y)/4;

    /** Gets gravity load */
    return GravityLoad(f = l0->Fg(pt));
  }
  else if ( LoadEdge::Pointer l0=dynamic_cast<LoadEdge*>(&*l) ) {
    /**
     * Handle edge loads
     */
    int n1, n2;
    Float Pn1, Pt1, Pn2, Pt2;
    Float x1,x2, y1,y2;
    
    /** Get node local numbers and coordinates */
    GetNode(l0->m_Edge, n1, n2);
    GetNodeCoordinates(n1, x1, y1);
    GetNodeCoordinates(n2, x2, y2);

    /** Gets normal and tangential force acting on the edge */
    /** Normal forces */ 
    Pn1 = l0->m_Force[0][0];
    Pn2 = l0->m_Force[1][0];

    /** Tangential forces */ 
    Pt1 = l0->m_Force[0][1];
    Pt2 = l0->m_Force[1][1];

    return EdgeLoad(Pn1, Pt1, Pn2, Pt2, x1, y1, x2, y2, n1, n2);
  }
  else {
    /** we can't handle this load, pass it over to the parent class */
    return Superclass::Fe(l);
  }
}



/**
 * Computes the gravity load contribution to a QuadC02D element
 */
vnl_vector<QuadC02D::Float>
QuadC02D::GravityLoad(const vnl_vector<Float>& f) const
{
  vnl_vector<Float> GL(8), shapeF(4);
  vnl_matrix<Float> J(2,2);
  Float gxcom, gycom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /** Defines gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-pt, -pt}, {pt, -pt}, {pt, pt}, {-pt, pt}};

  /** Declares auxiliary variables */
  Float x[2];
  int p;

  /** Initialize load vector */
  GL.fill(0.0);

  /** Computes constants
    * theta should be in radians
    */
  gxcom = dense * f[0];
  gycom = dense * f[1];
  
  /** Carry out numerical integration */
  for (int i=0; i<4; i++) {
    
    /** Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];

    /** Computes the shape function derivatives at integration point */
    shapeF = ComputeShapeFunctionsAt(x);

    /** Computes the Jacobian matrix and its determinant
      * at the i-th integration point
      */
    J = ComputeJacobianMatrixAt(x);
    detJ = JacobianMatrixDeterminant(J);

    /** Calculates loads and associate with element nodal points */
    for (int j=0; j<4; j++) {
      p = j<<1;

      GL[  p] += gxcom * shapeF[j] * detJ;
      GL[p+1] += gycom * shapeF[j] * detJ;
    }
  }

  return GL;
}



/**
 * Computes the normal and tangential contribution
 * acting on one edge of a QuadC02D element
 */
vnl_vector<QuadC02D::Float>
QuadC02D::EdgeLoad(Float Pn1, Float Pn2, Float Pt1, Float Pt2,
           Float  x1, Float  y1, Float  x2, Float  y2,
           int n1, int n2) const
{
  vnl_vector<Float> GL(8), shapeF(4);
  vnl_matrix<Float> J(2,2), shapeD(4,2);

  /** Defines gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[2] = {-pt, pt};

  /** Declares auxiliary variables */
  double x[2], pgash1, pgash2, dgash1, dgash2, pxcom, pycom;

  /** Initializes load vector */
  GL.fill(0.0);

  /** Gaussian numerical integration */
  for (int j=0; j<2; j++) {
    /** Gets Gaussian integration points */
    x[0] = GPoints[j];
    x[1] = 0.0;

    /** Computes the shape function derivatives at
      * integration point
      */
    shapeF = ComputeShapeFunctionsAt(x);

    /** Computes the shape function derivatives at
      * integration point
      */
    shapeD = ComputeShapeFunctionDerivativesAt(x);

    /** Computes components of the equivalent nodal loads */
    pgash1 = (Pn1 * shapeF(0)) + (Pn2 * shapeF(1));
    pgash2 = (Pt1 * shapeF(0)) + (Pt2 * shapeF(1));
    dgash1 = (x1 * shapeD(0,0)) + (x2 * shapeD(1,0));
    dgash2 = (y1 * shapeD(0,0)) + (y2 * shapeD(1,0));

    pxcom = (dgash1 * pgash2) - (dgash2 * pgash1);
    pycom = (dgash1 * pgash1) + (dgash2 * pgash2);

    GL[      n1 << 1] += shapeF(n1) * pxcom;
    GL[(n1 << 1) + 1] += shapeF(n1) * pycom;
    GL[      n2 << 1] += shapeF(n2) * pxcom;
    GL[(n2 << 1) + 1] += shapeF(n2) * pycom;
  }

  return GL;
}



/**
 * Gets the indices of the nodes defining an edge
 */
void
QuadC02D::GetNode(int id, int& n1, int& n2) const
{
  switch (id) {
    case 0 :
      n1 = 0;
      n2 = 1;
      break;
    case 1 :
      n1 = 1;
      n2 = 2;
      break;
    case 2 :
      n1 = 2;
      n2 = 3;
      break;
    case 3 :
      n1 = 3;
      n2 = 1;
  }
}



/**
 * Gets the coordinates of a given node
 */
void
QuadC02D::GetNodeCoordinates(int n, Float& x, Float& y) const
{
  switch (n) {
    case 0 :
      x = m_node1->X;
      y = m_node1->Y;
      break;
    case 1 :
      x = m_node2->X;
      y = m_node2->Y;
      break;
    case 2 :
      x = m_node3->X;
      y = m_node3->Y;
      break;
    case 3 :
      x = m_node4->X;
      y = m_node4->Y;
  }
}



/**
 * Read the element from input stream
 */
void QuadC02D::Read( std::istream& f, void* info )
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
    /**
     * Read and set the material pointer
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));

    /**
     * Read and set each of the four expected GNN
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node1=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node2=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node3=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node4=dynamic_cast<const NodeXY*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"QuadC02D::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"QuadC02D::Read()","Error reading FEM element!");
  }

}




/**
 * Write the element to the output stream.
 */
void QuadC02D::Write( std::ostream& f, int ofid ) const {

  /** If not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** First call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * then the actual data (node, and material numbers)
   * we add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node1->GN<<"\t% NodeXY 1 ID\n";
  f<<"\t"<<m_node2->GN<<"\t% NodeXY 2 ID\n";
  f<<"\t"<<m_node3->GN<<"\t% NodeXY 3 ID\n";
  f<<"\t"<<m_node4->GN<<"\t% NodeXY 4 ID\n";

  /** check for errors */
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"QuadC02D::Write()","Error writing FEM element!");
  }

}

FEM_CLASS_REGISTER(QuadC02D)




}} // end namespace itk::fem
