/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTriC02D.cxx
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

#include "itkFEMElementTriC02D.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * Constructor for class TriC02D
 */
TriC02D::TriC02D(  Node::ConstPointer n1_,
          Node::ConstPointer n2_,
          Node::ConstPointer n3_,
          Material::ConstPointer p_ )
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
    m_mat=&dynamic_cast<const MaterialStandard&>(*p_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"TriC02D::TriC02D()");
  }

}



/**
 * Return the stiffness matrix for TriC02D element
 */
vnl_matrix<TriC02D::Float> TriC02D::Ke() const {

  vnl_matrix<Float> MatKe(6,6), shapeD(3,3), shapeINVD(3,2), J(3,3), D(3,3), B(3,6), DB(3,6);
  Float detJ, detJ2;
  int i, j;

  /** Material properties matrix */
  Float disot = (m_mat->E*(1-m_mat->ni))/((1+m_mat->ni)*(1-2*m_mat->ni));
    
  D[0][0] = disot;
  D[0][1] = disot * (m_mat->ni) / (1 - m_mat->ni);
  D[0][2] = 0;

  D[1][0] = D[0][1];
  D[1][1] = disot;
  D[1][2] = 0;

  D[2][0] = 0;
  D[2][1] = 0;
  D[2][2] = disot * (1-2*m_mat->ni)/(2*(1-m_mat->ni));
  
  /** Initialize stiffness matrix */
  MatKe.fill(0.0);

  /**
   * Computes the Jacobian matrix and its determinant
   * at the k-th integration point
   */

  Float x[3] = {0,0,0};

  J = ComputeJacobianMatrixAt(x);
  detJ = JacobianMatrixDeterminant(J);
  detJ2 = detJ / 2.;

  /**
   * Computes the shape function derivatives in Cartesian coordinates
   * at integration point
   */
  shapeINVD = ComputeShapeFunctionCartDerivatives(J, detJ);

  /** Computes the strain (B) matrix */
  B = ComputeBMatrix(shapeINVD);

  /** Computes the matrix multiplication DB */
  DB = ComputeDBMatrix(D,B);

  /** For each row of the stiffness matrix */
  for (i=0; i<6; i++) {

    /** For each column of the stiffness matrix */
    for (j=0; j<6; j++) {

      /** Computes MatKe[i][j] */
      MatKe[i][j] = 0;

      for (int k=0; k<3; k++) {
        MatKe[i][j] += B[k][i] * DB[k][j];
      }

      MatKe[i][j] *= detJ2;
    }
  }

  return MatKe;
}



/**
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void TriC02D::Draw(CDC* pDC) const
{

  int x1=m_node1->X*DC_Scale+m_node1->uX.value*DC_Scale;
  int y1=m_node1->Y*DC_Scale+m_node1->uY.value*DC_Scale;
  
  int x2=m_node2->X*DC_Scale+m_node2->uX.value*DC_Scale;
  int y2=m_node2->Y*DC_Scale+m_node2->uY.value*DC_Scale;
  
  int x3=m_node3->X*DC_Scale+m_node3->uX.value*DC_Scale;
  int y3=m_node3->Y*DC_Scale+m_node3->uY.value*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x1,y1);

}
#endif



/**
 * Returns a vector with global point p corresponding to
 * local point x in the cell.
 */
vnl_vector<TriC02D::Float>
TriC02D::ComputePositionAt(Float x[]) const
{
  vnl_vector<Float> p(2);

  vnl_vector<Float> shapeF = ComputeShapeFunctionsAt(x); 


  p[0] = (m_node1->X * shapeF[0])
     + (m_node2->X * shapeF[1])
     + (m_node3->X * shapeF[2]);

  p[1] = (m_node1->Y * shapeF[0])
     + (m_node2->Y * shapeF[1])
     + (m_node3->Y * shapeF[2]);

  return p;
}



/**
 * Returns the Jacobian matrix at point x, which is given
 * with respect to the local coordinate system.
 */
vnl_matrix<TriC02D::Float>
TriC02D::ComputeJacobianMatrixAt(Float x[]) const
{
  vnl_matrix<Float> J(3,3), shapeD(3,3);

  /**
   * Get the derivatives of the shape functions at given
   * point x
   */
  shapeD = ComputeShapeFunctionDerivativesAt(x);

  /**
   * Compute the elements of the Jacobian matrix
   * for each coordinate of a node w.r.t. global coordinate system
   */
  J[0][0] = J[0][1] = J[0][2] = 1.0; 


  J[1][0] = (shapeD[0][0] * m_node1->X)
        + (shapeD[1][0] * m_node2->X)
        + (shapeD[2][0] * m_node3->X);

  J[1][1] = (shapeD[0][1] * m_node1->X)
        + (shapeD[1][1] * m_node2->X)
        + (shapeD[2][1] * m_node3->X);

  J[1][2] = (shapeD[0][2] * m_node1->X)
        + (shapeD[1][2] * m_node2->X)
        + (shapeD[2][2] * m_node3->X);

  J[2][0] = (shapeD[0][0] * m_node1->Y)
        + (shapeD[1][0] * m_node2->Y)
        + (shapeD[2][0] * m_node3->Y);

  J[2][1] = (shapeD[0][1] * m_node1->Y)
        + (shapeD[1][1] * m_node2->Y)
        + (shapeD[2][1] * m_node3->Y);

  J[2][2] = (shapeD[0][2] * m_node1->Y)
        + (shapeD[1][2] * m_node2->Y)
        + (shapeD[2][2] * m_node3->Y);

  return J;
}



/**
 * Returns a vector with the value of the shape functions
 * at point x, which is given with respect to the local
 * coordinate system.
 */
vnl_vector<TriC02D::Float>
TriC02D::ComputeShapeFunctionsAt(Float x[]) const
{
  /** Linear triangular element has three shape functions */
  vnl_vector<Float> shapeF(3);

  /**
   * given local point x=(r,s,t), where 0 <= r,s,t <= 1 and
   * r + s + t = 1
   */

  /** shape function 1: r */
  shapeF[0] = x[0];

  /** shape function 2: s */
  shapeF[1] = x[1];

  /** shape function 3: t */
  shapeF[2] = x[2];


  return shapeF;
}



/**
 * Return a matrix with the value of the derivatives of
 * the shape functions at point x, which is given with
 * respect to the local coordinate system.
 */
vnl_matrix<TriC02D::Float>
TriC02D::ComputeShapeFunctionDerivativesAt(Float x[]) const
{
  /**
   * Linear triangular element has derivatives of shape
   * functions at directions r and s. These derivatives
   * are constants and thus they do not depend on
   * x = (r, s).
   */
  vnl_matrix<Float> shapeD(3,3);
  
  /** Derivative w.r.t r for shape function 1 (node 1) */
  shapeD[0][0] = 1;

  /** Derivative w.r.t s for shape function 1 (node 1) */
  shapeD[0][1] = 0;

  /** Derivative w.r.t t for shape function 1 (node 1) */
  shapeD[0][2] = 0;

  /** Derivative w.r.t r for shape function 2 (node 2) */
  shapeD[1][0] = 0;

  /** Derivative w.r.t s for shape function 2 (node 2) */
  shapeD[1][1] = 1;

  /** Derivative w.r.t t for shape function 2 (node 2) */
  shapeD[1][2] = 0;

  /** Derivative w.r.t r for shape function 3 (node 3) */
  shapeD[2][0] = 0;

  /** Derivative w.r.t s for shape function 3 (node 3) */
  shapeD[2][1] = 0;

  /** Derivative w.r.t t for shape function 3 (node 3) */
  shapeD[2][2] = 1;

  return shapeD;
}



/**
 * Returns computes the determinant of the Jacobian Matrix
 * at a given point (r,s,t) with respect to the local
 * coordinate system.
 */
TriC02D::Float
TriC02D::JacobianMatrixDeterminant(const vnl_matrix<Float>& J) const
{
  /** Computes the determinant of the Jacobian matrix */
  return ((J[0][0] * J[1][1] * J[2][2]) +
      (J[1][0] * J[2][1] * J[0][2]) +
      (J[2][0] * J[0][1] * J[1][2])
       ) -
       (
      (J[2][0] * J[1][1] * J[0][2]) +
        (J[1][0] * J[0][1] * J[2][2]) +
      (J[0][0] * J[2][1] * J[1][2])
       );
}



/**
 * Return a matrix with the cartesian derivatives of the shape functions.
 */
vnl_matrix<TriC02D::Float>
TriC02D::ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>& J,
                       Float detJ) const
{
  vnl_matrix<Float> shapeINVD(3,2);
  Float invj;

  invj = 1.0 / detJ;

  shapeINVD[0][0] = invj * (J[2][1] - J[2][2]);
  shapeINVD[0][1] = invj * (J[1][2] - J[1][1]);
  shapeINVD[1][0] = invj * (J[2][2] - J[2][0]);
  shapeINVD[1][1] = invj * (J[1][0] - J[1][2]);
  shapeINVD[2][0] = invj * (J[2][0] - J[2][1]);
  shapeINVD[2][1] = invj * (J[1][1] - J[1][0]);

  return shapeINVD;
}



/**
 * Return the strain matrix.
 */
vnl_matrix<TriC02D::Float>
TriC02D::ComputeBMatrix(const vnl_matrix<Float>& shapeINVD) const
{
  vnl_matrix<Float> B(3,6);
  int p;

  /** Computes the inverse shape function derivatives */
  for (int i=0; i<3; i++) {
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
vnl_matrix<TriC02D::Float>
TriC02D::ComputeDBMatrix(const vnl_matrix<Float>& D, const vnl_matrix<Float>& B) const
{
  vnl_matrix<Float> DB(3,6);

  for (int i=0; i<3; i++) {
    for (int j=0; j<6; j++) {
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
vnl_vector<TriC02D::Float>
TriC02D::Fe(LoadElementPointer l) const {
    

  if ( LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l) ) {
    /**
     * Handle gravity loads
     */
    vnl_vector<Float> pt(2), f(2);

    /** Computes point at which gravity load acts on */
    pt[0] = (m_node1->X + m_node2->X + m_node3->X)/3;
    pt[1] = (m_node1->Y + m_node2->Y + m_node3->Y)/3;

    /** Gets gravity load */
    return GravityLoad(f = l0->Fg(pt));
  }
  else if ( LoadEdge::Pointer l0=dynamic_cast<LoadEdge*>(&*l) ) {
    /**
     * Handle edge loads
     * FIXME: LoadEdge is not handled properly in this code
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
vnl_vector<TriC02D::Float>
TriC02D::GravityLoad(const vnl_vector<Float>& f) const
{
  vnl_vector<Float> GL(6), shapeF(3);
  vnl_matrix<Float> J(3,3);
  Float gxcom, gycom, detJ;

  // Talk to Aljaz about that
  // Material properties !!!
  Float dense = 1;
  
  /** Defines gaussian integration points */
  Float GPoints[][3] = {{2./3.,1./6.,1./6.}, {1./6.,2./3.,1./6.}, {1./6.,1./6.,2./3.}};

  /** Declares auxiliary variables */
  Float x[3], detJ2;
  int p;

  /** Initialize load vector */
  GL.fill(0.0);

  /** Computes constants
    * theta should be in radians
    */
  gxcom = dense * f[0];
  gycom = dense * f[1];

  x[0] = x[1] = x[2] = 0.0;
  J = ComputeJacobianMatrixAt(x);
  detJ = JacobianMatrixDeterminant(J);
  detJ2 = detJ / 2.;
  
  /** Carry out numerical integration */
  for (int i=0; i<3; i++) {
    
    /** Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];
    x[2] = GPoints[i][2];

    /** Computes the shape function derivatives at integration point */
    shapeF = ComputeShapeFunctionsAt(x);

    /** Calculates loads and associate with element nodal points */
    for (int j=0; j<3; j++) {
      p = j<<1;

      GL[  p] += gxcom * shapeF[j] * detJ2;
      GL[p+1] += gycom * shapeF[j] * detJ2;
    }
  }

  return GL / 3.;
}



/**
 * Computes the normal and tangential contribution
 * acting on one edge of a QuadC02D element
 */
vnl_vector<TriC02D::Float>
TriC02D::EdgeLoad(Float Pn1, Float Pn2, Float Pt1, Float Pt2,
           Float  x1, Float  y1, Float  x2, Float  y2,
           int n1, int n2) const
{
  vnl_vector<Float> GL(6), shapeF(3);
  vnl_matrix<Float> J(3,3), shapeD(3,3);

  /** Defines gaussian integration points */
  Float GPoints[][3] = {{2./3.,1./3.,0}, {1./3.,2./3.,0}};

  /** Declares auxiliary variables */
  Float x[3], pgash1, pgash2, dgash1, dgash2, pxcom, pycom;

  /** Initializes load vector */
  GL.fill(0.0);

  /** Gaussian numerical integration */
  for (int j=0; j<2; j++) {
    /** Gets Gaussian integration points */
    x[0] = GPoints[j][0];
    x[1] = GPoints[j][1];
    x[2] = GPoints[j][2];

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

    pxcom = 0.5 * ((dgash1 * pgash2) - (dgash2 * pgash1));
    pycom = 0.5 * ((dgash1 * pgash1) + (dgash2 * pgash2));

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
TriC02D::GetNode(int id, int& n1, int& n2) const
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
      n2 = 0;
  }
}



/**
 * Gets the coordinates of a given node
 */
void
TriC02D::GetNodeCoordinates(int n, Float& x, Float& y) const
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
  }
}



/**
 * Read the element from input stream
 */
void TriC02D::Read( std::istream& f, void* info )
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  Node::ArrayType::Pointer nodes=static_cast<ReadInfoType*>(info)->m_node;
  Material::ArrayType::Pointer mats=static_cast<ReadInfoType*>(info)->m_mat;

  /** First call the parent's read function */
  Superclass::Read(f,info);

  try
  {
    /** Read and set the material pointer */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));

    /**
     * Read and set each of the three expected GNN
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node1=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node2=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node3=dynamic_cast<const NodeXY*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"TriC02D::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"TriC02D::Read()","Error reading FEM element!");
  }

}



/**
 * Write the element to the output stream.
 */
void TriC02D::Write( std::ostream& f, int ofid ) const {

  /** if not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** first call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * then the actual data (node, and material numbers)
   * we add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node1->GN<<"\t% NodeXY 1 ID\n";
  f<<"\t"<<m_node2->GN<<"\t% NodeXY 2 ID\n";
  f<<"\t"<<m_node3->GN<<"\t% NodeXY 3 ID\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"TriC02D::Write()","Error writing FEM element!");
  }
}

FEM_CLASS_REGISTER(TriC02D)




}} // end namespace itk::fem
