/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementMembraneC02D.cxx
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

#include "itkFEMElementMembraneC02D.h"
#include "FEM/itkFEMLoadGrav.h"
#include "FEM/itkFEMLoadEdge.h"
#include "FEM/itkFEMObjectFactory.h"
#include "FEM/itkFEMUtility.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * Constructor for class MembraneC02D
 */
MembraneC02D::MembraneC02D(  Node::ConstPointer n1_,
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
    m_node[0]=&dynamic_cast<const NodeXY&>(*n1_);
    m_node[1]=&dynamic_cast<const NodeXY&>(*n2_);
    m_node[2]=&dynamic_cast<const NodeXY&>(*n3_);
    m_node[3]=&dynamic_cast<const NodeXY&>(*n4_);
    m_mat=&dynamic_cast<const MaterialStandard&>(*p_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"MembraneC02D::MembraneC02D()");
  }

}

 /**
   * Function that returns the global coordinate at the selected node.
   */
vnl_vector<MembraneC02D::Float> MembraneC02D::GetNodalCoordinates( unsigned int n ) const
{

  vnl_vector<Float> coord(2,0.0);

  coord[0]=m_node[n]->X;
  coord[1]=m_node[n]->Y;
  return coord;
}



/**
 * Returns the stiffness matrix for MembraneC02D element
 */
vnl_matrix<MembraneC02D::Float> MembraneC02D::Ke() const 
{
  vnl_matrix<Float> MatKe(8,8), I(2,2), shapeD(4,2), shapeINVD(4,2),
            J(2,2), D(3,3,0.0), B(3,8), DB(3,8);

  Float detJ;
  
  /** Gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-pt, -pt}, {pt, -pt}, {pt, pt}, {-pt, pt}};

  /** Material properties matrix */
  Float disot = m_mat->E;
    
  D(0,0) = disot;
  D(1,1) = disot;
  D(2,2) = disot;

  Float x[2];

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
//  std::cout << B << std::endl;
    /** Computes the matrix multiplication DB */
 //   DB = ComputeDBMatrix(D,B);

    /**
     * Add the contribution of k-th integration point to
     * the stiffness matrix
     */
  MatKe += detJ*(B.transpose()*(D*B));
  }
  return MatKe;
}



/**
 * Returns the stiffness matrix for MembraneC02D element
 */
vnl_matrix<MembraneC02D::Float> MembraneC02D::Me() const 
{
  vnl_matrix<Float> MatMe(8,8), I(2,2),  shapeFINV(4,2), 
     shapeF(4,2), J(2,2), Nmat(3,8);

  Float detJ;
//  Float rho=1.;
  
  /** Gaussian integration points */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-pt, -pt}, {pt, -pt}, {pt, pt}, {-pt, pt}};


  Float x[2];

  /** Initialize stiffness matrix */
  MatMe.fill(0.0);

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

    /** Computes the shape function at integration point */
    
    vnl_vector<Float> temp = ComputeShapeFunctionsAt(x); 
  shapeF.set_column(0,temp);
  shapeF.set_column(1,temp);
  
    /**
     * Computes the shape function in Cartesian coordinates
     * at integration point
     */
  shapeFINV = ComputeShapeFunctionCartDerivatives(I, shapeF);

    Nmat = ComputeBMatrix(shapeFINV);

    /**
     * Add the contribution of k-th integration point to
     * the stiffness matrix
     */
    MatMe += detJ*(Nmat.transpose()*Nmat);
  }
 
  return MatMe;
}



/**
 * Draw the element on device context pDC.
 */
#ifdef FEM_BUILD_VISUALIZATION
void MembraneC02D::Draw(CDC* pDC, Solution::ConstPointer sol) const 
{

  int x1=m_node[0]->X*DC_Scale;
  int y1=m_node[0]->Y*DC_Scale;
  
  int x2=m_node[1]->X*DC_Scale;
  int y2=m_node[1]->Y*DC_Scale;
  
  int x3=m_node[2]->X*DC_Scale;
  int y3=m_node[2]->Y*DC_Scale;
  
  int x4=m_node[3]->X*DC_Scale;
  int y4=m_node[3]->Y*DC_Scale;

  x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
  y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;
  x2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(2))*DC_Scale;
  y2+=sol->GetSolutionValue(this->GetDegreeOfFreedom(3))*DC_Scale;
  x3+=sol->GetSolutionValue(this->GetDegreeOfFreedom(4))*DC_Scale;
  y3+=sol->GetSolutionValue(this->GetDegreeOfFreedom(5))*DC_Scale;
  x4+=sol->GetSolutionValue(this->GetDegreeOfFreedom(6))*DC_Scale;
  y4+=sol->GetSolutionValue(this->GetDegreeOfFreedom(7))*DC_Scale;

  pDC->MoveTo(x1,y1);
  pDC->LineTo(x2,y2);
  pDC->LineTo(x3,y3);
  pDC->LineTo(x4,y4);
  pDC->LineTo(x1,y1);

}
#endif


vnl_vector<MembraneC02D::Float> MembraneC02D::ShapeFunctions
( const vnl_vector<MembraneC02D::Float>& x ) const
{
 /* Linear quadrilateral element has four shape functions  */
  vnl_vector<Float> shapeF(4);
  
  /*
   * Linear quadrilateral element has local coordinates
   * (-1,-1), (1,-1), (1,1), and (-1,1)
   */
  
  /* given local point x=(r,s), where -1 <= r,s <= 1 and */

  /* shape function 1: ((1 - r) * (1 - s)) / 4  (node 1) */
  shapeF[0] = (1 - x[0]) * (1 - x[1]) * .25;

  /* shape function 2: ((1 + r) * (1 - s)) / 4  (node 2) */
  shapeF[1] = (1 + x[0]) * (1 - x[1]) * .25;

  /* shape function 3: ((1 + r) * (1 + s)) / 4  (node 3) */
  shapeF[2] = (1 + x[0]) * (1 + x[1]) * .25;

  /* shape function 1: ((1 - r) * (1 + s)) / 4  (node 4) */
  shapeF[3] = (1 - x[0]) * (1 + x[1]) * .25;

  return shapeF;
}

/**
 * Returns a vector with global point p corresponding to
 * local point x in the cell.
 */
vnl_vector<MembraneC02D::Float>
MembraneC02D::ComputePositionAt(Float x[]) const
{
  vnl_vector<Float> p(2);
  
  vnl_vector<Float> shapeF = ComputeShapeFunctionsAt(x); 
  
  p[0] = (m_node[0]->X * shapeF[0])
     + (m_node[1]->X * shapeF[1])
     + (m_node[2]->X * shapeF[2])
     + (m_node[3]->X * shapeF[3]);

  p[1] = (m_node[0]->Y * shapeF[0])
     + (m_node[1]->Y * shapeF[1])
     + (m_node[2]->Y * shapeF[2])
     + (m_node[3]->Y * shapeF[3]);

  return p;
}




/**
 * Returns the Jacobian matrix at point x, which is given
 * with respect to the local coordinate system.
 */
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeJacobianMatrixAt(Float x[]) const
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
  J[0][0] = (shapeD[0][0] * m_node[0]->X)
        + (shapeD[1][0] * m_node[1]->X)
        + (shapeD[2][0] * m_node[2]->X)
        + (shapeD[3][0] * m_node[3]->X);

  J[0][1] = (shapeD[0][1] * m_node[0]->X)
        + (shapeD[1][1] * m_node[1]->X)
        + (shapeD[2][1] * m_node[2]->X)
        + (shapeD[3][1] * m_node[3]->X);

  J[1][0] = (shapeD[0][0] * m_node[0]->Y)
        + (shapeD[1][0] * m_node[1]->Y)
        + (shapeD[2][0] * m_node[2]->Y)
        + (shapeD[3][0] * m_node[3]->Y);
    
  J[1][1] = (shapeD[0][1] * m_node[0]->Y)
        + (shapeD[1][1] * m_node[1]->Y)
        + (shapeD[2][1] * m_node[2]->Y)
        + (shapeD[3][1] * m_node[3]->Y);

  return J;
}




/**
 * Returns a vector with the value of the shape functions
 * at point x, which is given with respect to the local
 * coordinate system.
 */
vnl_vector<MembraneC02D::Float>
MembraneC02D::ComputeShapeFunctionsAt(Float x[]) const
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

MembraneC02D::Float 
MembraneC02D::InterpolateScalarWithShapeFunctions(VectorType shapeF,VectorType Vals)
{

  return Vals[0]*shapeF[0]+Vals[1]*shapeF[1]+Vals[2]*shapeF[2]+Vals[3]*shapeF[3];
  
}

 /* This method interpolates a function at a point in local coordinates on the interior 
  *   of the element from values of that function at the nodes.  
  *   The interpolation is done with the shape functions
  */
MembraneC02D::VectorType 
MembraneC02D::InterpolateWithShapeFunctions(VectorType shapeF,VectorType Vals)
{

  unsigned int vsz=Vals.size();
  unsigned int numfuns=vsz / 4;
  vnl_vector<Float> f(numfuns,0.0);
  Float temp=0.0;

  for (unsigned int i=0; i<numfuns; i++) 
  {
    std::cout << " sols " << 
                 shapeF[0] << " " <<
                 shapeF[1] << " " << 
                 shapeF[2] << " " << 
                 shapeF[3] << std::endl;

    std::cout << " vals " << 
                 Vals[4*i+0] << " " <<
                 Vals[4*i+1] << " " << 
                 Vals[4*i+2] << " " << 
                 Vals[4*i+3] << std::endl;
   
    temp=Vals[4*i+0]*shapeF[0]+Vals[4*i+1]*shapeF[1]+Vals[4*i+2]*shapeF[2]+Vals[4*i+3]*shapeF[3];
    f[i]=temp;
  }
  return f;

}


MembraneC02D::VectorType MembraneC02D::InterpolateSolutionAt(VectorType shapeF,Solution* S)
{
  vnl_vector_fixed<Float,2> sol(0.0);

  sol[0]=S->GetSolutionValue(GetDegreeOfFreedom(0))*shapeF[0]
        +S->GetSolutionValue(GetDegreeOfFreedom(2))*shapeF[1]
        +S->GetSolutionValue(GetDegreeOfFreedom(4))*shapeF[2]
        +S->GetSolutionValue(GetDegreeOfFreedom(6))*shapeF[3];
  sol[1]=S->GetSolutionValue(GetDegreeOfFreedom(1))*shapeF[0]
        +S->GetSolutionValue(GetDegreeOfFreedom(3))*shapeF[1]
        +S->GetSolutionValue(GetDegreeOfFreedom(5))*shapeF[2]
        +S->GetSolutionValue(GetDegreeOfFreedom(7))*shapeF[3];

  return sol;
}


/**
 * Return a matrix with the value of the derivatives of
 * the shape functions at point x, which is given with
 * respect to the local coordinate system.
 */
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeShapeFunctionDerivativesAt(Float x[]) const
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
MembraneC02D::Float
MembraneC02D::JacobianMatrixDeterminant(const vnl_matrix<Float>& J) const
{
  /** Computes the determinant of the Jacobian matrix */
  return (J[0][0] * J[1][1]) - (J[1][0] * J[0][1]);
}



/**
 * Returns the inverse transformation matrix from the master element to a
 * quadrilateral element.
 */
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeJacobianInverse(const vnl_matrix<Float>& J, Float detJ) const
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
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>& I,
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
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeBMatrix(const vnl_matrix<Float>& shapeINVD) const
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
vnl_matrix<MembraneC02D::Float>
MembraneC02D::ComputeDBMatrix(const vnl_matrix<Float>& D, const vnl_matrix<Float>& B) const
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
 * Computes the gravity load contribution to a MembraneC02D element
 */
vnl_vector<MembraneC02D::Float>
MembraneC02D::GravityLoad(const vnl_vector<Float>& f) const
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
  //std::cout << GL << std::endl;
  return GL;
}



/**
 * Computes the normal and tangential contribution
 * acting on one edge of a MembraneC02D element
 */
vnl_vector<MembraneC02D::Float>
MembraneC02D::EdgeLoad(Float Pn1, Float Pn2, Float Pt1, Float Pt2,
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
MembraneC02D::GetNode(int id, int& n1, int& n2) const
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
MembraneC02D::GetNodeCoordinates(int n, Float& x, Float& y) const
{
  switch (n) {
    case 0 :
      x = m_node[0]->X;
      y = m_node[0]->Y;
      break;
    case 1 :
      x = m_node[1]->X;
      y = m_node[1]->Y;
      break;
    case 2 :
      x = m_node[2]->X;
      y = m_node[2]->Y;
      break;
    case 3 :
      x = m_node[3]->X;
      y = m_node[3]->Y;
  }
}



/**
 * Read the element from input stream
 */
void MembraneC02D::Read( std::istream& f, void* info )
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
    /**
     * Read and set the material pointer
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));

    /**
     * Read and set each of the four expected GNN
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[0]=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[1]=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[2]=dynamic_cast<const NodeXY*>( &*nodes->Find(n));

    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_node[3]=dynamic_cast<const NodeXY*>( &*nodes->Find(n));
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"MembraneC02D::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"MembraneC02D::Read()","Error reading FEM element!");
  }

}




/**
 * Write the element to the output stream.
 */
void MembraneC02D::Write( std::ostream& f ) const
{
  /** First call the parent's write function */
  Superclass::Write(f);

  /**
   * then the actual data (node, and material numbers)
   * we add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  f<<"\t"<<m_node[0]->GN<<"\t% NodeXY 1 ID\n";
  f<<"\t"<<m_node[1]->GN<<"\t% NodeXY 2 ID\n";
  f<<"\t"<<m_node[2]->GN<<"\t% NodeXY 3 ID\n";
  f<<"\t"<<m_node[3]->GN<<"\t% NodeXY 4 ID\n";

  /** check for errors */
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"MembraneC02D::Write()","Error writing FEM element!");
  }

}

FEM_CLASS_REGISTER(MembraneC02D)




}} // end namespace itk::fem
