/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementHexahedronC03D.cxx
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

#include "itkFEMElementHexahedronC03D.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {



/**
 * Constructor for class HexahedronC03D
 */
HexahedronC03D::HexahedronC03D(  Node::ConstPointer ns_[],
                Material::ConstPointer p_)
{
  /**
   * Initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * If the node class was incorrect a bad_cast exception is thrown
   */

  try
  {
    for (int j=0; j < 8; j++) {
      m_nodes[j] = &dynamic_cast<const NodeXYZ&>(*ns_[j]);
    }

    m_mat = &dynamic_cast<const MaterialStandard&>(*p_);
  }
  catch ( std::bad_cast )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"HexahedronC03D::HexahedronC03D()");
  }


}



/**
 * Returns the stiffness matrix for HexahedronC03D element
 */
vnl_matrix<HexahedronC03D::Float> HexahedronC03D::Ke() const 
{
  vnl_matrix<Float> MatKe(24,24), I(3,3), shapeD(8,3), shapeINVD(8,3),
    J(3,3), D(6,6), B(6,24), DB(6,24);

  Float detJ;
  Float x[3];
  int i, j;
  
  /**
   * Gaussian integration points
   */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][3] = {{-pt, -pt, pt}, {pt, -pt, pt}, {pt, pt, pt}, {-pt, pt, pt}, 
            {-pt, -pt, -pt}, {pt, -pt, -pt}, {pt, pt, -pt}, {-pt, pt, -pt}};

  /**
   * Material properties matrix.  This should be acommodate the
   * real situation, using E and ni.
   */
  D.fill(0.0); 
  Float fac = m_mat->E / ((1 + m_mat->ni) * (1 - 2 * m_mat->ni));
  
  /** Set the elements in the top left quadrant */
  for (j=0; j < 3; j++) {
    for (int k=0; k < 3; k++) {
      D[j][k] = m_mat->ni;
    }
  }

  /** Set the diagonal elements */
  for (int k=0; k < 3; k++) {
    D[k][k] = 1 - m_mat->ni;
  }
  for (int k=3; k < 6; k++) {
    D[k][k] = 1 - (2 * m_mat->ni) * 0.5;
  }

  /** Multiply by the factor */
  D = D * fac;

  /** Initialize stiffness matrix */
  MatKe.fill(0.0);

  /** For each integration point */
  for (int k=0; k<8; k++) {
    /** Get the k-th integration point */
    x[0] = GPoints[k][0];
    x[1] = GPoints[k][1];
    x[2] = GPoints[k][2];

    /** 
     * Compute the Jacobian matrix and its determinant
     * at the k-th integration point
     */
    J = ComputeJacobianMatrixAt(x);
    detJ = JacobianMatrixDeterminant(J);

    /** Compute the inverse of the Jacobian matrix */
    I = ComputeJacobianInverse(J, detJ);

    /** Compute the shape function derivatives at integration point */
    shapeD = ComputeShapeFunctionDerivativesAt(x);

    /**
     * Computes the shape function derivatives in Cartesian coordinates
     * at integration point.
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
    for (i=0; i<24; i++) {

      /** For each column of the stiffness matrix */
      for (j=0; j<24; j++) {

        /** 
         * Compute MatKe(i,j) - implies that
         * all Wi = 1 for the Gaussian integration
         */
        Float temp = 0;
        for (int k=0; k<6; k++) {
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
void HexahedronC03D::Draw(CDC* pDC) const 
{
}
#endif


/**
 * Returns a vector with global point p corresponding to local point x
 * in the cell.  x should be a point in 3D space
 */
vnl_vector<HexahedronC03D::Float>
HexahedronC03D::ComputePositionAt(Float x[]) const
{
  vnl_vector<Float> p(3);

  vnl_vector<Float> shapeF = ComputeShapeFunctionsAt(x); 

  p[0] = p[1] = p[2] = 0.0;

  for (int j=0; j < 8; j++) {
    p[0] += (m_nodes[j]->X * shapeF[j]);
    p[1] += (m_nodes[j]->Y * shapeF[j]);
    p[2] += (m_nodes[j]->Z * shapeF[j]);
  }
  
  return p;
}



/**
 * Return the Jacobian matrix at point x in 3D space, which is given
 * with respect to the local coordinate system.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeJacobianMatrixAt(Float x[]) const
{
  vnl_matrix<Float> J(3,3);

  /** Get the derivatives of the shape functions at given point x */
  vnl_matrix<Float> shapeD = ComputeShapeFunctionDerivativesAt(x);

  /** Initialize J to all zeros */
  J.fill(0.0);

  /** 
   * Compute the elements of the Jacobian matrix for each
   * coordinate of a node w.r.t. global coordinate system
   */
  for (int q=0; q < 3; q++) {
    for (int p=0; p < 8; p++) {
      J[0][q] += (shapeD[p][q] * m_nodes[p]->X);
      J[1][q] += (shapeD[p][q] * m_nodes[p]->Y);
      J[2][q] += (shapeD[p][q] * m_nodes[p]->Z);
    }
  }

  return J;
}



/**
 * Returns a vector with the value of the shape functions at point x
 * in 3D space, which is given with respect to the local coordinate
 * system.
 */
vnl_vector<HexahedronC03D::Float>
HexahedronC03D::ComputeShapeFunctionsAt(Float x[]) const
{
  /** Linear hexahedral element has 8 shape functions */
  vnl_vector<Float> shapeF(8);
  
  /** 
   * Linear hexahedral element has local coordinates
   *  (-1,-1,-1), (1,-1,-1), (1,1,-1), (-1,1,-1), (-1,-1,1), (1,-1,1), (1,1,1), (-1,1,1)
   */
  
  /**  given local point x=(r,s,t), where -1 <= r,s,t <= 1 and */

  /** N_1 = ((1-r) * (1-s) * (1-t)) / 8 */
  shapeF[0] = (1 - x[0]) * (1 - x[1]) * (1 - x[2]) * 0.125;

  /** N_2 = ((1+r) * (1-s) * (1-t)) / 8 */
  shapeF[1] = (1 + x[0]) * (1 - x[1]) * (1 - x[2]) * 0.125;

  /** N_3 = ((1+r) * (1+s) * (1-t)) / 8 */
  shapeF[2] = (1 + x[0]) * (1 + x[1]) * (1 - x[2]) * 0.125;

  /** N_4 = ((1-r) * (1+s) * (1-t)) / 8 */
  shapeF[3] = (1 - x[0]) * (1 + x[1]) * (1 - x[2]) * 0.125;

  /** N_5 = ((1-r) * (1-s) * (1+t)) / 8 */
  shapeF[4] = (1 - x[0]) * (1 - x[1]) * (1 + x[2]) * 0.125;

  /** N_6 = ((1+r) * (1-s) * (1+t)) / 8 */
  shapeF[5] = (1 + x[0]) * (1 - x[1]) * (1 + x[2]) * 0.125;

  /** N_7 = ((1+r) * (1+s) * (1+t)) / 8 */
  shapeF[6] = (1 + x[0]) * (1 + x[1]) * (1 + x[2]) * 0.125;

  /** N_8 = ((1-r) * (1+s) * (1+t)) / 8 */
  shapeF[7] = (1 - x[0]) * (1 + x[1]) * (1 + x[2]) * 0.125;

  return shapeF;
}



/**
 * Returns a matrix with the value of the derivatives of the shape
 * functions at point x in 3D space, which is given with respect to
 * the local coordinate system.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeShapeFunctionDerivativesAt(Float x[]) const
{
  /** functions at directions r, s, and t. */
  vnl_matrix<Float> shapeD(8,3);

  // d(N_1) / d(r)
  shapeD[0][0] = (-1) * (1 - x[1]) * (1 - x[2]) * 0.125;
  
  // d(N_1) / d(s)
  shapeD[0][1] = (-1) * (1 - x[0]) * (1 - x[2]) * 0.125;

  // d(N_1) / d(t)
  shapeD[0][2] = (-1) * (1 - x[0]) * (1 - x[1]) * 0.125;

  // d(N_2) / d(r)
  shapeD[1][0] = (+1) * (1 - x[1]) * (1 - x[2]) * 0.125;
  
  // d(N_2) / d(s)
  shapeD[1][1] = (-1) * (1 + x[0]) * (1 - x[2]) * 0.125;

  // d(N_2) / d(t)
  shapeD[1][2] = (-1) * (1 + x[0]) * (1 - x[1]) * 0.125;

  // d(N_3) / d(r)
  shapeD[2][0] = (+1) * (1 + x[1]) * (1 - x[2]) * 0.125;
  
  // d(N_3) / d(s)
  shapeD[2][1] = (+1) * (1 + x[0]) * (1 - x[2]) * 0.125;

  // d(N_3) / d(t)
  shapeD[2][2] = (-1) * (1 + x[0]) * (1 + x[1]) * 0.125;

  // d(N_4) / d(r)
  shapeD[3][0] = (-1) * (1 + x[1]) * (1 - x[2]) * 0.125;
       
  // d(N_4) / d(s)
  shapeD[3][1] = (+1) * (1 - x[0]) * (1 - x[2]) * 0.125;

  // d(N_4) / d(t)
  shapeD[3][2] = (-1) * (1 - x[0]) * (1 + x[1]) * 0.125;

  // d(N_5) / d(r)
  shapeD[4][0] = (-1) * (1 - x[1]) * (1 + x[2]) * 0.125;
  
  // d(N_5) / d(s)
  shapeD[4][1] = (-1) * (1 - x[0]) * (1 + x[2]) * 0.125;

  // d(N_5) / d(t)
  shapeD[4][2] = (+1) * (1 - x[0]) * (1 - x[1]) * 0.125;

  // d(N_6) / d(r)
  shapeD[5][0] = (+1) * (1 - x[1]) * (1 + x[2]) * 0.125;
  
  // d(N_6) / d(s)
  shapeD[5][1] = (-1) * (1 + x[0]) * (1 + x[2]) * 0.125;

  // d(N_6) / d(t)
  shapeD[5][2] = (+1) * (1 + x[0]) * (1 - x[1]) * 0.125;

  // d(N_7) / d(r)
  shapeD[6][0] = (+1) * (1 + x[1]) * (1 + x[2]) * 0.125;
  
  // d(N_7) / d(s)
  shapeD[6][1] = (+1) * (1 + x[0]) * (1 + x[2]) * 0.125;

  // d(N_7) / d(t)
  shapeD[6][2] = (+1) * (1 + x[0]) * (1 + x[1]) * 0.125;

  // d(N_8) / d(r)
  shapeD[7][0] = (-1) * (1 + x[1]) * (1 + x[2]) * 0.125;
  
  // d(N_8) / d(s)
  shapeD[7][1] = (+1) * (1 - x[0]) * (1 + x[2]) * 0.125;

  // d(N_8) / d(t)
  shapeD[7][2] = (+1) * (1 - x[0]) * (1 + x[1]) * 0.125;

  return shapeD;
}



/**
 * Returns computes the determinant of the Jacobian Matrix
 * at a given point (r,s,t) with respect to the local
 * coordinate system.
 */
HexahedronC03D::Float
HexahedronC03D::JacobianMatrixDeterminant(const vnl_matrix<Float>& J) const
{
  /** Computes the determinant of the 3x3 Jacobian matrix */
        return (J[0][0] * (J[1][1] * J[2][2] - J[1][2] * J[2][1])
    - J[0][1] * (J[1][0] * J[2][2] - J[1][2] * J[2][0]) 
    + J[0][2] * (J[1][0] * J[2][1] - J[1][1] * J[2][0]));
}




/**
 * Return the inverse transformation matrix from the master element to a
 * quadrilateral element.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeJacobianInverse(const vnl_matrix<Float>& J, Float detJ) const
{
  vnl_matrix<Float> I(3,3);

  I = vnl_matrix_inverse<Float>(J);

  return I;
}



/**
 * Returns a matrix with the cartesian derivatives of the shape functions.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>& I,
        const vnl_matrix<Float>& shapeD) const
{
  vnl_matrix<Float> shapeINVD(8,3);

  shapeINVD = shapeD * I;

  /*
  // This code may not be perfect....
  for (int i=0; i<8; i++) {
    shapeINVD[i][0] = (shapeD[i][0] * I[0][0]) + (shapeD[i][1] * I[1][0]) + (shapeD[i][2] * I[2][0]);
    shapeINVD[i][1] = (shapeD[i][0] * I[0][1]) + (shapeD[i][1] * I[1][1]) + (shapeD[i][2] * I[2][1]);
    shapeINVD[i][2] = (shapeD[i][0] * I[0][2]) + (shapeD[i][1] * I[1][2]) + (shapeD[i][2] * I[2][2]);
  }
  */

  return shapeINVD;
}



/**
 * Returns the strain matrix.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeBMatrix(const vnl_matrix<Float>& shapeINVD) const
{
        vnl_matrix<Float> B(6,24);
  int p;

  /**
   * Initialize the B matrix to all zeros.  Later, only the
   * nonzero terms will be filled in.
   */
  B.fill(0.0);

  for (int i=0; i<24; i++) {  
    p = i / 3;
    
    switch(i % 3) {
      case 0:  /** Columns 1, 4, 7, ..., 22 */
        B[0][i] = shapeINVD[p][0];
        B[3][i] = shapeINVD[p][1];
        B[5][i] = shapeINVD[p][2];
        break;
        
        case 1:  /** Columns 2, 5, 8, ..., 23 */
        B[1][i] = shapeINVD[p][1];
        B[3][i] = shapeINVD[p][0];
        B[4][i] = shapeINVD[p][2];
        break;

      case 2:  /** Columns 3, 6, 9, ..., 24 */
        B[2][i] = shapeINVD[p][2];
        B[4][i] = shapeINVD[p][1];
        B[5][i] = shapeINVD[p][0];
        break;    
    }
  }

  return B;
}




/**
 * Return the result of multiplying the elastic constant matrix by
 * the strain matrix.
 */
vnl_matrix<HexahedronC03D::Float>
HexahedronC03D::ComputeDBMatrix(const vnl_matrix<Float>& D, const vnl_matrix<Float>& B) const
{
  vnl_matrix<Float> DB(9,24);

  DB = D * B;

  /*
  // Double check this code - it may not be right!
  for (int i=0; i<9; i++) {
    for (int j=0; j<24; j++) {
      DB[i][j] = 0;
      for (int k=0; k<9; k++) {
        DB[i][j] += D[i][k] * B[k][j];
      }
    }
  }
  */

  return DB;
}




/**
 * Read the element from input stream
 */
void HexahedronC03D::Read(std::istream& f, void* info)
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  Node::ArrayType::Pointer nodes=static_cast<ReadInfoType*>(info)->m_node;
  Material::ArrayType::Pointer mats=static_cast<ReadInfoType*>(info)->m_mat;

  /** first call the parent's read function */
  Superclass::Read(f,info);

  /**
   * Read and set the material pointer
   */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  this->m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n));

  try
  {
  /**
   * Read and set each of the eight expected GNN
   */
    for (int k=0; k < 8; k++)
    {
      SkipWhiteSpace(f); f>>n; if(!f) goto out;
      this->m_nodes[k]=dynamic_cast<const NodeXYZ*>( &*nodes->Find(n));
    }
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"HexahedronC03D::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"HexahedronC03D::Read()","Error reading FEM element!");
  }

}




/**
 * Write the element to the output stream.
 */
void HexahedronC03D::Write( std::ostream& f, int ofid ) const {

  /** If not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** First call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * Then write the actual data (node, and material numbers).
   * We add some comments in the output file.
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  
  for (int j=0; j < 8; j++)
  {
    f<<"\t"<<m_nodes[j]->GN<<"\t% NodeXYZ "<<j+1<<" ID\n";
  }
  
  /** check for errors */
  if (!f) {
    throw FEMExceptionIO(__FILE__,__LINE__,"HexahedronC03D::Write()","Error writing FEM element!");
  }

}

FEM_CLASS_REGISTER(HexahedronC03D)





}} // end namespace itk::fem
