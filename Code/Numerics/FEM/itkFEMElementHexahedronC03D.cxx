/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementHexahedronC03D.cxx
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

#include "itkFEMElementHexahedronC03D.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include <math.h>

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
  std::cout << "TS: HexahedronC03D"<< std::endl;
  
  for (int j=0; j < 8; j++) {
    m_nodes[j] = &dynamic_cast<const NodeXYZ&>(*ns_[j]);
  }

  m_mat = &dynamic_cast<const MaterialStandard&>(*p_);

}



/**
 * Returns the stiffness matrix for HexahedronC03D element
 */
vnl_matrix<HexahedronC03D::Float> HexahedronC03D::Ke() const 
{
  vnl_matrix<Float> MatKe(24,24), I(3,3), shapeD(8,3), shapeINVD(8,3),
    J(3,3), D(6,6), B(6,24), DB(6,24);

  std::cout << "TS: Ke"<< std::endl;

  Float detJ;
  
  /**
   * Gaussian integration points
   */
  Float pt = 1.0 / sqrt(3.0);
  Float GPoints[][3] = {{-pt, -pt, pt}, {pt, -pt, pt}, {pt, pt, pt}, {-pt, pt, pt}, 
            {-pt, -pt, -pt}, {pt, -pt, -pt}, {pt, pt, -pt}, {-pt, pt, -pt}};

  /**
   * Material properties matrix.  This should be acommodate the
   * real situation, using E and ni.  However, I have no idea
   * how to formulate a 6x6 matrix for a 3D hexahedral element!
   */
  D.set_identity();

  Float x[3];
  int i, j;

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
#ifdef _FEM_Build_Visualization_Routines_
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

  std::cout <<"TS: ComputePositionAt"<<std::endl;

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

  std::cout <<"TS: ComputeJacobianMatrixAt"<<std::endl;

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
  
  std::cout <<"TS: ComputeShapeFunctionsAt"<<std::endl;

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

  std::cout <<"TS: ComputeShapeFunctionDerivativesAt" <<std::endl;

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
  std::cout <<"TS: JacobianMatrixDeterminant" <<std::endl;
  
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

  std::cout << "TS: ComputeJacobianInverse" << std::endl;

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

  std::cout << "TS: ComputeShapeFunctionCartDerivatives" << std::endl;

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

  std::cout << "TS: ComputeBMatrix" << std::endl;

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

  std::cout << "TS: ComputeDBMatrix" << std::endl;

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
 * Return the force vector for HexahedronC03D element.
 */
vnl_vector<HexahedronC03D::Float> HexahedronC03D::Fe(LoadElementPointer l) const {
  std::cout << "TS: Fe" << std::endl;

  /**
   * We can't handle this load, pass it over to the parent class
   * FIXME: write code that handles loads.
   */
  return Superclass::Fe(l);

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

  std::cout << "TS: Reading element" << std::endl;

  /** first call the parent's read function */
  Superclass::Read(f,info);

  /**
   * Read and set the material pointer
   */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  if ( !(this->m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n)) ) )
  {
    throw std::runtime_error("Global element properties number not found!");
  }

  /**
   * Read and set each of the eight expected GNN
   */
  for (int k=0; k < 8; k++) {
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    if ( !(this->m_nodes[k]=dynamic_cast<const NodeXYZ*>( &*nodes->Find(n)) ) )
    {
      throw std::runtime_error("Global node number not found!");
    }
  }


out:

  if( !f ) { throw std::runtime_error("Error reading element!"); }

}




/**
 * Write the element to the output stream.
 */
void HexahedronC03D::Write( std::ostream& f, int ofid ) const {

  std::cout << "TS: Writing element" << std::endl;

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
  if (!f) { throw std::runtime_error("Error writing element!"); }
}

FEM_CLASS_REGISTER(HexahedronC03D)





}} // end namespace itk::fem
