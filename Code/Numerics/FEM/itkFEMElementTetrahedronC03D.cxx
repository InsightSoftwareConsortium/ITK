/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTetrahedronC03D.cxx
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

#include "itkFEMElementTetrahedronC03D.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include <math.h>

namespace itk {
namespace fem {




/**
 * Constructor for class TetrahedronC03D
 */
TetrahedronC03D::TetrahedronC03D(  Node::ConstPointer ns_[],
                  Material::ConstPointer p_ )
{
  /**
   * Initialize the pointers to nodes and check that
   * we were given the pointers to the right node class.
   * if the node class was incorrect a bad_cast exception is thrown
   */
  for (int j=0; j < 4; j++) {
    m_nodes[j] = &dynamic_cast<const NodeXYZ&>(*ns_[j]);
  }

  m_mat = &dynamic_cast<const MaterialStandard&>(*p_);
}




/**
 * Returns the stiffness matrix for TetrahedronC03D element
 */
vnl_matrix<TetrahedronC03D::Float> TetrahedronC03D::Ke() const 
{
  vnl_matrix<Float> MatKe(12,12), I(3,3), shapeD(4,3), shapeINVD(4,3),
    J(3,3), D(6,6), B(6,12), DB(6,12);

  int i, j;
  Float detJ;
  
  /**
   * Gaussian integration point - use the one-point integration
   * for tetrahedra, which is exact for linear elements.  Set
   * each shape function equal to 0.25 and evaluate from there.
   */
  Float x[3] = {0.25, 0.25, 0.25};

  /**
   * Material properties matrix.  This should be acommodate the
   * real situation, using E and ni.  However, I have no idea
   * how to formulate a 6x6 matrix for a 3D tetrahedral element!
   */
  D.set_identity();

  /** Initialize stiffness matrix */
  MatKe.fill(0.0);

  /**
   * Compute the Jacobian matrix and its determinant
   * at the k-th integration point
   */
  J = ComputeJacobianMatrixAt(x);
  detJ = JacobianMatrixDeterminant(J);
  
  /**
   * Compute the inverse of the Jacobian matrix
   */
  I = ComputeJacobianInverse(J, detJ);
  
  /**
   * Compute the shape function derivatives at integration point
   */
  shapeD = ComputeShapeFunctionDerivativesAt(x);
  
  /** 
   * Compute the shape function derivatives in Cartesian coordinates
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
  for (i=0; i<12; i++) {
    
    /** For each column of the stiffness matrix */
    for (j=0; j<12; j++) {
      
      /**
     * Computes MatKe(i,j) - implies that W = 1 for the
     * Gaussian quadrature
     */
      Float temp = 0;
      for (int k=0; k<6; k++) {
        temp += B[k][i] * DB[k][j];
      }
      
      MatKe[i][j] += (detJ * temp);
    }
  }
  

  return MatKe;
}




/**
 * Draw the element on device context pDC.
 */
#ifdef _FEM_Build_Visualization_Routines_

void TetrahedronC03D::Draw(CDC* pDC) const 
{
}
#endif




/**
 * Returns a vector with global point p corresponding to local point x
 * in the cell.  x should be a point in 3D space
 */
vnl_vector<TetrahedronC03D::Float>
TetrahedronC03D::ComputePositionAt(Float x[]) const
{
  vnl_vector<Float> p(3);
  
  vnl_vector<Float> shapeF = ComputeShapeFunctionsAt(x); 

  p[0] = p[1] = p[2] = 0.0;

  for (int j=0; j < 4; j++) {
    p[0] += (m_nodes[j]->X * shapeF[j]);
    p[1] += (m_nodes[j]->Y * shapeF[j]);
    p[2] += (m_nodes[j]->Z * shapeF[j]);
  }
  
  return p;
}



/**
 * Returns the Jacobian matrix at point x in 3D space, which is given
 * with respect to the local coordinate system.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeJacobianMatrixAt(Float x[]) const
{
  vnl_matrix<Float> J(3,3);

  /** Get the derivatives of the shape functions at given point x */
  vnl_matrix<Float> shapeD = ComputeShapeFunctionDerivativesAt(x);

  /** Initialize J to all zeros */
  J.fill(0.0);

  /**
   * Computes the elements of the Jacobian matrix for each
   * coordinate of a node w.r.t. global coordinate system
   */
  for (int q=0; q < 3; q++) {
    for (int p=0; p < 4; p++) {
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
vnl_vector<TetrahedronC03D::Float>
TetrahedronC03D::ComputeShapeFunctionsAt(Float x[]) const
{
  /** Linear tetrahedral element has 4 shape functions */
  vnl_vector<Float> shapeF(4);
  
  /**
   * Linear tetrahedral element has local coordinates
   * (0,0,0), (1,0,0), (0,1,0), (0,0,1)
   */
  
  /** given local point x=(r,s,t), where 0 <= r,s,t <= 1 */

  /** N_1 = 1 - r - s - t; */
  shapeF[0] = 1 - x[0] - x[1] - x[2];

  /** N_2 = r */
  shapeF[1] = x[0];

  /** N_3 = s */
  shapeF[2] = x[1];

  /** N_4 = t */
  shapeF[3] = x[2];

  return shapeF;
}



/**
 * Returns a matrix with the value of the derivatives of the shape
 * functions at point x in 3D space, which is given with respect to
 * the local coordinate system.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeShapeFunctionDerivativesAt(Float x[]) const
{
  /** functions at directions r, s, and t. */
  vnl_matrix<Float> shapeD(4,3);

  /** Initialize to zeros and fill in nonzero terms next */
  shapeD.fill(0.0);

  /** d(N_1) / d(r,s,t) = -1 */
  for (int j=0; j < 3; j++)
    shapeD[0][j] = -1;

  /** d(N_2) / dr, d(N_3) / ds, d(N_4) / dt = 1 */
  for (int j=1; j < 4; j++)
    shapeD[j][j-1] = 1;

  return shapeD;
}




/**
 * Returns computes the determinant of the Jacobian Matrix
 * at a given point (r,s,t) with respect to the local
 * coordinate system.
 */
TetrahedronC03D::Float
TetrahedronC03D::JacobianMatrixDeterminant(const vnl_matrix<Float>& J) const
{
  /** Computes the determinant of the 3x3 Jacobian matrix */
        return (J[0][0] * (J[1][1] * J[2][2] - J[1][2] * J[2][1])
    - J[0][1] * (J[1][0] * J[2][2] - J[1][2] * J[2][0]) 
    + J[0][2] * (J[1][0] * J[2][1] - J[1][1] * J[2][0]));
}



/**
 * Returns the inverse transformation matrix from the master element to a
 * quadrilateral element.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeJacobianInverse(const vnl_matrix<Float>& J, Float detJ) const
{
  vnl_matrix<Float> I(3,3);

  I = vnl_matrix_inverse<Float>(J);

  return I;
}



/**
 * Returns a matrix with the cartesian derivatives of the shape functions.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>& I,
        const vnl_matrix<Float>& shapeD) const
{
  vnl_matrix<Float> shapeINVD(4,3);

  shapeINVD = shapeD * I;

  return shapeINVD;
}




/**
 * Returns the strain matrix.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeBMatrix(const vnl_matrix<Float>& shapeINVD) const
{
  vnl_matrix<Float> B(6,12);
  int p;

  /**
   * Initialize the B matrix to all zeros.  Later, only the
   * nonzero terms will be filled in.
   */
  B.fill(0.0);

  for (int i=0; i<12; i++) {  
    p = i / 3;
    
    switch(i % 3) {
      case 0:  /** Columns 1, 4, 7, 10 */
        B[0][i] = shapeINVD[p][0];
        B[3][i] = shapeINVD[p][1];
        B[5][i] = shapeINVD[p][2];
        break;
        
        case 1:  /** Columns 2, 5, 8, 11 */
        B[1][i] = shapeINVD[p][1];
        B[3][i] = shapeINVD[p][0];
        B[4][i] = shapeINVD[p][2];
        break;

      case 2:  /** Columns 3, 6, 9, 12 */
        B[2][i] = shapeINVD[p][2];
        B[4][i] = shapeINVD[p][1];
        B[5][i] = shapeINVD[p][0];
        break;    
    }
  }

  return B;
}


/**
 * Returns the result of multiplying the elastic constant matrix by
 * the strain matrix.
 */
vnl_matrix<TetrahedronC03D::Float>
TetrahedronC03D::ComputeDBMatrix(const vnl_matrix<Float>& D, const vnl_matrix<Float>& B) const
{
  vnl_matrix<Float> DB(6,12);

  DB = D * B;

  return DB;
}




/**
 * Return the force vector for TetrahedronC03D element
 */
vnl_vector<TetrahedronC03D::Float> TetrahedronC03D::Fe(LoadElementPointer l) const {
  if ( LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l) ) {
    return Superclass::Fe(l);
  } else
    /** we can't handle this load, pass it over to the parent class */
    return Superclass::Fe(l);  
}





/**
 * Read the element from input stream
 */
void TetrahedronC03D::Read( std::istream& f, void* info )
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
  if ( !(this->m_mat=dynamic_cast<const MaterialStandard*>( &*mats->Find(n)) ) )
  {
    throw std::runtime_error("Global element properties number not found!");
  }

  /** read and set each of the four expected GNN */
  for (int k=0; k < 4; k++) {
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
void TetrahedronC03D::Write( std::ostream& f, int ofid ) const {

  /** If not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** First call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * Then write the actual data (node, and material numbers).
   * We add some comments in the output file.
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialStandard ID\n";
  for (int j=0; j < 4; j++)
  {
    f<<"\t"<<m_nodes[j]->GN<<"\t% NodeXYZ "<<j+1<<" ID\n";
  }
  
  /** Check for errors */
  if (!f) 
  { 
    throw std::runtime_error("Error writing element!");
  }
}

FEM_CLASS_REGISTER(TetrahedronC03D)




}} // end namespace itk::fem
