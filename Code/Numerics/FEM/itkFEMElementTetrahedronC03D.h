/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTetrahedronC03D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef __itkElementTetrahedronC03D_h
#define __itkElementTetrahedronC03D_h

#include "itkFEMElementBase.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMNodeXYZ.h"
#include "itkFEMMaterialStandard.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vxl/vnl/algo/vnl_matrix_inverse.h"

namespace itk {
namespace fem {




/**
 * \class TetrahedronC03D
 * \brief 4-noded finite element class in 3D space.
 */
class TetrahedronC03D : public Element
{
FEM_CLASS(TetrahedronC03D,Element)
public:
  /**
   * 12 DOF constant for faster access within the class
   */
  enum {NDOF=12};              

  /** 
   * Access to NDOF from base class
   */
  int N() const { return NDOF; }

  /** 
   * Element stiffness matrix 
   */
  vnl_matrix<Float> Ke() const;

  /*
   * Macro that defines a specific version of the Fe() function
   */
  LOAD_FUNCTION();

  /**
   * Pointers to DOF displacements, which are stored
   * in node classes.  Expects i from 0 to 11
   */
  Displacement* uDOF(int i) const {
        switch (i % 3) {
    case 0:
      return &m_nodes[i/3]->uX;
      break;
    case 1:
      return &m_nodes[i/3]->uY;
      break;
    case 2:
      return &m_nodes[i/3]->uZ;
      break;
    default:
      return 0;
      break;
    }
    return Element::uDOF(i);
  }

  /**
   * Read data for this class from input stream
   */
  void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int ofid ) const;


  /**
   * Default constructor only clears the internal storage
   */
  TetrahedronC03D() : m_mat(0) { for (int k=0; k < 4; k++) m_nodes[k] = 0; }

  /**
   * Construct an element by specifying 4 nodes and material
   */
  TetrahedronC03D(
    Node::ConstPointer ns_[], 
    Material::ConstPointer p_ );

  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC) const;
#endif


  /**
   * Function that returns a point in the global coordinate
   * system corresponding to a given point in the master element.
   */
  vnl_vector<Float> ComputePositionAt(Float[]) const;

  /**
   * Function that computes the Jacobian matrix of the
   * transformation from the master element.
   */
  vnl_matrix<Float> ComputeJacobianMatrixAt(Float[]) const;

  /**
   * Function that computes the shape functions defining
   * the geometry of this finite element at a given point.
   */
  vnl_vector<Float> ComputeShapeFunctionsAt(Float[]) const;

  /**
   * Function that computes the derivatives of the shape
   * functions of this element at a given point.
   */
  vnl_matrix<Float> ComputeShapeFunctionDerivativesAt(Float[]) const;

private:
  /**
   * Compute the determinant of the Jacobian Matrix
   * at a given point
   */
  Float JacobianMatrixDeterminant(const vnl_matrix<Float>&) const;

  /**
   * Compute the inverse of the Jacobian
   */
  vnl_matrix<Float>
  ComputeJacobianInverse(const vnl_matrix<Float>&, Float) const;

  /**
   * Computes shape function derivatives in global coordinates
   */
  vnl_matrix<Float>
  ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>&,
      const vnl_matrix<Float>&) const;

  /**
   * Computes the strain matrix B
   */
  vnl_matrix<Float>
  ComputeBMatrix(const vnl_matrix<Float>&) const;

  /** 
   * Computes D (elastic constant matrix) times B (strain matrix)
   */
  vnl_matrix<Float>
  ComputeDBMatrix(const vnl_matrix<Float>&, const vnl_matrix<Float>&) const;


public:
  /**
   * Pointers to 4 node classes that define the
   * element
   */
  NodeXYZ::ConstPointer m_nodes[4];

  /**
   * Pointer to geometric and material properties
   * of the element.
   */
  MaterialStandard::ConstPointer m_mat;

}; // class TetrahedronC03D

FEM_CLASS_INIT(TetrahedronC03D)




}} // end namespace itk::fem

#endif  // #ifndef __itkElementTetrahedronC03D_h
