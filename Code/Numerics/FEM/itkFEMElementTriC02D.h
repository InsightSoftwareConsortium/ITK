/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTriC02D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef __itkFEMElementTriC02D_h
#define __itkFEMElementTriC02D_h

#include "itkFEMElementStandard.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMNodeXY.h"
#include "itkFEMMaterialStandard.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \class TriC02D
 * \brief 3-noded finite element class in 2D space.
 */
class TriC02D : public ElementStandard<3,2,NodeXY>
{
typedef ElementStandard<3,2,NodeXY> TemplatedParentClass;
FEM_CLASS(TriC02D,TemplatedParentClass)
public:

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /*
   * Macro that defines a specific version of the Fe() function
   */
  LOAD_FUNCTION();

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
  TriC02D() : m_mat(0) {}

  /**
   * Construct an element by specifying 3 nodes and material
   */
  TriC02D(
    Node::ConstPointer n1_, 
    Node::ConstPointer n2_,
    Node::ConstPointer n3_,
    Material::ConstPointer p_);

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
   * Function that computes the derivatives of of the shape
   * functions of this element at a given point.
   */
  vnl_matrix<Float> ComputeShapeFunctionDerivativesAt(Float[]) const;

//private:
  /** 
   * Compute the determinant of the Jacobian Matrix
   * at a given point
   */
  Float JacobianMatrixDeterminant(const vnl_matrix<Float>&) const;

  /**
   * Compute shape function derivatives in global (Cartesian) coordinates
   */
  vnl_matrix<Float>
  ComputeShapeFunctionCartDerivatives(const vnl_matrix<Float>&, Float) const;

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

  /**
    * Computes gravity load contribution
    */
  vnl_vector<Float>
  GravityLoad(const vnl_vector<Float>&) const;

  /**
    * Computes edge load contribution
    */
  vnl_vector<Float>
  EdgeLoad(Float, Float, Float, Float, Float, Float, Float,
       Float, int, int) const;

  /**
    * Gets the indices of the nodes defining an edge
      */
  void GetNode(int, int&, int&) const;

  /**
    * Gets the coordinates of a given node
    */
  void GetNodeCoordinates(int, Float&, Float&) const;

public:


  /**
   * Pointer to geometric and material properties
   * of the element
   */
  MaterialStandard::ConstPointer m_mat;

}; // class TriC02D

FEM_CLASS_INIT(TriC02D)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementTriC02D_h
