/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTriC02D.h
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


#ifndef __itkFEMElementTriC02D_h
#define __itkFEMElementTriC02D_h

#include "itkFEMElementBase.h"
#include "itkFEMNodeXY.h"
#include "itkFEMMaterialStandard.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \class TriC02D
 * \brief 3-noded, C0, element in 2D space
 */
class TriC02D : public Element
{
FEM_CLASS(TriC02D,Element)
public:
  /**
   * 6 DOF constant for faster access within the class
   */
  enum {NDOF=6};              

  /** Access to NDOF from base class */
  int N() const { return NDOF; }

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /**
   * Element force vector 
   */
  vnl_vector<Float> Fe(LoadElementPointer l) const;
  
  /**
   * Pointers to DOF displacements, which are stored
   * in node classes
   */
  Disp* uDOF(int i) const {
    switch ( i ) {
      case 0:
        return &m_node1->uX;
        break;
      case 1:
        return &m_node1->uY;
        break;
      case 2:
        return &m_node2->uX;
        break;
      case 3:
        return &m_node2->uY;
        break;
      case 4:
        return &m_node3->uX;
        break;
      case 5:
        return &m_node3->uY;
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
  TriC02D() : m_node1(0), m_node2(0), m_node3(0), m_mat(0) {}

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
#ifdef _FEM_Build_Visualization_Routines_
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

private:
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
   * Pointers to three node classes that define the
   * element
   */
  NodeXY::ConstPointer m_node1;
  NodeXY::ConstPointer m_node2;
  NodeXY::ConstPointer m_node3;

  /**
   * Pointer to geometric and material properties
   * of the element
   */
  MaterialStandard::ConstPointer m_mat;

}; // class TriC02D

FEM_CLASS_INIT(TriC02D)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementTriC02D_h
