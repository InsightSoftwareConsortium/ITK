/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBar2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementBar2D_h
#define __itkFEMElementBar2D_h

#include "itkFEMElementBase.h"
#include "itkFEMNodeXY.h"
#include "itkFEMMaterialStandard.h"

namespace itk {
namespace fem {




/**
 * \class Bar2D
 * \brief 1D Bar (spring) finite element in 2D space.
 *
 * This element is defined by two NodeXY object and a MaterialStandard object.
 */
class Bar2D : public Element
{
FEM_CLASS(Bar2D,Element)
public:

  /**
   * 4 DOF. Constant for faster access within the class.
   */
  enum {NDOF=4};
  
  /**
   * Required virtual functions
   */

  /**
   * Access to NDOF from base class
   */
  int N() const { return NDOF; };

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /**
   * Function that handles all external loads applied to the element
   */
  vnl_vector<Float> Fe(LoadElementPointer l) const;

  /**
   * Pointers to DOF displacements, which are stored in node classes.
   */
  Displacement* uDOF(int i) const {
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
  void Write( std::ostream& f, int ofid ) const ;

  /**
   * Default constructor only clears the internal storage
   */
  Bar2D() : m_node1(0), m_node2(0), m_mat(0) {}

  /**
   * Construct an element by specifying two nodes and material
   */
  Bar2D(  Node::ConstPointer n1_, 
      Node::ConstPointer n2_, 
      Material::ConstPointer mat_);


  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC) const;
#endif

public:

  /**
   * Pointers to node objects that defines the element
   */
  NodeXY::ConstPointer m_node1;
  NodeXY::ConstPointer m_node2;

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer m_mat;

};

FEM_CLASS_INIT(Bar2D)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBar2D_h
