/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBeam2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementBeam2D_h
#define __itkFEMElementBeam2D_h

#include "itkFEMElementStandard.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMMaterialStandard.h"
#include "itkFEMNodeXYrotZ.h"

namespace itk {
namespace fem {




/**
 * \class Beam2D
 * \brief 1D Beam (spring that also bends) finite element in 2D space.
 */
class Beam2D : public ElementStandard<2,3,NodeXYrotZ>
{
typedef ElementStandard<2,3,NodeXYrotZ> TemplatedParentClass;
FEM_CLASS(Beam2D,TemplatedParentClass)
public:

  /**
   * Required virtual functions
   */

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /*
   * Macro that defines a specific version of the Fe() function
   */
  LOAD_FUNCTION();

  /**
   * Read data of this class from input stream
   */
  void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int ofid ) const;

  /**
   * Draws the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif

  /**
   * Default constructor only clears the internal storage
   */
  Beam2D() : m_mat(0) {}

  /**
   * Construct an element by specifying two nodes and material
   */
  Beam2D(  Node::ConstPointer n1_, 
      Node::ConstPointer n2_, 
      Material::ConstPointer mat_);

public:

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer m_mat;

};

FEM_CLASS_INIT(Beam2D)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBeam2D_h
