/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXYrotZ.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMNodeXYrotZ_h
#define __itkFEMNodeXYrotZ_h

#include "itkFEMNodeXY.h"

namespace itk {
namespace fem {




/**
 * \class NodeXYrotZ
 * \brief Node in 2D space with 2 displacements and 1 rotation.
 *
 * This node class is derived from NodeXY class. The additional component
 * NodeXYrotZ#urotZ holds the displacement of rotation around the Z axis.
 *
 * Note that any element that requires nodes of class NodeXY, can also
 * use this node class. This way you can for example connect the Bar2D
 * and Beam2D elements. Bar2D requires NodeXY, while Beam2D adds a
 * rotation displacement and therefore requires NodeXYrotZ.
 */
class NodeXYrotZ : public NodeXY 
{
FEM_CLASS(NodeXYrotZ,Node)
public:

  /**
   * This node has 3 degrees fo freedom
   */
  enum { NDOF=3 };          

  /**
   * Write the NodeXYrotZ to output stream
   */
  void Write( std::ostream& f, int ofid ) const;

  /**
   * Windows visualizatoion
   */
  #ifdef FEM_BUILD_VISUALIZATION
    void Draw(CDC* pDC, Solution::ConstPointer sol) const;
  #endif

  NodeXYrotZ() : NodeXY() {}
  NodeXYrotZ(Float X_, Float Y_) : NodeXY(X_,Y_) {}

};

FEM_CLASS_INIT(NodeXYrotZ)




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeXYrotZ_h */
