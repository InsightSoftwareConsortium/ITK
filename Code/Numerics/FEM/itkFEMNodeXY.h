/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXY.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMNodeXY_h
#define __itkFEMNodeXY_h

#include "itkFEMNodeBase.h"

namespace itk {
namespace fem {




/**
 * \class NodeXY
 * \brief Defines basic 2D node in space
 *
 * This implies that the NodeXY class holds 2 coordinates (X and Y), which are
 * used to describe element geometry and 2 Degrees Of Freedom (DOF)
 * displacements. The later are used to hold the displacements of this node
 * due to some forces.
 */
class NodeXY : public Node 
{
FEM_CLASS(NodeXY,Node)
public:

  /**
   * this node has 2 degrees of freedom
   */
  enum { NDOF=2 };          

  NodeXY() : X(0.0), Y(0.0) {}
  NodeXY(Float X_, Float Y_) : X(X_), Y(Y_) {}

  void Read(  std::istream& f, void* info );
  void Write( std::ostream& f, int ofid ) const;

  /**
   * Windows visualization
   */
  #ifdef FEM_BUILD_VISUALIZATION
    /**
     * draws the node on the DC
     */
    void Draw(CDC* pDC) const;
  #endif

  /**
   * node coordinates
   */
  Float X;
  Float Y;

};

FEM_CLASS_INIT(NodeXY)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMNodeXY_h */
