/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXYZ.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMNodeXYZ_h
#define __itkFEMNodeXYZ_h

#include "itkFEMNodeBase.h"

namespace itk {
namespace fem {




/**
 * \class NodeXYZ
 * \brief Node in 3D space
 *
 * This class defines the basic node in 3D space.  It holds X, Y, and
 * Z coordinates for element geometry and 3 degree-of-freedom (DOF)
 * displacements to describe the movement of the node in space.
 */
class NodeXYZ : public Node 
{
FEM_CLASS(NodeXYZ,Node)
public:

  /**
   * this node has 3 degrees fo freedom
   */
  enum { NDOF=3 };          

  NodeXYZ() : X(0.0), Y(0.0), Z(0.0) {}
  NodeXYZ(Float X_, Float Y_, Float Z_) : X(X_), Y(Y_), Z(Z_) {}
  
  void Read(  std::istream& f, void* info );
  void Write( std::ostream& f, int ofid ) const;

#ifdef FEM_BUILD_VISUALIZATION
  /**
   * draws the node on the DC
   */
  void Draw(CDC* pDC) const;
#endif

  /**
   * Node coordinates
   */
  Float X;
  Float Y;
  Float Z;

};

FEM_CLASS_INIT(NodeXYZ)




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeXYZ_h */
