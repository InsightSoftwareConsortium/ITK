/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNode2DIsotropic.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef __itkFEMNode2DIsotropic_h
#define __itkFEMNode2DIsotropic_h

#include "itkFEMNodeBase.h"

namespace itk {
namespace fem {




/**
 * class that defines the basic 2D node in space. this implies that the Node2DIsotropic class holds 2 coordinates (X and Y)
 * that can be used to describe element geometry and 2 Degrees Of Freedom (DOF) displacements, which can be used
 * to hold the displacement of this node due to some forces.
 */
class Node2DIsotropic : public Node {
FEM_CLASS(Node2DIsotropic,Node)
public:

  enum { NDOF=1 };        // this node has 1 degree of freedom

  Float X,Y;
  mutable Displacement v;    // even if the node is const, we must always be able to change the displacements
                  // v stores disp in dim1, w stores in dim2

  Node2DIsotropic() : X(0.0), Y(0.0) {}
  
  Node2DIsotropic(Float X_, Float Y_): X(X_), Y(Y_){}
  
  void Read(  std::istream& f, void* info );
  void Write( std::ostream& f, int ofid ) const ;

  int N() const { return NDOF; };    // access to NDOF from base class
  Displacement* uDOF(int i) const {  // pointers to DOF displacements
    switch ( i ) {
    case 0:
      return &v;
      break;  
    }
    return 0;            // if DOF is out of range we return NULL pointer
  };

};

FEM_CLASS_INIT(Node2DIsotropic)




}} // end namespace itk::fem

#endif //#ifndef __itkFEMNode2DIsotropic_h
