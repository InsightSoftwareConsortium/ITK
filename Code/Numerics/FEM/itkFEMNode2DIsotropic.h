/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNode2DIsotropic.h
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
