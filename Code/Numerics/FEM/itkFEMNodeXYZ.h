/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXYZ.h
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
#ifndef __itkFEMNodeXYZ_h
#define __itkFEMNodeXYZ_h

#include "itkFEMNodeBase.h"

namespace itk {
namespace fem {




/**
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
   * access to NDOF from base class
   */
  int N() const 
    { 
    return NDOF; 
    };    

  /**
   * pointers to DOF displacements
   */
  Displacement* uDOF(int i) const 
    {  
    switch ( i ) {
      case 0:
        return &uX;
        break;
      case 1:
        return &uY;
        break;
      case 2:
            return &uZ;
        break;
      }

    /**
     * if DOF is out of range we return NULL pointer
     */
    return 0;            
    };

  /**
   * Node coordinates
   */
  Float X;
  Float Y;
  Float Z;

  /**
   * even if the node is const, we must always be able to change the displacements
   */
  mutable Displacement uX;
  mutable Displacement uY;
  mutable Displacement uZ;

};

FEM_CLASS_INIT(NodeXYZ)




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeXYZ_h */
