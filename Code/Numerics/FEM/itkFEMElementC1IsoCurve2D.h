/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementC1IsoCurve2D.h
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

#ifndef __itkElementC1IsoCurve2D_h
#define __itkElementC1IsoCurve2D_h

#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include "itkFEMNode2DIsotropic.h"
#include "itkFEMMaterialStandard.h"
#include "itkFEMElementBase.h"
#include <math.h>

namespace itk {
namespace fem {




/**
 * \class C1IsoCurve2D
 * \brief An isotropic C1 curve element for 2D.
 * An isotropic C1 curve element for 2D.  To expand to ND, must
 * implement correct node class.  Derivatives are estimated by using
 * neighboring node positions.  interpolation is with cubic hermite
 * polynomials.    
 */
class C1IsoCurve2D : public Element
{
FEM_CLASS(C1IsoCurve2D,Element)
public:  
   
  /**
   * 4 DOF. constant for faster access within the class
   *        3 of which overlap
   */
  enum {NDOF=4};

  /**
   * 2 isotropic dimensions for each DOF
   */
  enum {IsotropicDOF=2};
  
  /**
   * Required virtual functions
   */

  /**
   * Access to NDOF from base class
   */
  int N() const { return NDOF; };
  
  /**
   * Access to IsotropicDOF
   */
  int NI() const { return IsotropicDOF; };

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /**
   * Function that handles all external loads applied to the element
   */
  vnl_vector<Float> Fe(LoadElementPointer L) const;

  /**
   * Pointers to DOF displacements, which are stored in node classes.
   */
  Disp* uDOF(int i) const {
    switch ( i ) {
    case 0:
      return &neg_node1->v;
      break;
    case 1:
      return &cur_node->v;
      break;  
    case 2:
      return &pos_node1->v;
      break;
    case 3:
      return &pos_node2->v;
      break;  
    }
    return 0;
  };

  int current_match_index;

  
  /** interpolation with hermite shape functions */
  vnl_vector<Float> InterpolateWithShapeFunctions(Float s);  


  /**
   * Default constructor only clears the internal storage
   */
  C1IsoCurve2D() : neg_node1(0), cur_node(0), 
      pos_node1(0), pos_node2(0), mat(0) {}

  /**
   * Construct an element by specifying four nodes and material
   */
  C1IsoCurve2D(  Node::ConstPointer nn1_,
          Node::ConstPointer cn_,
          Node::ConstPointer pn1_,
          Node::ConstPointer pn2_,
          Material::ConstPointer mat_);

  /**
   * Read data of this class from input stream
   */
  void Read( std::istream& f, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int ofid ) const ;

protected:
  
  vnl_matrix<Float> ControlVec;

public:
  /** Pointer to node that holds current position */
  Node2DIsotropic::ConstPointer cur_node;

  /** pointer to negative side of pos_node */
  Node2DIsotropic::ConstPointer neg_node1;
  
  /** pointer to negative sides of cur_node */
  Node2DIsotropic::ConstPointer pos_node1;
  Node2DIsotropic::ConstPointer pos_node2;

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer mat;
  
};

FEM_CLASS_INIT(C1IsoCurve2D)




}} //end namespace itk::fem

#endif // #ifndef __itkElementC1IsoCurve2D_h             
