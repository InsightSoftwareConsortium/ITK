/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBar2D.h
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

#ifndef __itkFEMElementBar2D_h
#define __itkFEMElementBar2D_h

#include "itkFEMElementBase.h"
#include "itkFEMNodeXY.h"
#include "itkFEMMaterialStandard.h"

namespace itk {
namespace fem {




/**
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
