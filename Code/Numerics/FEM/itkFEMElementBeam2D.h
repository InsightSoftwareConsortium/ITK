/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBeam2D.h
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

#ifndef __itkFEMElementBeam2D_h
#define __itkFEMElementBeam2D_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialStandard.h"
#include "itkFEMNodeXYrotZ.h"

namespace itk {
namespace fem {




/**
 * \class Beam2D
 * \brief 1D Beam (spring that also bends) finite element in 2D space.
 * Beam2D finite element is derived from the Element base class
 */
class Beam2D : public Element
{
FEM_CLASS(Beam2D,Element)
public:

  /**
   * 6 DOF. Constant for faster access within the class.
   */
  enum {NDOF=6};

  /**
   * Required virtual functions
   */

  /**
   * Access to NDOF from base class
   */
  int N() const { return NDOF; }

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
      return &m_node1->urotZ;
      break;
    case 3:
      return &m_node2->uX;
      break;
    case 4:
      return &m_node2->uY;
      break;
    case 5:
      return &m_node2->urotZ;
      break;
    }
    return Element::uDOF(i);
  }

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
#ifdef _FEM_Build_Visualization_Routines_
  void Draw(CDC* pDC) const;
#endif

  /**
   * Default constructor only clears the internal storage
   */
  Beam2D() : m_node1(0), m_node2(0), m_mat(0) {}

  /**
   * Construct an element by specifying two nodes and material
   */
  Beam2D(  Node::ConstPointer n1_, 
      Node::ConstPointer n2_, 
      Material::ConstPointer mat_);

public:

  /**
   * Pointers to node objects that defines the element
   */
  NodeXYrotZ::ConstPointer m_node1;
  NodeXYrotZ::ConstPointer m_node2;

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer m_mat;

};

FEM_CLASS_INIT(Beam2D)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBeam2D_h
