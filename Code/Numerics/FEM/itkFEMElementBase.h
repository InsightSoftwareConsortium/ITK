/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBase.h
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

#ifndef __itkFEMElementBase_h
#define __itkFEMElementBase_h

#ifdef _FEM_Build_Visualization_Routines_
#include <afxwin.h>        /** required to draw the element on device context */
#endif

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include "itkFEMNodeBase.h"
#include "itkFEMMaterialBase.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include <iostream>

namespace itk {
namespace fem {




/**
 * \class Element
 * \brief Abstract base element class.
 *
 * Derive this class to create new finite element classes.
 * All derived classes must define:
 *
 *    N():    function that returns the number of degrees of freedom in element
 *    Ke():    function to calculate the element stiffness matrix in global coordinate system
 *    Fe():    function to calculate the element force vector in global coordinate system
 *    uDOF(i):  provides a pointer to storage of i-th DOF displacement in the element
 *    Clone():  function that creates a duplicate of current element and returns a pointer to it
 *
 *  optionally (if required):
 *    Read(f):  reads element data from the stream f. assume that the stream position is
 *          already where the element data starts. take care of the error checking
 *    Write(f):  writes element data to the stream.
 *    Draw(pDC):  draws the elemenet on the provided device context (Windows only)
 *
 * The storage of element parameters (geometry...) can't be implemented here, since we don't know yet, 
 * how much memory each element needs. Instead each derived class should take care of the memory 
 * management (declare appropriate data members) for the element parameters and provide access
 * to these parameters (like nodes, materials...).
 */

/** We need forward declaration of the LoadElement base class. */
class LoadElement;

class Element : public FEMLightObject
{
FEM_CLASS_SP(Element)
public:
  /**
   * Float type used in Node and derived classes
   */
  typedef Node::Float Float;

  /**
   * Class type used in Node and derived classes to specify displacements
   */
  typedef Node::Displacement Disp;

  /**
   * Array class that holds special pointers to the Element objects
   */
  typedef FEMPArray<Element> ArrayType;

  /**
   * Return the number of degrees of freedom (DOF) for a derived element class
   */
  virtual int N() const = 0;

  /**
   * Compute and return element stiffnes matrix in global coordinate system
   */
  virtual vnl_matrix<Float> Ke() const = 0;

  /**
   * Easy access of LoadElement pointer type. When using SmartPointers,
   * this is a pointer to FEMLightObject to avoid cyclic references between
   * LoadElement and Element classes.
   * As a consequence whenever you need to use a pointer to LoadElement class
   * within the element's declaration or definition, ALWAYS use this typedef
   * instead.
   * When calling the Fe(...) function from outside, you should ALWAYS first
   * convert the argument to Element::LoadElementPointer. See code of function
   * Solver::AssembleF(...) for more info.
   */
#ifndef FEM_USE_SMART_POINTERS
  typedef LoadElement* LoadElementPointer;
#else
  typedef SmartPointer<FEMLightObject> LoadElementPointer;
#endif


  /** 
   * Compute and return the element force vector. Basically this is the contribution
   * of this element on the right side of the master matrix equation. Returned vector
   * should include only nodal forces that correspond to the given Load object.
   * Within the function definition you should use the dynamic_cast operator on the 
   * provided pointer to the Load object to obtain the pointer to the derived class
   * and act accordingly. If the implementation of the Fe function does not handle
   * given Load class (dynamic_cast returns 0), parent's Fe member should be called.
   * Several types of loads can be implemented using several if statements.
   *
   * Example:
   *
   *  class Quad2D : public Element {
   *    ...
   *    vnl_vector<Float> Fe(LoadElement* l) {
   *      if (LoadGravity l0=dynamic_cast<LoadGravity*>(l)) {
   *        ... implement the gravity load using l0 pointer
   *      } else
   *        return Element::Fe(l);
   *    }
   *    ...
   *  };
   */
  virtual vnl_vector<Float> Fe(LoadElementPointer l) const {
    /**
     * If this function is not defined in the derived class we return a vector
     * containing all zeros. it has the dimension of N(). This quietly accepts
     * all load classes, but they have no affect on the element.
     */
    return vnl_vector<Float>( N(), 0.0 );
  };

  /**
   * Pure virtual function that returns a pointer to an allocated memory that stores displacement
   * of i-th degree of freedom (DOF) of this element.
   * Number of different pointers in a whole system determines global number of DOF. if two
   * pointers in two different elements point to the same location, this means that those two
   * elements share that DOF and are connected together.
   *
   * Normally this function is overriden by defining a simple switch statement that returns
   * pointers to members in nodes object that define the element.
   */
  virtual Disp* uDOF(int i) const = 0;

  /**
   * When the element is to be read from the input stream, we must provide
   * pointers to the array of nodes and materials. Construct this class and
   * pass a pointer to it when calling the Read member function for elements.
   */
  class ReadInfoType {
  public:
    Node::ArrayType::Pointer m_node;  /** We need pointer to an array nodes */
    Material::ArrayType::Pointer m_mat;  /** We also need pointer to array of materials */
    ReadInfoType(Node::ArrayType::Pointer node_, Material::ArrayType::Pointer mat_) :
      m_node(node_), m_mat(mat_) {}
  };

#ifdef _FEM_Build_Visualization_Routines_
  /**
   * Draws the element on the DC
   */
  virtual void Draw(CDC* pDC) const {}
  /** global scale for drawing on the DC */
  static double& DC_Scale;
#endif

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBase_h
