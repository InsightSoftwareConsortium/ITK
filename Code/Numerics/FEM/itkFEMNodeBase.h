/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeBase.h
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
#ifndef __itkFEMNodeBase_h
#define __itkFEMNodeBase_h

/* Windows visualization */
#ifdef _FEM_Build_Visualization_Routines_
#include <afxwin.h>      
#endif

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include <stdexcept>
#include <iostream>
//#include "itkLightObject.h"

namespace itk {
namespace fem {




/**
 * \class Node
 * \brief Abstract base class for nodes
 *
 * Abstranct base class for nodes. Create specific nodes by deriving this (or any other) base class.
 *
 * A general node can hold an arbitrary number of coordinates and the DOF. members in a node claas that hold
 * coordinates are normally named with X,Y or Z, while rotX, rotY and rotZ specify the rotations about an axis.
 * displacements names are always prefixed with a 'u'. e.g. uX means the displacement of a node in the X axis 
 * direction. displacements values are determined by the Solver routine and can be used when modeling the element.
 *
 * Each element requires a specific node type. By properly deriving node classes, an element can use several different
 * classes of nodes. This becomes important when connecting different types of elements in a system.
 */
class Node : public FEMLightObject
{
FEM_CLASS_SP(Node,FEMLightObject)
public:

   /**
   * type that holds coordinates and displacements of nodes
   */
  typedef double Float;

  /**
   * array class that holds special pointers to the nodes
   */
  typedef FEMPArray<Self> ArrayType;

  /**
   * class that holds DOF displacements
   */
  class Displacement 
  {
  public:

    /**
     * global freedom number of this DOF displacement (position in master stiffness matrix)
     * this value can only be set within the function that enumerates global DOF
     */
    int GFN;      
            
    /**
     * the actual value of a displacement (after the whole system is solved)
     */
    Float value;    

    /**
     * default constructor clears the memory used by class
     */
    Displacement() :  
      value(0.0), GFN(-1) {}
  };
  
  /**
   * return the number of DOF in a derived node class
   */
  virtual int N() const = 0;

  /**
   * pure virtual function that returns a pointer to an allocated memory that stores displacement
   * of i-th degree of freedom of this node.
   * normally this function is overriden by defining a simple switch statement that returns
   * pointers to displacement members in derived node object. this function serves the same purpose
   * as the coresponding one in an element class
   */
  virtual Displacement* uDOF(int i) const = 0;

  /* Windows visualization */
  #ifdef _FEM_Build_Visualization_Routines_
    /* draws the node on the DC */
    virtual void Draw(CDC* pDC) const {}
    /* global scale for drawing on the DC */
    static double DC_Scale;
  #endif

};




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeBase_h */
