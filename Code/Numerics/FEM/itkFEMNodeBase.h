/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMNodeBase_h
#define __itkFEMNodeBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
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
   * \class Displacement
   * \brief Class that holds displacements of degrees of freedom (DOF).
   */
  class Displacement 
  {
  public:

    /**
     * Global freedom number of this DOF displacement (position in master stiffness matrix)
     * this value can only be set within the function that enumerates global DOF (Solver class).
     */
    int GFN;      
            
    /**
     * The actual value of a displacement (after the whole system is solved).
     */
    Float value;

    /**
     * Default constructor clears the memory used by Displacement class.
     */
    Displacement() :  
      GFN(-1), value(0.0) {}
  };
  
  /**
   * Return the number of DOF in a derived node class.
   */
  virtual int N() const = 0;

  /**
   * Pure virtual function that returns a pointer to an allocated memory that stores displacement
   * of i-th degree of freedom of this node.
   * 
   * Normally this function is overriden by defining a simple switch statement that returns
   * pointers to displacement members in derived node object. This function serves the same purpose
   * as the coresponding one in an element class.
   *
   * \sa Element::uDOF
   */
  virtual Displacement* uDOF(int i) const = 0;

  /* Windows visualization */
  #ifdef FEM_BUILD_VISUALIZATION
    /** Draws the node on the DC */
    virtual void Draw(CDC* pDC) const {}
    /** Global scale for drawing on the DC */
    static double DC_Scale;
  #endif

};




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeBase_h */
