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
#include "itkFEMSolution.h"
#include "itkFEMPArray.h"
#include <vector>
#include <set>
#include <iostream>


namespace itk {
namespace fem {


/* Forward declaratoin of Element base class */
class Element;

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
FEM_ABSTRACT_CLASS(Node,FEMLightObject)
public:

  /**
   * type that holds coordinates and displacements of nodes
   */
  typedef double Float;

  /**
   * array class that holds special pointers to the nodes
   */
  typedef FEMPArray<Self> ArrayType;


  /* Windows visualization */
  #ifdef FEM_BUILD_VISUALIZATION
    /** Draws the node on the DC */
    virtual void Draw(CDC* pDC, Solution::ConstPointer sol) const {}
    /** Global scale for drawing on the DC */
    static double DC_Scale;
  #endif




//////////////////////////////////////////////////////////////////////////
  /**
   * Obtain DOFs associated with this node from element objects
   * defined in m_elements.
   */
  virtual unsigned int GetDegreeOfFreedom(unsigned int i) const;

  /**
   * List of pointers to elements that use this node. External code is
   * responsible for maintaining the list.
   *
   * FIXME: This should be done in a Mesh class.
   *        Maybe we should also store a point number in the element that
   *        uses this node object. 
   */
  typedef std::set<Element*> SetOfElements;
  mutable SetOfElements m_elements;

};




}} // end namespace itk::fem

#endif /* #ifndef __itkFEMNodeBase_h */
