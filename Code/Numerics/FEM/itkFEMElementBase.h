/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementBase_h
#define __itkFEMElementBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include "itkFEMNodeBase.h"
#include "itkFEMMaterialBase.h"
#include "itkVisitorDispatcher.h"
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
 *    - N():     Function that returns the number of degrees of freedom in element.
 *    - Ke():    Function to calculate the element stiffness matrix in global coordinate system.
 *    - Fe():    Function to calculate the element force vector in global coordinate system.
 *    - uDOF():  Provide a pointer to storage of i-th DOF displacement in the element.
 *    - Clone(): Function that creates a duplicate of current element and returns a pointer to it.
 *
 * and optionally (if required):
 *    - Read():  Reads element data from the stream f. assume that the stream position is
 *               already where the element data starts. Take care of the error checking.
 *    - Write(): Writes element data to the stream.
 *    - Draw():  Draws the element on the device context (Windows only).
 *
 * The storage of element parameters (geometry...) can't be implemented here, since we don't know yet, 
 * how much memory each element needs. Instead each derived class should take care of the memory 
 * management (declare appropriate data members) for the element parameters and provide access
 * to these parameters (like nodes, materials...).
 */

/* We need forward declaration of the LoadElement base class. */
class LoadElement;

/**
 * \def LOAD_FUNCTION()
 * \brief Macro that simplifies the the Fe function definitions.
 *
 * NOTE: This macro must be called in declaration of ALL
 *       derived Element classes.
 */
#define LOAD_FUNCTION() \
  virtual LoadVectorType Fe( LoadElementPointer l ) const \
  { return VisitorDispatcher<Self,LoadElement,LoadVectorType>::Visit(this,l); }


class Element : public FEMLightObject
{
FEM_CLASS_SP(Element,FEMLightObject)
public:
  /**
   * Float type used in Node and derived classes
   */
  typedef Node::Float Float;

  /**
   * Class type used in Node and derived classes to specify
   * displacements is redefined here for easier access.
   */
  typedef Node::Displacement Displacement;

  /**
   * Array class that holds special pointers to the Element objects
   */
  typedef FEMPArray<Element> ArrayType;

  /**
   * Class used to store the element stiffness matrix
   */
  typedef vnl_matrix<Float> StiffnesMatrixType;

  /**
   * Class to store the element load vector
   */
  typedef vnl_vector<Float> LoadVectorType;


  /**
   * Return the number of degrees of freedom (DOF) for a derived element class
   */
  virtual int N() const = 0;

  /**
   * Compute and return element stiffnes matrix in global coordinate system
   */
  virtual StiffnesMatrixType Ke() const = 0;

  /**
   * Compute and return element mass matrix in global coordinate system.
   * This is needed if dynamic problems (parabolic or hyperbolix d.e.)
   * need to be solved.
   */
  virtual vnl_matrix<Float> Me() const;

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
   * Compute and return the element load vector for a given external load.
   * The class of load object determines the type of load acting on the
   * elemnent. Basically this is the contribution of this element on the right
   * side of the master matrix equation, due to the specified load. 
   * Returned vector includes only nodal forces that correspond to the given
   * Load object.
   *
   * Visitor design pattern is used in the loads implementation. This function
   * only selects and calls the proper function based on the given class of
   * load object. The code that performs the actual conversion to the
   * corresponding nodal loads is defined elswhere.
   *
   * \note Each deriver class must implement its own version of this function.
   *       This is automated by calling the LOAD_FUNCTION() macro within the
   *       class declaration (in the public: block).
   *
   * For example on how to define specific element load, see funtion
   * LoadImplementationPoint_Bar2D.
   *
   * \note: Before a load can be applied to an element, the function that
   *        implements a load must be registered with the VisitorDispactcher
   *        class.
   *
   * \sa VisitorDispatcher
   */
  virtual LoadVectorType Fe(LoadElementPointer l) const = 0;

  /**
   * Pure virtual function that returns a pointer to an allocated memory that stores displacement
   * of i-th degree of freedom (DOF) of this element.
   *
   * Number of different pointers in a whole system determines global number of DOF. If two
   * pointers in two different elements point to the same location, this means that those two
   * elements share that DOF and are connected together.
   *
   * Normally this function is overriden by defining a simple switch statement that returns
   * pointers to members in nodes object that define the element. If an error occurs
   * (when i is out of range for example), derived function should call
   * implementation in base class (this one).
   */
  virtual Displacement* uDOF(int i) const = 0;

  /**
   * \class ReadInfoType
   * \brief Additional information that is required when reading elements
            from stream.
   *
   * When the element is to be read from the input stream, we must provide
   * pointers to the array of nodes and materials. Construct this class and
   * pass a pointer to it when calling the Element::Read virtual member
   * function.
   */
  class ReadInfoType {
  public:
    Node::ArrayType::Pointer m_node;  /**< Pointer to an array nodes. */
    Material::ArrayType::Pointer m_mat;  /**< Pointer to an array of materials. */
    /** Constructor for simple object creation. */
    ReadInfoType(Node::ArrayType::Pointer node_, Material::ArrayType::Pointer mat_) :
      m_node(node_), m_mat(mat_) {}
  };

#ifdef FEM_BUILD_VISUALIZATION
  /**
   * Draws the element on the DC.
   */
  virtual void Draw(CDC* pDC) const {}
  /** global scale for drawing on the DC */
  static double& DC_Scale;
#endif

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBase_h
