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

namespace itk {
namespace fem {




/**
 * \class Element
 * \brief Abstract base element class.
 *
 * Derive this class to create new finite element classes.
 * All derived classes must define:
 *
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


/**
 * \def LOAD_FUNCTION()
 * \brief Macro that simplifies the the Fe function definitions.
 *
 * NOTE: This macro must be called in declaration of ALL
 *       derived Element classes.
 */
#define LOAD_FUNCTION() \
  virtual LoadVectorType Fe( LoadElementPointer l ) const \
  { return VisitorDispatcher<Self,LoadElementType,LoadVectorType>::Visit(this,l); }


class Element : public FEMLightObject
{
FEM_CLASS_SP(Element,FEMLightObject)
public:
  /**
   * Float type used in Node and derived classes
   */
  typedef Node::Float Float;

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
   * Easy access of LoadElement and LoadElement::Pointer type.
   * This is a pointer to FEMLightObject to avoid cyclic references between
   * LoadElement and Element classes.
   * As a consequence whenever you need to use a pointer to LoadElement class
   * within the element's declaration or definition, ALWAYS use this typedef
   * instead.
   * When calling the Fe(...) function from outside, you should ALWAYS first
   * convert the argument to Element::LoadElementPointer. See code of function
   * Solver::AssembleF(...) for more info.
   */
  typedef FEMLightObject LoadElementType;
  typedef LoadElementType::Pointer LoadElementPointer;

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
  virtual void Draw(CDC* pDC, Solution::ConstPointer sol) const {}
  /** global scale for drawing on the DC */
  static double& DC_Scale;
#endif




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods that define geometry of an element
   * FIXME: These should be implemented in Cell/Mesh
   */
  virtual unsigned int GetNumberOfPoints(void) const = 0;
  virtual Node::ConstPointer GetPoint(unsigned int pt) const = 0;


  
//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and typedefs related to Node management 
   */

  /**
   * Type that stores global ID's of degrees of freedom. Default constructor
   * must initialize this object to a value, which represents an invalid
   * (not defined) degree of freedom. Sort of like a null pointer.
   *
   * Derived classes must provide static storage for an array of objects of
   * class DegreeOfFreedomIDType.
   *
   * FIXME: This can be something else than an int. Maybe a pointer to
   *        a DOF object, or something...
   */
  typedef unsigned int DegreeOfFreedomIDType;
  enum{ InvalidDegreeOfFreedomID = 0xffffffff };

  /**
   * Return the total number of degrees of freedom defined in a derived
   * element class. By default this is equal to number of points in a cell
   * multiplied by number of degrees of freedom at each point. If a derived
   * class has more advanced DOFs (for example DOFs associated with edges)
   * this function must be overriden in derived class.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedom( void ) const
  { return this->GetNumberOfPoints() * this->GetNumberOfDegreesOfFreedomPerPoint(); }

  /**
   * Return number of DOFs present at each point within an Element. This is
   * basically the number of unknowns that we want to solve for at each point
   * within an element. It's related to the dimensionality of a problem.
   *
   * /note This function must be overriden in all derived classes.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerPoint(void) const = 0;

  /**
   * Method to get DOF ids. Returns the global id of the DOF with given
   * local id. If id is out of range it returns invalid DOF id.
   *
   * \param local_dof Number of DOF within an element (local id of DOF).
   *
   * /note This function must be overriden in all derived classes.
   */
  virtual DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int local_dof ) const = 0;

  /**
   * Method to set DOF ids. Sets local DOF with given id to the global id,
   * which is provided in dof parameter.
   *
   * \param local_dof Number of DOF within an element (local id of DOF).
   * \param global_dof Global DOF id.
   *
   * /note This function must be overriden in all derived classes.
   */
  virtual void SetDegreeOfFreedom( unsigned int local_dof, DegreeOfFreedomIDType global_dof) = 0;

  /**
   * Releases all DOFs used by element. It sets all local DOF
   * ids to invalid global id values (-1).
   */
  virtual void ClearDegreesOfFreedom(void);

  /**
   * Returns a global DOF id that is associated with given point in an element.
   * Since there are usually many DOFs present at each point, you can
   * specify which one, by providing the dof parameter.
   *
   * Function returnes InvalidDegreeOfFreedomID, if any of the parameters is
   * out of range.
   *
   * By default, local DOF ids in all derived cells are numbered starting from
   * 0 for first DOF at first point, then 1 for second DOF at first point,
   * and so on. If more complex DOFs exist in a cell, this function should
   * be overriden.
   *
   * \param pt Local index of a point within a cell (0 - number_of_points-1).
   * \param dof_at_pt Number of DOF present at point pt (0 - number_of_dofs_at_point-1).
   */
  virtual DegreeOfFreedomIDType GetDegreeOfFreedomAtPoint( unsigned int pt, unsigned int dof_at_pt ) const
  {
    if (dof_at_pt>=GetNumberOfDegreesOfFreedomPerPoint() || pt>=GetNumberOfPoints())
    {
      return InvalidDegreeOfFreedomID;
    }
    return GetDegreeOfFreedom(pt*GetNumberOfDegreesOfFreedomPerPoint()+dof_at_pt);
  }

  /**
   * Interpolate the known solution at a given point in local co-ordinates.
   * Returns zero, if either pt or dof_at_pt parameter is out of range.
   *
   * \param vnl_vector<Float> LocalCoords  - vector containing the local coordinates.
   * \param unsigned int which_dof  - which of the degrees of freedom to interpolate.
   */
  
  virtual Float InterpolateSolutionAtLocalCoordinate(vnl_vector<Float> LocalCoords, unsigned int which_dof ) const
  {
    return 0.0;
  }

  /**
   * Sets a global DOF id that is associated with given point in an element.
   *
   * Function does nothing, if either pt or dof_at_pt parameter is out of range.
   *
   * \sa GetDegreeOfFreedomAtPoint
   */
  virtual void SetDegreeOfFreedomAtPoint( unsigned int pt, unsigned int dof_at_pt, DegreeOfFreedomIDType global_dof )
  {
    if (dof_at_pt>=GetNumberOfDegreesOfFreedomPerPoint() || pt>=GetNumberOfPoints())
    {
      return;
    }
    SetDegreeOfFreedom(pt*GetNumberOfDegreesOfFreedomPerPoint()+dof_at_pt, global_dof);
  }

  /**
   * Links the DOFs in a current element with the elements
   * in the neighborhood. The basic implementation of this
   * function is provided here with the base class. If the
   * derived class requires some special DOF linking, this
   * function can be redeclared.
   */
  virtual void LinkDegreesOfFreedom(void);

  /**
   * Create a new unique DOF id.
   * FIXME: This code is not multithread safe.
   */
  static DegreeOfFreedomIDType CreateNewGlobalDOF(void)
  {
    return ++m_DOFCounter;
  };

  static void ResetGlobalDOFCounter(void)
  {
    m_DOFCounter=InvalidDegreeOfFreedomID;
  };

  static DegreeOfFreedomIDType GetGlobalDOFCounter(void)
  {
    return m_DOFCounter;
  }

private:
  static DegreeOfFreedomIDType m_DOFCounter;


};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBase_h
