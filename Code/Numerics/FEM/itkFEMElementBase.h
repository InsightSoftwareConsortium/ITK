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
#include <set>

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
   * Interpolate the known solution at a given point in local co-ordinates.
   * Returns zero, if either pt or dof_at_pt parameter is out of range.
   *
   * \param LocalCoords  - vector containing the local coordinates.
   * \param which_dof  - which of the degrees of freedom to interpolate.
   */
  virtual Float InterpolateSolutionAtLocalCoordinate(vnl_vector<Float> LocalCoords, unsigned int which_dof ) const
  {
    return 0.0;
  }



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to IO and drawing
   */

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
  typedef Node::ConstPointer PointIDType;
  virtual unsigned int GetNumberOfPoints(void) const = 0;
  virtual PointIDType GetPoint(unsigned int pt) const = 0;
  virtual void SetPoint(unsigned int pt, PointIDType node) = 0;



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and typedefs related to node management 
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
   * Type used to store an array of global point IDs that define a node.
   *
   * \sa GetNodeDefinition
   */
  typedef std::multiset<PointIDType> NodeDefinitionType;



  /**
   * Return the total number of degrees of freedom defined in a derived
   * element class. By default this is equal to number of points in a cell
   * multiplied by number of degrees of freedom at each point. If a derived
   * class has more advanced DOFs (for example DOFs associated with edges)
   * this function must be overriden in derived class.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedom( void ) const
  { return this->GetNumberOfPoints() * this->GetNumberOfDegreesOfFreedomPerNode(); }

  /**
   * Method to get DOF ids. Returns the global id of the DOF with given
   * local id. If id is out of range the function returns invalid DOF id.
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
   * Return the total number of nodes in an elememnt. A node is a point in
   * the element that stores one or more degrees of freedom.
   *
   * In linear elements nodes are typically colocated with geometrical points,
   * so the number of nodes is equal to the number of points. This is also
   * the default implementation here. If you need to define a more complex
   * element which has additional nodes, you need to override this function
   * in a derived class.
   */
  virtual unsigned int GetNumberOfNodes( void ) const
  {
    return this->GetNumberOfPoints();
  }

  /**
   * Return the number of degrees of freedom at each node. This is also
   * equal to number of unknowns that we want to solve for at each point
   * within an element.
   *
   * /note This function must be overriden in all derived classes.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const = 0;

  /**
   * Returns a global DOF id that is associated with given node in an element.
   * Since there are in general many DOFs present at each node, you can
   * specify which one, by providing the dof parameter.
   *
   * Function returns InvalidDegreeOfFreedomID, if any of the parameters is
   * out of range. This functionality must be preserved when overriding the
   * function.
   *
   * Typically, local DOF ids in all elements are numbered
   * starting from 0 for first DOF at first node, then 1 for second DOF at
   * first node, and so on. If more complex DOFs exist in a cell, this
   * function should be overriden.
   *
   * \param nd Local index of a node within an element (0 - number_of_nodes-1).
   * \param dof_at_nd Number of DOF present at point pt (0 - number_of_dofs_at_node-1).
   */
  virtual DegreeOfFreedomIDType GetDegreeOfFreedomAtNode( unsigned int nd, unsigned int dof_at_nd ) const
  {
    if (dof_at_nd>=this->GetNumberOfDegreesOfFreedomPerNode() || nd>=this->GetNumberOfNodes())
    {
      return InvalidDegreeOfFreedomID;
    }
    return this->GetDegreeOfFreedom(nd*this->GetNumberOfDegreesOfFreedomPerNode()+dof_at_nd);
  }

  /**
   * Sets a global DOF id that is associated with given node in an element.
   *
   * If either nd or dof_at_nd parameter is out of range, this function
   * does nothing. This functionality must be preserved when overriding the
   * function.
   *
   * \sa GetDegreeOfFreedomAtNode
   */
  virtual void SetDegreeOfFreedomAtNode( unsigned int nd, unsigned int dof_at_nd, DegreeOfFreedomIDType global_dof )
  {
    if (dof_at_nd>=this->GetNumberOfDegreesOfFreedomPerNode() || nd>=this->GetNumberOfNodes())
    {
      return;
    }
    this->SetDegreeOfFreedom(nd*this->GetNumberOfDegreesOfFreedomPerNode()+dof_at_nd, global_dof);
  }

  /**
   * Return a set of global point IDs, that define the n-th node in an 
   * element.
   *
   * Each node in an element must be somehow uniquely associated with a set
   * of geometrical points. This is required to determine if two nodes
   * in different elements correspond to the same point in space, and must
   * therefore also share its DOFs.
   *
   * In a linear elements each node is only associated with the geometrical
   * point at which it is defined. So the set contains only one global point
   * id. This is the default implementation provided in this class.
   *
   * If an element has additional nodes, you must override this function and
   * provide proper mapping from node number to set of global point id.
   *
   * Example: In quadratic elements, a node is typically located at a middle
   * point on an edge. An edge is defined with two points. So this node is
   * also uniquely defined with two global point ids that define that
   * edge. And this is exactly what this function should return.
   *
   * \sa NodeDefinitionType
   *
   * \note This function is only used to determine which nodes are shared
   *       between elements in a mesh. It does not store the actual
   *       coordinates of a node. The node coordinates must be calculated
   *       manually where they are required in a derived class from the
   *       given coordinates of geometrical points.
   *
   * \param n Node number within the element.
   *          Must be 0 <= n < GetNumberOfNodes().
   * \param def A reference to the multiset object which will return the
   *            global point ids of the n-th node.
   */
  virtual void GetNodeDefinition(unsigned int n, NodeDefinitionType& def) const
  {
    def.clear();
    if( n>=this->GetNumberOfPoints() ) { return; }
    def.insert(this->GetPoint(n));
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
