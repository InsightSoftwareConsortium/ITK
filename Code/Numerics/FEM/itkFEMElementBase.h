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



// FIXME: Write better documentation
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
  { return VisitorDispatcher<Self,LoadElementType,LoadVectorType (*)(Self::ConstPointer,LoadElementPointer)>::Visit(l)(this,l); } \
  virtual VectorType GetLoadVector( LoadElementPointer l ) const \
  { return VisitorDispatcher<Self,LoadElementType,LoadVectorType (*)(Self::ConstPointer,LoadElementPointer)>::Visit(l)(this,l); }




class Element : public FEMLightObject
{
FEM_ABSTRACT_CLASS(Element,FEMLightObject)
public:
  /**
   * Floating point type used in all Element classes.
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
  typedef vnl_matrix<Float> MatrixType;

  /**
   * Class to store the element load vector
   */
  typedef vnl_vector<Float> LoadVectorType;
  typedef vnl_vector<Float> VectorType;

  /**
   * Easy and consistent access to LoadElement and LoadElement::Pointer type.
   * This is a pointer to FEMLightObject to avoid cyclic references between
   * LoadElement and Element classes.
   * As a consequence whenever you need to use a pointer to LoadElement class
   * within the element's declaration or definition, ALWAYS use this typedef
   * instead.
   * When calling the GetLoadVector(...) function from outside, you should
   * ALWAYS first convert the argument to Element::LoadElementPointer. See
   * code of function Solver::AssembleF(...) for more info.
   */
  typedef FEMLightObject LoadElementType;
  typedef LoadElementType::Pointer LoadElementPointer;




//////////////////////////////////////////////////////////////////////////
  /*
   * Old methods. FIXME: To be removed when the new base class is working.
   */

  /**
   * Compute and return element stiffnes matrix in global coordinate system
   */
  virtual StiffnesMatrixType Ke() const
  {
    StiffnesMatrixType K;
    this->GetStiffnessMatrix(K);
    return K;
  }


  /**
   * Compute and return element mass matrix in global coordinate system.
   * This is needed if dynamic problems (parabolic or hyperbolix d.e.)
   * need to be solved.
   */
  virtual vnl_matrix<Float> Me() const;

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
   * Methods related to the physics of the problem.
   */

  /**
   * Compute and return element stiffnes matrix (Ke) in global coordinate
   * system.
   * The base class provides a general implementation which only computes
   *
   *     b   T
   * int    B(x) D B(x) dx
   *     a
   *
   * using the Gaussian numeric integration method. The function calls
   * GetIntegrationPoint() / GetNumberOfIntegrationPoints() to obtain the
   * integration points. It also calls the GetStrainDisplacementMatrix()
   * and GetMaterialMatrix() member functions.
   *
   * \param Ke Reference to the resulting stiffnes matrix.
   *
   * \note This is a very generic implementation of the stiffness matrix
   *       that is suitable for any problem/element definition. A specifc
   *       element may override this implementation with its own simple one.
   */
  virtual void GetStiffnessMatrix( MatrixType& Ke ) const;

  /**
   * Compute and return element mass matrix in global coordinate system.
   * This is needed if dynamic problems (parabolic or hyperbolix d.e.)
   * need to be solved.
   */
  virtual void GetMassMatrix( MatrixType& Me ) const;

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
   * \note Each derived class must implement its own version of this function.
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
  virtual VectorType GetLoadVector( LoadElementPointer l ) const = 0;

  /**
   * Compute the strain displacement matrix at local point.
   *
   * \param B Reference to a matrix object that will contain the result
   * \param shapeDgl Matrix that contains derivatives of shape functions
   *                 w.r.t. global coordinates.
   */
  virtual void GetStrainDisplacementMatrix( MatrixType& B, const MatrixType& shapeDgl ) const {}// = 0;

  /**
   * Compute the element material matrix.
   *
   * \param D Reference to a matrix object
   */
  virtual void GetMaterialMatrix( MatrixType& D ) const {}// = 0;

  /**
   * Return interpolated value of all unknown functions at
   * given local point.
   *
   * \param pt Point in local element coordinates.
   * \param sol Reference to the master solution object. This object
   *            is created by the Solver object when the whole FEM problem
   *            is solved and contains the values of unknown functions
   *            at nodes (degrees of freedom).
   */
  virtual VectorType InterpolateSolution( const VectorType& pt, const Solution& sol ) const;

  /**
   * Return interpolated value of f-th unknown function at
   * given local point.
   *
   * \param pt Point in local element coordinates.
   * \param sol Reference to the master solution object. This object
   *            is created by the Solver object when the whole FEM problem
   *            is solved and contains the values of unknown functions
   *            at nodes (degrees of freedom).
   * \param f Number of unknown function to interpolate.
   *          Must be 0 <= f < GetNumberOfDegreesOfFreedomPerNode().
   */
  virtual Float InterpolateSolution( const VectorType& pt, const Solution& sol, unsigned int f ) const;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  /**
   * Returns the vector representing the i-th integration point.
   *
   * \sa GetWeightAtIntegrationPoint()
   * \sa GetNumberOfIntegrationPoints()
   */
  virtual VectorType GetIntegrationPoint( unsigned int i ) const { return VectorType(); }// = 0;

  /**
   * Returns the summation weight at i-th integration point.
   *
   * \sa GetIntegrationPoint()
   */
  virtual Float GetWeightAtIntegrationPoint( unsigned int i ) const { return 0.0; }// = 0;

  /**
   * Returns total number of integration points.
   *
   * \sa GetIntegrationPoint()
   */
  virtual unsigned int GetTotalNumberOfIntegrationPoints( void ) const { return 0; }// = 0;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  /*
   * FIXME: The next four should be implemented in Cell/Mesh
   */
  typedef Node::ConstPointer PointIDType;
  virtual unsigned int GetNumberOfPoints(void) const = 0;
  virtual PointIDType GetPoint(unsigned int pt) const = 0;
  virtual void SetPoint(unsigned int pt, PointIDType node) = 0;

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
  virtual unsigned int GetNumberOfNodes( void ) const = 0;

  /**
   * Return a vector of global coordinates of n-th node in an element.
   *
   * \param n Local number of node. Must be 0 <= n < this->GetNumberOfNodes().
   */
  virtual VectorType GetNodalCoordinates( unsigned int n ) const { return VectorType(); } // = 0;

  /**
   * Transforms the given local element coordinates into global.
   *
   * \param pt Point in local element coordinates.
   */
  virtual VectorType GetGlobalFromLocalCoordinates( const VectorType& pt ) const;

  /**
   * Transforms the given global element coordinates into local.
   *
   * \param pt Point in global (world) coordinates.
   */
  virtual VectorType GetLocalFromGlobalCoordinates( const VectorType& pt ) const { return VectorType(); } // = 0;

  /**
   * Returns a vector containing the values of all shape functions
   * that define the geometry of a finite element at a given local point
   * within an element.
   *
   * \param pt Point in local element coordinates.
   */
  virtual VectorType ShapeFunctions( const VectorType& pt ) const { return VectorType(); } // = 0;

  /**
   * Compute the matrix of values of the shape functions derivatives with
   * respect to local coordinates of this element at a given point.
   *
   * A column in this matrix corresponds to a specific shape function,
   * while a row corresponds to different local coordinates. E.g.
   * element at row 2, col 3 contains derivative of shape function
   * number 3 with respect to local coordinate number 2.
   *
   * \param pt Point in local element coordinates.
   * \param shapeD Reference to a matrix object, which will be filled
   *               with values of shape function derivatives.
   *
   * \sa ShapeFunctionGlobalDerivatives
   */
  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const {} // = 0;

  /**
   * Compute matrix of shape function derivatives with respect to
   * global coordinates.
   *
   * A column in this matrix corresponds to a specific shape function,
   * while a row corresponds to different global coordinates.
   *
   * \param pt Point in local element coordinates.
   * \param shapeDgl Reference to a matrix object, which will be filled
   *                 with values of shape function derivatives w.r.t. global
   *                 (world) element coordinates.
   * \param pJ Optional pointer to Jacobian matrix computed at point pt. If this
   *           is set to 0, the Jacobian will be computed as necessary.
   * \param pshapeD A pointer to derivatives of shape functions at point pt.
   *                If this pointer is 0, derivatives will be computed as
   *                necessary.
   *
   * \sa ShapeFunctionDerivatives
   */
  virtual void ShapeFunctionGlobalDerivatives( const VectorType& pt, MatrixType& shapeDgl, const MatrixType* pJ=0, const MatrixType* pshapeD=0 ) const;

  /** 
   * Compute the Jacobian matrix of the transformation from local
   * to global coordinates at a given local point.
   *
   * A column in this matrix corresponds to a global coordinate,
   * while a row corresponds to different local coordinates. E.g.
   * element at row 2, col 3 contains derivative of the third global
   * coordinate with respect to local coordinate number 2.
   *
   * In order to compute the Jacobian, we normally need the shape
   * function derivatives. If they are known, you should pass a
   * pointer to an object of MatrixType that contains the shape
   * function derivatives. If they are not known, pass null pointer
   * and they will be computed automatically.
   *
   * \param pt Point in local coordinates
   * \param J referece to matrix object, which will contain the jacobian
   * \param pshapeD A pointer to derivatives of shape functions at point pt.
   *                If this pointer is 0, derivatives will be computed as
   *                necessary.
   */
  virtual void Jacobian( const VectorType& pt, MatrixType& J, const MatrixType* pshapeD = 0 ) const;

  /**
   * Compute the determinant of the Jacobian matrix
   * at a given point with respect to the local
   * coordinate system.
   *
   * \param pt Point in local element coordinates.
   * \param pJ Optional pointer to Jacobian matrix computed at point pt. If this
   *           is set to 0, the Jacobian will be computed as necessary.
   */
  virtual Float JacobianDeterminant( const VectorType& pt, const MatrixType* pJ = 0 ) const;

  /**
   * Compute the inverse of the Jacobian matrix
   * at a given point with respect to the local
   * coordinate system.
   *
   * \param pt Point in local element coordinates.
   * \param invJ Reference to the object of MatrixType that will store the
   *             computed inverse if Jacobian.
   * \param pJ Optional pointer to Jacobian matrix computed at point pt. If this
   *           is set to 0, the Jacobian will be computed as necessary.
   */
  virtual void JacobianInverse( const VectorType& pt, MatrixType& invJ, const MatrixType* pJ = 0 ) const;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and typedefs related to degrees of freedom management
   */

  /**
   * Type that stores global ID's of degrees of freedom.
   *
   * Derived classes must provide static storage for an array of objects of
   * class DegreeOfFreedomIDType.
   *
   * FIXME: This can be something else than an int. Maybe a pointer to
   *        a DOF object, or something...
   */
  typedef unsigned int DegreeOfFreedomIDType;

  /**
   * Constant that represents an invalid DegreeOfFreedomID object.
   * If a degree of freedom is assigned this value, this means that
   * that no specific value was (yet) assigned to this DOF.
   */ 
  enum{ InvalidDegreeOfFreedomID = 0xffffffff };

  /**
   * Type used to store an array of global point IDs that define a node.
   *
   * \sa GetNodeDefinition
   */
  typedef std::vector<PointIDType> NodeDefinitionType;



  /**
   * Return the total number of degrees of freedom defined in a derived
   * element class. By default this is equal to number of points in a cell
   * multiplied by number of degrees of freedom at each point. If a derived
   * class has more advanced DOFs (for example nodes and DOFs associated
   * with edges) this function must be overriden in derived class.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedom( void ) const
  {
    return this->GetNumberOfPoints() * this->GetNumberOfDegreesOfFreedomPerNode();
  }

  /**
   * Method to get DOF ids. Returns the global id of the DOF with given
   * local id. If id is out of range the function returns invalid DOF id.
   *
   * \param local_dof Number of DOF within an element (local id of DOF).
   *
   * \note This function must be overriden in all derived classes.
   */
  virtual DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int local_dof ) const = 0;

  /**
   * Method to set DOF ids. Sets local DOF with given id to the global id,
   * which is provided in dof parameter.
   *
   * \param local_dof Number of DOF within an element (local id of DOF).
   * \param global_dof Global DOF id.
   *
   * \note This function must be overriden in all derived classes.
   */
  virtual void SetDegreeOfFreedom( unsigned int local_dof, DegreeOfFreedomIDType global_dof) = 0;

  /**
   * Releases all DOFs used by element. It sets all local DOF
   * ids to invalid global id values.
   */
  virtual void ClearDegreesOfFreedom( void );

  /**
   * Return the number of degrees of freedom at each node. This is also
   * equal to number of unknowns that we want to solve for at each point
   * within an element.
   *
   * \note This function must be overriden in all derived classes.
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
    def.insert(def.end(),this->GetPoint(n));
  }




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and classes related to IO and drawing
   */

#ifdef FEM_BUILD_VISUALIZATION
  /**
   * Draws the element on the DC.
   */
  virtual void Draw(CDC* pDC, Solution::ConstPointer sol) const {}
  /** global scale for drawing on the DC */
  static double& DC_Scale;
#endif




  /**
   * Create a new unique DOF id.
   * FIXME: This code is not multithread safe.
   */
  static DegreeOfFreedomIDType CreateNewGlobalDOF(void)
  {
    return ++m_DOFCounter;
  };

  /**
   * Reset the DOF id counter.
   */
  static void ResetGlobalDOFCounter(void)
  {
    m_DOFCounter=InvalidDegreeOfFreedomID;
  };

  /**
   * Return the current value of DOF id counter.
   */
  static DegreeOfFreedomIDType GetGlobalDOFCounter(void)
  {
    return m_DOFCounter;
  }

private:

  /**
   * Storage for DOF id counter.
   */
  static DegreeOfFreedomIDType m_DOFCounter;

};




/**
 * \class ReadInfoType
 * \brief Helper class for storing additional information that is required
 *        when reading FEM objects from stream.
 *
 * When an element is to be read from the input stream, we must provide
 * pointers to the array of nodes and materials. When reading load objects
 * we also need pointer to the array of elements. Construct object of this
 * class and pass a pointer to it when calling Read virtual member function
 * for any type of fem classes.
 */
class ReadInfoType
{
public:

  typedef Node::ArrayType::ConstPointer NodeArrayPointer;
  typedef Element::ArrayType::ConstPointer ElementArrayPointer;
  typedef Material::ArrayType::ConstPointer MaterialArrayPointer;

  /** Pointer to an array of nodes. */
  NodeArrayPointer m_node;

  /** Pointer to an array of elements */
  ElementArrayPointer m_el;

  /** Pointer to an array of materials. */
  MaterialArrayPointer m_mat;

  /** Constructor for simple object creation. */
  ReadInfoType( NodeArrayPointer node_, ElementArrayPointer el_, MaterialArrayPointer mat_) :
    m_node(node_), m_el(el_), m_mat(mat_) {}
};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementBase_h
