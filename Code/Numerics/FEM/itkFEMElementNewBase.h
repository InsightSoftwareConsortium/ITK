/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementNewBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementNewBase_h
#define __itkFEMElementNewBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include "itkFEMNodeBase.h"
#include "itkFEMElementBase.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMSolution.h"
#include "itkVisitorDispatcher.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include <set>
#include <vector>

namespace itk {
namespace fem {



// FIXME: Write better documentation
/**
 * \class ElementNew
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
 * \def HANDLE_ELEMENT_LOADS()
 * \brief Macro that simplifies the the GetLoadVector function definitions.
 *
 * NOTE: This macro must be called in declaration of ALL
 *       derived Element classes.
 */
#define HANDLE_ELEMENT_LOADS() \
  virtual VectorType GetLoadVector( LoadElementPointer l ) const \
  { return VisitorDispatcher<Self,LoadElementType,VectorType>::Visit(this,l); }




class ElementNew : public Element
{
FEM_CLASS_SP(ElementNew,Element)
public:

  /*
   * Define functions required by parent class
   * FIXME: Remove, once this is the base class.
   */
  virtual LoadVectorType Fe(LoadElementPointer l) const { return this->GetLoadVector(l); }
  virtual unsigned int GetNumberOfPoints(void) const { return 0; }
  virtual PointIDType GetPoint(unsigned int pt) const { return PointIDType(); }
  virtual void SetPoint(unsigned int pt, PointIDType node) {}
  virtual DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int dof ) const
  {
    if(dof>this->GetNumberOfDegreesOfFreedom()) { return InvalidDegreeOfFreedomID; }
    return this->GetNode(dof/this->GetNumberOfDegreesOfFreedomPerNode())->GetDegreeOfFreedom(dof%this->GetNumberOfDegreesOfFreedomPerNode());
  }
  virtual void SetDegreeOfFreedom( unsigned int local_dof, DegreeOfFreedomIDType global_dof) {}
  virtual void ClearDegreesOfFreedom( void )
  {
    for(unsigned int n=0; n<this->GetNumberOfNodes(); n++)
    {
      this->GetNode(n)->ClearDegreesOfFreedom();
    }
  }


  /**
   * Floating point type used in all Element classes.
   */
  typedef double Float;

  /**
   * Array class that holds special pointers to the Element objects
   */
  typedef FEMPArray<ElementNew> ArrayType;

  /**
   * Class used to store the element stiffness matrix
   */
  typedef vnl_matrix<Float> MatrixType;

  /**
   * Class to store the element load vector
   */
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
//  typedef FEMLightObject LoadElementType;
//  typedef LoadElementType::Pointer LoadElementPointer;

  /**
   * Type that stores global ID's of degrees of freedom.
   */
//  typedef unsigned int DegreeOfFreedomIDType;

  /**
   * Constant that represents an invalid DegreeOfFreedomID object.
   * If a degree of freedom is assigned this value, this means that
   * that no specific value was (yet) assigned to this DOF.
   */ 
//  enum{ InvalidDegreeOfFreedomID = 0xffffffff };

  /**
   * \class ElementNew::Node
   * \brief Class that stores information required to define a node.
   *
   * A node can define a point in space and can hold an arbitrary number 
   * of coordinates and the DOFs. Since the only classes that use nodes
   * are the elements, the node class is defined within an element base class.
   */
  class Node : public ::itk::fem::Node
  {
  FEM_CLASS(Node,::itk::fem::Node)
  public:

    /**
     * Floating point precision type.
     */
    typedef double Float;

    /**
     * Array class that holds special pointers to the nodes.
     */
    typedef FEMPArray<Self> ArrayType;


    /* Windows visualization */
  #ifdef FEM_BUILD_VISUALIZATION
    /** Draws the node on the DC */
    void Draw(CDC* pDC, Solution::ConstPointer sol) const;
    /** Global scale for drawing on the DC */
    static double& DC_Scale;
  #endif

    /**
     * Default constructor
     */
    Node() {}

    /**
     * Create 2D node.
     */
    Node(Float x, Float y) : m_coordinates(VectorType(2))
    { m_coordinates[0]=x; m_coordinates[1]=y; }

    /**
     * Create 3D node.
     */
    Node(Float x, Float y, Float z) : m_coordinates(VectorType(x,y,z)) {}

    /**
     * Return a reference to a vector that contains coordinates
     * of this node.
     */
    const VectorType& GetCoordinates( void ) const
    { return m_coordinates; }

    /**
     * Set coordinates of a node.
     */
    void SetCoordinates( const VectorType& coords )
    { m_coordinates=coords; }

    /**
     * Get DOF IDs associated with this node.
     */
    DegreeOfFreedomIDType GetDegreeOfFreedom(unsigned int i) const
    {
      if( i>=m_dof.size() ) { return InvalidDegreeOfFreedomID; }
      return m_dof[i];
    }

    /**
     * Set DOF IDs associated with this node.
     */
    void SetDegreeOfFreedom(unsigned int i, DegreeOfFreedomIDType dof) const
    {
      if( i>=m_dof.size() ) { m_dof.resize(i+1, InvalidDegreeOfFreedomID); }
      m_dof[i]=dof;
    }

    virtual void ClearDegreesOfFreedom( void ) const
    {
      m_dof.clear();
    }

    virtual void Read(  std::istream& f, void* info );
    virtual void Write( std::ostream& f, int ofid=-1 ) const;

  public:
    /**
     * List of pointers to elements that use this node. External code is
     * responsible for maintaining the list.
     */
    typedef std::set<ElementNew*> SetOfElements;
    mutable SetOfElements m_elements;

  private:
    /**
     * Vector object that holds node coordinates.
     */
    VectorType m_coordinates;

    /**
     * Array that holds IDs of degrees of freedom that are
     * defined at this node.
     */
    mutable std::vector<DegreeOfFreedomIDType> m_dof;

  }; // end class Node




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
  virtual void GetStrainDisplacementMatrix( MatrixType& B, const MatrixType& shapeDgl ) const = 0;

  /**
   * Compute the element material matrix.
   *
   * \param D Reference to a matrix object
   */
  virtual void GetMaterialMatrix( MatrixType& D ) const = 0;

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
  virtual VectorType GetIntegrationPoint( unsigned int i ) const = 0;

  /**
   * Returns the summation weight at i-th integration point.
   *
   * \sa GetIntegrationPoint()
   */
  virtual Float GetWeightAtIntegrationPoint( unsigned int i ) const = 0;

  /**
   * Returns total number of integration points.
   *
   * \sa GetIntegrationPoint()
   */
  virtual unsigned int GetNumberOfIntegrationPoints( void ) const = 0;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  /**
   * Type that is used to store IDs of a node. It is a
   * pointer to Node objects.
   */
  typedef Node::ConstPointer NodeIDType;

  /**
   * Return the total number of nodes in an elememnt.
   */
  virtual unsigned int GetNumberOfNodes( void ) const = 0;

  /**
   * Returns the ID (pointer) of n-th node in an element.
   */
  virtual NodeIDType GetNode(unsigned int n) const = 0;

  /**
   * Sets the pointe of n-th node in an element to node.
   */
  virtual void SetNode(unsigned int n, NodeIDType node) = 0;

  /**
   * Return a vector of global coordinates of n-th node in an element.
   *
   * \param n Local number of node. Must be 0 <= n < this->GetNumberOfNodes().
   */
  virtual const VectorType& GetNodeCoordinates( unsigned int n ) const = 0;

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
  virtual VectorType GetLocalFromGlobalCoordinates( const VectorType& pt ) const = 0;

  /**
   * Returns a vector containing the values of all shape functions
   * that define the geometry of a finite element at a given local point
   * within an element.
   *
   * \param pt Point in local element coordinates.
   */
  virtual VectorType ShapeFunctions( const VectorType& pt ) const = 0;

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
  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const = 0;

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

  /**
   * Return the total number of degrees of freedom defined in a derived
   * element class. By default this is equal to number of points in a cell
   * multiplied by number of degrees of freedom at each point.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedom( void ) const
  {
    return this->GetNumberOfNodes() * this->GetNumberOfDegreesOfFreedomPerNode();
  }

  /**
   * Return the number of degrees of freedom at each node. This is also
   * equal to number of unknowns that we want to solve for at each point
   * within an element.
   *
   * \note This function must be overriden in all derived classes.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const = 0;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and classes related to IO and drawing
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
    ::itk::fem::Node::ArrayType::Pointer m_node;  /**< Pointer to an array nodes. */
    Material::ArrayType::Pointer m_mat;  /**< Pointer to an array of materials. */
    /** Constructor for simple object creation. */
    ReadInfoType(::itk::fem::Node::ArrayType::Pointer node_, Material::ArrayType::Pointer mat_) :
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

};

// Make sure that Element::Node class is registered with the object factory.
static INITClass Initializer_ElementNewNode(ElementNew::Node::OFID);

// Alias for Node class
typedef ElementNew::Node NodeNew;




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementNewBase_h
