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
 * \def HANDLE_ELEMENT_LOADS()
 * \brief Macro that simplifies the the GetLoadVector function definitions.
 *
 * NOTE: This macro must be called in declaration of ALL
 *       derived Element classes.
 */
#define HANDLE_ELEMENT_LOADS() \
  /** Pointer type that specifies functions that can handle loads on this element */ \
  typedef void (*LoadImplementationFunctionPointer)(ConstPointer,Element::LoadPointer, Element::VectorType& ); \
  virtual void GetLoadVector( Element::LoadPointer l, Element::VectorType& Fe ) const \
  { VisitorDispatcher<Self,Element::LoadType, LoadImplementationFunctionPointer>::Visit(l)(this,l,Fe); }

class Element : public FEMLightObject
{
FEM_ABSTRACT_CLASS(Element,FEMLightObject)
public:

  /**
   * Floating point type used in all Element classes.
   */
  typedef double Float;

  /**
   * Array class that holds special pointers to the Element objects
   */
  typedef FEMPArray<Element> ArrayType;

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
   * ALWAYS first convert the argument to Element::LoadPointer. See
   * code of function Solver::AssembleF(...) for more info.
   */
  typedef FEMLightObject LoadType;
  typedef LoadType::Pointer LoadPointer;

  /**
   * Type that stores global ID's of degrees of freedom.
   */
  typedef unsigned int DegreeOfFreedomIDType;

  /**
   * Constant that represents an invalid DegreeOfFreedomID object.
   * If a degree of freedom is assigned this value, this means that
   * that no specific value was (yet) assigned to this DOF.
   */ 
  enum{ InvalidDegreeOfFreedomID = 0xffffffff };

  /**
   * \class Node
   * \brief Class that stores information required to define a node.
   *
   * A node can define a point in space and can hold an arbitrary number 
   * of coordinates and the DOFs. Since the only classes that use nodes
   * are the elements, the node class is defined within an element base class.
   */
  class Node : public FEMLightObject
  {
  FEM_CLASS(Node,FEMLightObject)
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
    virtual void Write( std::ostream& f ) const;

  public:
    /**
     * List of pointers to elements that use this node. External code is
     * responsible for maintaining the list.
     */
    typedef std::set<Element*> SetOfElements;
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
   * GetIntegrationPointAndWeight() / GetNumberOfIntegrationPoints() to obtain
   * the integration points. It also calls the GetStrainDisplacementMatrix()
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
   * Compute the physical energy, U, of the deformation (e.g. stress / strain ).
   *
   *      T
   * U = u  Ke u
   *         
   * The matrix LocalSolution contains the solution to use in the energy
   * computation.  Usually, this is the solution at the nodes.
   */
  virtual Float GetElementDeformationEnergy( MatrixType& LocalSolution ) const;

  /**
   * Compute and return element mass matrix (Me) in global coordinate system.
   *
   *     b   T
   * int    N(x) (rho c) N(x) dx
   *     a
   *
   * where (rho c) is constant (element density), which is here assumed to be
   * equal to one. If this is not the case, this function must be overriden in
   * a derived class. Implementation is similar to GetStiffnessMatrix.
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
   * \param l Pointer to a load object.
   * \param Fe Reference to vector object that will store nodal forces.
   *
   * \sa VisitorDispatcher
   */
  virtual void GetLoadVector( LoadPointer l, VectorType& Fe ) const = 0;

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
   * \param solutionIndex We allow more than one solution vector to be stored - this selects which to use in interpolation.
   */
  virtual VectorType InterpolateSolution( const VectorType& pt, const Solution& sol , unsigned int solutionIndex=0 ) const;

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
   * \param solutionIndex We allow more than one solution vector to be stored - this selects which to use in interpolation.
   */
  virtual Float InterpolateSolutionN( const VectorType& pt, const Solution& sol, unsigned int f , unsigned int solutionIndex=0 ) const;

  /**
   * Convenient way to access IDs of degrees of freedom 
   * that are stored in node objects.
   *
   * \param local_dof Local number of degree of freedom within an element.
   */
  DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int local_dof ) const
  {
    if(local_dof>this->GetNumberOfDegreesOfFreedom()) { return InvalidDegreeOfFreedomID; }
    return this->GetNode(local_dof/this->GetNumberOfDegreesOfFreedomPerNode())->GetDegreeOfFreedom(local_dof%this->GetNumberOfDegreesOfFreedomPerNode());
  }

  /**
   * Return the pointer to the Material object used by the element.
   * All derived classes, which use objects of Material class should
   * override this method to provide access to the material from the
   * base class.
   *
   * \note Derived Element classes don't have to use a material
   * class, but since the majority of the final Element classes
   * uses Material classes to specify phhysical constants that the
   * element depends on, we provide this virtual function that
   * enables easy access to this pointer from the base class. If the
   * derived class does not override this function, the returned pointer
   * is 0 by default, signaling that there is no Material object.
   * 
   * \sa SetMaterial
   */
  virtual Material::ConstPointer GetMaterial(void) const { return 0; }

  /**
   * Set the pointer to the Material object used by the element.
   * All derived classes, which use objects of Material class should
   * override this method to provide access to the material from the
   * base class.
   *
   * \sa GetMaterial
   */
  virtual void SetMaterial(Material::ConstPointer) {} // FIXME: maybe we should throw an exception instead




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  /**
   * Computes the vector representing the i-th integration point in 
   * local element coordinates for a Gauss-Legendre numerical integration
   * over the element domain. It also computes the weight at this integration
   * point.
   *
   * Optionally you can also specify the order of integration. If order
   * is not specified, it defaults to 0, which means that the derived element
   * should use the optimal integration order specific for that element.
   *
   * \note This function must be implemented in derived element classes, and
   *       is expected to provide valid integration points for up to
   *       gaussMaxOrder-th order of integration.
   *
   * \param i Integration point number 0<=i<GetNumberOfIntegrationPoints()
   * \param pt Reference to object of class VectorType that will hold the
   *           integration point.
   * \param w Reference to Float variable that will hold the weight.
   * \param order Order of integration.
   *
   * \sa GetNumberOfIntegrationPoints()
   */
  virtual void GetIntegrationPointAndWeight( unsigned int i, VectorType& pt, Float& w, unsigned int order=0 ) const = 0;

  /**
   * Returns total number of integration points, for given order
   * of Gauss-Legendre numerical integration rule.
   *
   * \note This function must be implemented in derived element classes, and
   *       is expected to provide valid number of integration points for up
   *       to gaussMaxOrder-th order of integration.
   *
   * \sa GetIntegrationPointAndWeight()
   */
  virtual unsigned int GetNumberOfIntegrationPoints( unsigned int order=0 ) const = 0;

  /**
   * Maximum supported order of 1D Gauss-Legendre integration.
   * Integration points are defined for orders from 1 to gaussMaxOrder.
   * Number of integration points is equal to the order of integration
   * rule.
   *
   * \sa gaussPoint
   */
  enum { gaussMaxOrder=10 };

  /**
   * Points for 1D Gauss-Legendre integration from -1 to 1. First
   * index is order of integration, second index is the number of
   * integration point.
   * 
   * Example: gaussPoint[4][2] returns third point of the 4th order
   * integration rule. Subarray gaussPoint[0][...] does not provide useful
   * information. It is there only to keep order index correct.
   *
   * \sa gaussWeight
   */
  static const Float gaussPoint[gaussMaxOrder+1][gaussMaxOrder];

  /**
   * Weights for Gauss-Legendre integration.
   *
   * \sa gaussPoint
   */
  static const Float gaussWeight[gaussMaxOrder+1][gaussMaxOrder];


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
   * Transforms the given global element coordinates into local.  Returns false if the point is outside.
   *
   * \param globalPt Reference to vector containing a point in global (world) coordinates.
   * \param localPt Reference to the vector that will store the local coordinate.
   */
  virtual bool GetLocalFromGlobalCoordinates( const VectorType& globalPt , VectorType& localPt ) const = 0;

  /**
   * Returns the number of dimensions of space in which the element is
   * defined. e.g. 2 for 2D elements, 3 for 3D... This is also equal
   * to the size vector containing nodal coordinates.
   */
  virtual unsigned int GetNumberOfSpatialDimensions() const = 0;

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

#ifdef FEM_BUILD_VISUALIZATION
  /**
   * Draws the element on the DC.
   */
  virtual void Draw(CDC* pDC, Solution::ConstPointer sol) const {}
  /** global scale for drawing on the DC */
  static double DC_Scale;
#endif

};

// Make sure that Element::Node class is registered with the object factory.
static INITClass Initializer_ElementNode(Element::Node::CLID);

// Alias for Element::Node class
typedef Element::Node Node;




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
