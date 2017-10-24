/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMElementBase_h
#define itkFEMElementBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMSolution.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "ITKFEMExport.h"

#include <set>
#include <vector>

namespace itk
{
namespace fem
{
/**
 * \class Element
 * \brief Abstract base element class.
 *
 * Derive this class to create new finite element classes.
 * The storage of element parameters (geometry...) can't be implemented here, since we don't know yet,
 * how much memory each element needs. Instead each derived class should take care of the memory
 * management (declare appropriate data members) for the element parameters and provide access
 * to these parameters (like nodes, materials...).
 *
 * Derived classes must define the following class methods:
 *   GetIntegrationPointAndWeight
 *   GetNumberOfIntegrationPoints
 *   ShapeFunctions
 *   ShapeFunctionDerivatives
 *   GetLocalFromGlobalCoordinates
 *   JacobianDeterminant
 *   JacobianInverse
 *   PopulateEdgeIds
 *
 * These are required for the loads to be properly applied properly to the
 * element.
 *
 * \sa Element2DC0LinearLine
 * \sa Element2DC0LinearQuadrilateral
 * \sa Element2DC0LinearTriangular
 * \sa Element2DC1Beam
 * \sa Element2DC0QuadraticTriangular
 * \sa Element3DC0LinearHexahedron
 * \sa Element3DC0LinearTetrahedron
 * \sa Element3DC0LinearTriangular
 * \sa Element3DC0LinearTriangularLaplaceBeltrami
 * \ingroup ITKFEM
 */

class ITKFEM_EXPORT Element : public FEMLightObject
{
public:
  /** Standard class typedefs. */
  typedef Element                  Self;
  typedef FEMLightObject           Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element, FEMLightObject);

  /**
   * Floating point type used in all Element classes.
   */
  typedef double        Float;
  typedef unsigned long ElementIdentifier;

  /**
   * Array class that holds special pointers to the Element objects
   */
  // FIXME - Remove FEMPArray Type and replace with VectorContainer version
  typedef FEMPArray<Element>                                   ArrayType;
  typedef VectorContainer<ElementIdentifier, Element::Pointer> ArrayType1;

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
  typedef FEMLightObject    LoadType;
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
  enum { InvalidDegreeOfFreedomID = 0xffffffff };


/**
 * \class Node
 * \brief Class that stores information required to define a node.
 *
 * A node can define a point in space and can hold an arbitrary number
 * of coordinates and the DOFs. Since the only classes that use nodes
 * are the elements, the node class is defined within an element base class.
 *
 * \note Possibly move this class to its own file
 * \ingroup ITKFEM
 */
  class ITKFEM_EXPORT Node : public FEMLightObject
  {
  public:
    /** Standard class typedefs. */
    typedef Node                     Self;
    typedef FEMLightObject           Superclass;
    typedef SmartPointer<Self>       Pointer;
    typedef SmartPointer<const Self> ConstPointer;

    /** Method for creation through the object factory. */
    // itkNewMacro(Self);
    static Pointer New(void)
      {
        Pointer smartPtr = ::itk::ObjectFactory<Self>::Create();

        if( smartPtr.IsNull() )
          {
          smartPtr = static_cast<Pointer>(new Self);
          }
        smartPtr->UnRegister();
        return smartPtr;
      }

    /** Run-time type information (and related methods). */
    itkTypeMacro(Node, FEMLightObject);

    /** CreateAnother method will clone the existing instance of this type,
     * including its internal member variables. */
    virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

    /**
     * Floating point precision type.
     */
    typedef double Float;

    /**
     * Array class that holds special pointers to the nodes.
     */
    typedef FEMPArray<Self> ArrayType;

    /**
     * Default constructor
     */
    Node()
      {
      }
    /**
     * Destructor
     */
    ~Node() ITK_OVERRIDE
      {
        this->ClearDegreesOfFreedom();
        this->m_elements.clear();
      }

    /**
     * Return a reference to a vector that contains coordinates
     * of this node.
     */
    const VectorType & GetCoordinates(void) const
      {
        return m_coordinates;
      }

    /**
     * Set coordinates of a node.
     */
    void SetCoordinates(const VectorType & coords)
      {
        m_coordinates = coords;
      }

    /**
     * Get DOF IDs associated with this node.
     */
    DegreeOfFreedomIDType GetDegreeOfFreedom(unsigned int i) const
      {
        if( i >= m_dof.size() )
          {
          return InvalidDegreeOfFreedomID;
          }
        return m_dof[i];
      }

    /**
     * Set DOF IDs associated with this node.
     */
    void SetDegreeOfFreedom(unsigned int i, DegreeOfFreedomIDType dof) const
      {
        if( i >= m_dof.size() )
          {
          m_dof.resize(i + 1, InvalidDegreeOfFreedomID);
          }
        m_dof[i] = dof;
      }

    virtual void ClearDegreesOfFreedom(void) const;

  public:
    /**
     * List of pointers to elements that use this node. External code is
     * responsible for maintaining the list.
     */
    typedef std::set<Element *> SetOfElements;
    mutable SetOfElements m_elements;
  protected:
    virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;
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
  };  // end class Node

// ////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

  virtual VectorType GetStrainsAtPoint(const VectorType & pt, const Solution & sol, unsigned int index) const;

  virtual VectorType GetStressesAtPoint(const VectorType & pt, const VectorType & e, const Solution & sol,
                                        unsigned int index) const;

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
  virtual void GetStiffnessMatrix(MatrixType & Ke) const;

  /**
   * Compute the physical energy, U, of the deformation (e.g. stress / strain ).
   *
   *      T
   * U = u  Ke u
   *
   * The matrix LocalSolution contains the solution to use in the energy
   * computation.  Usually, this is the solution at the nodes.
   */
  virtual Float GetElementDeformationEnergy(MatrixType & LocalSolution) const;

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
  virtual void GetMassMatrix(MatrixType & Me) const;

  /**
   * Compute and return landmark contribution to element stiffness
   * matrix (Le) in global coordinate system.
   *
   *     b             T
   * int   (1/eta)^2  N(x) N(x) dx
   *     a
   *
   * where (eta ) is the landmark weight.  Implementation is similar
   * to GetMassMatrix.
   */
  virtual void GetLandmarkContributionMatrix(float eta, MatrixType & Le) const;

  /**
   * Compute the strain displacement matrix at local point.
   *
   * \param B Reference to a matrix object that will contain the result
   * \param shapeDgl Matrix that contains derivatives of shape functions
   *                 w.r.t. global coordinates.
   */
  virtual void GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const = 0;

  /**
   * Compute the element material matrix.
   *
   * \param D Reference to a matrix object
   */
  virtual void GetMaterialMatrix(MatrixType & D) const = 0;

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
  virtual VectorType InterpolateSolution(const VectorType & pt,
                                         const Solution & sol,
                                         unsigned int solutionIndex = 0) const;

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
  virtual Float InterpolateSolutionN(const VectorType & pt, const Solution & sol, unsigned int f,
                                     unsigned int solutionIndex = 0) const;

  /**
   * Convenient way to access IDs of degrees of freedom
   * that are stored in node objects.
   *
   * \param local_dof Local number of degree of freedom within an element.
   */
  DegreeOfFreedomIDType GetDegreeOfFreedom(unsigned int local_dof) const
    {
      if( local_dof > this->GetNumberOfDegreesOfFreedom() )
        {
        return InvalidDegreeOfFreedomID;
        }
      return this->GetNode(local_dof /
                           this->GetNumberOfDegreesOfFreedomPerNode() )
        ->GetDegreeOfFreedom(local_dof % this->GetNumberOfDegreesOfFreedomPerNode() );
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
  virtual Material::ConstPointer GetMaterial(void) const;

  /**
   * Set the pointer to the Material object used by the element.
   * All derived classes, which use objects of Material class should
   * override this method to provide access to the material from the
   * base class.
   *
   * \sa GetMaterial
   */
  virtual void SetMaterial(Material::ConstPointer);

  // ////////////////////////////////////////////////////////////////////////
  /**
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
  virtual void GetIntegrationPointAndWeight(unsigned int i,
                                            VectorType & pt,
                                            Float & w,
                                            unsigned int order = 0) const = 0;

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
  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order = 0) const = 0;

  /**
   * Maximum supported order of 1D Gauss-Legendre integration.
   * Integration points are defined for orders from 1 to gaussMaxOrder.
   * Number of integration points is equal to the order of integration
   * rule.
   *
   * \sa gaussPoint
   */
  itkStaticConstMacro(gaussMaxOrder, unsigned int, 10);

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
  static const Float gaussPoint[gaussMaxOrder + 1][gaussMaxOrder];

  /**
   * Weights for Gauss-Legendre integration.
   *
   * \sa gaussPoint
   */
  static const Float gaussWeight[gaussMaxOrder + 1][gaussMaxOrder];

// ////////////////////////////////////////////////////////////////////////
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
  virtual unsigned int GetNumberOfNodes(void) const = 0;

  /**
   * Returns the ID (pointer) of n-th node in an element.
   */
  virtual NodeIDType GetNode(unsigned int n) const = 0;

  /**
   * Sets the pointe of n-th node in an element to node.
   */
  virtual void SetNode(unsigned int n, NodeIDType node) = 0;
  virtual void SetNode(unsigned int n, Node::Pointer node);
  /**
   * Return a vector of global coordinates of n-th node in an element.
   *
   * \param n Local number of node. Must be 0 <= n < this->GetNumberOfNodes().
   */
  virtual const VectorType & GetNodeCoordinates(unsigned int n) const = 0;

  /**
   * Transforms the given local element coordinates into global.
   *
   * \param pt Point in local element coordinates.
   */
  virtual VectorType GetGlobalFromLocalCoordinates(const VectorType & pt) const;

  /**
   * Transforms the given global element coordinates into local.  Returns false if the point is outside.
   *
   * \param globalPt Reference to vector containing a point in global (world) coordinates.
   * \param localPt Reference to the vector that will store the local coordinate.
   */
  virtual bool GetLocalFromGlobalCoordinates(const VectorType & globalPt, VectorType & localPt) const = 0;

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
  virtual VectorType ShapeFunctions(const VectorType & pt) const = 0;

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
  virtual void ShapeFunctionDerivatives(const VectorType & pt, MatrixType & shapeD) const = 0;

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
  virtual void ShapeFunctionGlobalDerivatives(const VectorType & pt, MatrixType & shapeDgl, const MatrixType *pJ = ITK_NULLPTR,
                                              const MatrixType *pshapeD = ITK_NULLPTR) const;

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
   * \param J reference to matrix object, which will contain the jacobian
   * \param pshapeD A pointer to derivatives of shape functions at point pt.
   *                If this pointer is 0, derivatives will be computed as
   *                necessary.
   */
  virtual void Jacobian(const VectorType & pt, MatrixType & J, const MatrixType *pshapeD = ITK_NULLPTR) const;

  /**
   * Compute the determinant of the Jacobian matrix
   * at a given point with respect to the local
   * coordinate system.
   *
   * \param pt Point in local element coordinates.
   * \param pJ Optional pointer to Jacobian matrix computed at point pt. If this
   *           is set to 0, the Jacobian will be computed as necessary.
   */
  virtual Float JacobianDeterminant(const VectorType & pt, const MatrixType *pJ = ITK_NULLPTR) const;

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
  virtual void JacobianInverse(const VectorType & pt, MatrixType & invJ, const MatrixType *pJ = ITK_NULLPTR) const;

  /**
   * Return the total number of degrees of freedom defined in a derived
   * element class. By default this is equal to number of points in a cell
   * multiplied by number of degrees of freedom at each point.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedom(void) const;

  /**
   * Access the edge ids vector. The vector in turn contains a list of edge ids.
   */
  virtual std::vector<std::vector<int> > GetEdgeIds(void) const;

  /**
   * Return the number of degrees of freedom at each node. This is also
   * equal to number of unknowns that we want to solve for at each point
   * within an element.
   *
   * \note This function must be overriden in all derived classes.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode(void) const = 0;

  /** Set the edge order and the points defining each edge */
  virtual void PopulateEdgeIds(void) = 0;

protected:

  // to store edge connectivity data
  std::vector<std::vector<int> > m_EdgeIds;

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElementBase_h
