/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMSolver_h
#define __itkFEMSolver_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMLoadBase.h"

#include "itkFEMLinearSystemWrapper.h"
#include "itkFEMLinearSystemWrapperVNL.h"

namespace itk {
namespace fem {

/**
 * \class Solver
 * \brief Main FEM solver
 *
 * This is the main class used for solving the FEM problems. It also stores
 * all the objects that define the specific FEM problem. Normally there is
 * one Solver object for each FEM problem.
 */
class Solver 
{
public:

  /**
   * Local float type
   */
  typedef Element::Float Float;

  /**
   * Array that holds pointers to all elements. since we want to be
   * able to manipulate the array we have to use special pointers
   */
  typedef Element::ArrayType ElementArray;
  ElementArray el;

  /**
   * Array that holds special pointers to the nodes
   */
  typedef Node::ArrayType NodeArray;
  NodeArray node;

  /**
   * Array that holds special pointers to all external loads
   */
  typedef Load::ArrayType LoadArray;
  LoadArray load;

  /**
   * Array that holds pointers to the materials
   */
  typedef Material::ArrayType MaterialArray;
  MaterialArray mat;

  /**
   * Reads the whole system (nodes, materials and elements) from input stream
   */
  void Read( std::istream& f );
  
  /**
   * Writes everything (nodes, materials and elements) to output stream
   */
  void Write( std::ostream& f );

  /**
   * Cleans all data members, and initializes the solver to initial state.
   */
  virtual void Clear( void );


  /**
   * System solver functions. Call all six functions below (in listed order) to solve system.
   */

  /**
   * Assign a global freedom numbers to each DOF in a system.
   * This must be done before any other solve function can be called.
   */
  void GenerateGFN( void );
            
  /**
   * Assemble the master stiffness matrix (also apply the MFCs to K)
   */  
  void AssembleK( void );

  /**
   * This function is called before assembling the matrices. You can
   * override it in a derived class to account for special needs.
   *
   * \param N Size of the matrix.
   */
  virtual void InitializeMatrixForAssembly(unsigned int N);

  /**
   * This function is called after the assebly has been completed.
   * In this class it is only used to apply the BCs. You may however
   * use it to perform other stuff in derived solver classes.
   */
  virtual void FinalizeMatrixAfterAssembly( void )
  {
    // Apply the boundary conditions to the K matrix
    this->ApplyBC();
  };

  /**
   * Copy the element stiffness matrix into the correct position in the
   * master stiffess matrix. Since more complex Solver classes may need to
   * assemble many matrices and may also do some funky stuff to them, this
   * function is virtual and can be overriden in a derived solver class.
   */
  virtual void AssembleElementMatrix(Element::Pointer e);

  /**
   * Apply the boundary conditions to the system.
   *
   * \note This function must be called after AssembleK().
   *
   * \param matrix Index of a matrix, to which the BCs should be
   *               applied (master stiffness matrix). Normally this
   *               is zero, but in derived classes many matrices may
   *               be used and this index must be specified.
   * \param dim This is a parameter that can be passed to the function and is
   *            normally used with isotropic elements to specify the
   *            dimension in which the DOF is fixed.
   */
  void ApplyBC(int dim=0, unsigned int matrix=0);
  
  /**
   * Assemble the master force vector.
   *
   * \param dim This is a parameter that can be passed to the function and is
   *            normally used with isotropic elements to specify the
   *            dimension for which the master force vector should be assembled.
   */
  void AssembleF(int dim=0);

  /**
   * Decompose matrix using svd, qr, whatever ...
   */
  void DecomposeK( void );

  /**
   * Solve for the displacement vector u. May be overriden in derived classes.
   */
  virtual void Solve( void );

  /**
   * Copy solution vector u to the corresponding nodal values, which are
   * stored in node objects). This is standard post processing of the solution
   */
  void UpdateDisplacements( void );

  Float GetSolution(unsigned int i,unsigned int which=0)
  {
    return m_ls->GetSolutionValue(i,which);
  }

  unsigned int GetNumberOfDegreesOfFreedom( void )
  {
    return NGFN;
  }

  /** Get the total deformation energy using the chosen solution */
  Float GetDeformationEnergy(unsigned int SolutionIndex=0);

public:
  /**
   * Default constructor sets Solver to use VNL linear system .
   * \sa Solver::SetLinearSystemWrapper
   */
  Solver();

  /**
   * Virtual destructor
   */
  virtual ~Solver() {}

  /**
   * Sets the LinearSystemWrapper object that will be used when solving
   * the master equation. If this function is not called, a default VNL linear
   * system representation will be used (class LinearSystemWrapperVNL).
   *
   * \param ls Pointer to an object of class which is derived from
   *           LinearSystemWrapper.
   *
   * \note Once the LinearSystemWrapper object is changed, it is used until
   *       the member function SetLinearSystemWrapper is called again. Since
   *       LinearSystemWrapper object was created outside the Solver class, it
   *       should also be destroyed outside. Solver class will not destroy it
   *       when the Solver object is destroyed.
   */
  void SetLinearSystemWrapper(LinearSystemWrapper::Pointer ls);

  /**
   * Gets the LinearSystemWrapper object.
   *
   * \sa SetLinearSystemWrapper
   */
  LinearSystemWrapper::Pointer GetLinearSystemWrapper() { return m_ls; }

  /**
   * Performs any initialization needed for LinearSystemWrapper
   * object i.e. sets the maximum number of matrices and vectors.
   */
  virtual void InitializeLinearSystemWrapper(void);

  /**
   * Returns the time step used for dynamic problems.
   */
  virtual Float GetTimeStep( void ) const { return 0.0; }

  /**
   * Sets the time step used for dynamic problems.
   *
   * \param dt New time step.
   */
  virtual void SetTimeStep(Float dt) {}

protected:

  /**
   * Number of global degrees of freedom in a system
   */
  unsigned int NGFN;

  /**
   * Number of multi freedom constraints in a system.
   * This member is set in a AssembleK function.
   */
  unsigned int NMFC;

  /** Pointer to LinearSystemWrapper object. */
  LinearSystemWrapper::Pointer m_ls;

private:

  /**
   * LinearSystemWrapperVNL object that is used by default in Solver class.
   */
  LinearSystemWrapperVNL m_lsVNL;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolver_h
