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
   * System solver functions. Call all six functions below (in listed order) to solve system.
   */

  /**
   * Assign a global freedom numbers to each DOF in a system.
   * This must be done before any other solve function can be called.
   */
  void GenerateGFN();            
            
  /**
   * Assemble the master stiffness matrix (also apply the MFCs to K)
   */  
  void AssembleK();            

  /**
   * Assemble the master force vector.
   *
   * \param dim This is a parameter that can be passed to the function and is
                normally used with isotropic elements to specify the
                dimension for which the master force vector should be assembled.
   */
  void AssembleF(int dim=0);

  /**
   * Decompose matrix using svd, qr, whatever ...
   */
  void DecomposeK();

  /**
   * Solve for the displacement vector u
   */
  void Solve();

  /**
   * Copy solution vector u to the corresponding nodal values, which are
   * stored in node objects). This is standard post processing of the solution
   */
  void UpdateDisplacements();

  Float GetSolution(int i)
  {
    return m_ls->GetX(i);
  }

protected:

  /**
   * Number of global degrees of freedom in a system
   */
  int NGFN;

  /**
   * Number of multi freedom constraints in a system.
   * This member is set in a AssembleK function.
   */
  int NMFC;

  /** Pointer to LinearSystemWrapper object. */
  LinearSystemWrapper* m_ls;

public:
  /**
   * Default constructor sets Solver to use VNL linear system .
   * \sa Solver::SetLinearSystemWrapper
   */
  Solver() : m_ls(&m_lsVNL) { Node::solution.clear(); }

  /**
   * Default destructor. We need to destroy the equation solver object
   * before destruction of the Solver.
   */
  ~Solver() {}

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
  void SetLinearSystemWrapper(LinearSystemWrapper* ls) { m_ls=ls; }

private:

  /**
   * Read any object from stream. This function is only called by the Read
   * member function inthe Solver class.
   */
  FEMLightObject::Pointer ReadAnyObjectFromStream(std::istream& f);

  /**
   * LinearSystemWrapperVNL object that is used by default in Solver class.
   */
  LinearSystemWrapperVNL m_lsVNL;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolver_h
