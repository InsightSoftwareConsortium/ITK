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

#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/algo/vnl_svd.h"

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
   * Look up table for mapping between global freedom number and pointers to DOF displacements
   */
  typedef std::vector<Element::Displacement*> GFN2DispMapType;
  GFN2DispMapType GFN2Disp;

  /**
   * Definition of matrix class that is used within Solver.
   * \note This should be sparse matrix, since the matrices in solver are very large.
   */
  typedef vnl_sparse_matrix<Float> MatrixType;

  /**
   * Definition of vector class that is used within Solver.
   */
  typedef vnl_vector<Float> VectorType;

  /**
   * Definition of class that is used to solve linear system of equations.
   */
  typedef vnl_svd<Float> EQSType;
   

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

private:

  /**
   * Read any object from stream. This function is only called by the Read
   * member function inthe Solver class.
   */
  FEMLightObject::Pointer ReadAnyObjectFromStream(std::istream& f);

public:
  /**
   * Default constructor
   */
  Solver() {}

  /**
   * Default destructor. We need to destroy the equation solver object
   * before destruction of the Solver.
   */
  ~Solver() {}

  /**
   * Assembled master stiffnes matrix (NDOF+size_of_MFC= size_of_K), includes the MFC
   */
  MatrixType K;

  /**
   * Assembled master load vector (NDOF+size_of_MFC= size_of_F)
   */
  VectorType F;
  
  /**
   * Solution of master equation (after the solve is called) (size_of_F = size_of_u)
   */
  VectorType u;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolver_h
