/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkFEMSolver_h
#define __itkFEMSolver_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMLoadBase.h"

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/algo/vnl_svd.h"

namespace itk {
namespace fem {




/**
 * \class Solver
 * \brief Main FEM solver
 * 
 * This is the main class used for solving the FEM problems.
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
  typedef std::vector<Element::Disp*> GFN2DispMapType;
  GFN2DispMapType GFN2Disp;

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
   * Assemble the master force vector
   */  
  void AssembleF(int dim=0);        

  /**
   * decompose matrix using svd, qr, whatever ...
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
   * Pointer to the equation solver object
   */
  vnl_svd<Float>* EQS;          

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
  Solver() : EQS(0) {}

  /**
   * Cleanup before destruction
   */  
  ~Solver() { delete EQS; }

  /**
   * Assembled master stiffnes matrix (NDOF+size_of_MFC= size_of_K), includes the MFC
   */
  vnl_matrix<Float> K;

  /**
   * Assembled master load vector (NDOF+size_of_MFC= size_of_F)
   */  
  vnl_vector<Float> F;
  
  /**
   * Solution of master equation (after the solve is called) (size_of_F = size_of_u)
   */
  vnl_vector<Float> u;

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolver_h
