/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkFEM.h"
#include "itkFEMSolver.h"

#include <iostream>
#include <fstream>

//#include <strings>

using namespace std;
using namespace itk;
using namespace fem;

int itkFEMElementTest(int ac, char** av)
{
  // File input stream
  ifstream f;

  // Path to input file
  // char fname[] = "../../Insight/Testing/Data/Input/FEM/hexa2.fem";

  std::cout << std::endl << "FEM Input: " << av[1] << std::endl;

  // Declare the FEM solver & associated input stream and read the input file
  f.open(av[1]);

  if (!f)
  {
    std::cout << "ERROR: null file handle...terminating.\n";
    return EXIT_FAILURE;
  }
  
  std::cout << "Solver()" << std::endl;
  Solver S;
  std::cout << "Read()" << std::endl;
  S.Read(f);
  std::cout << "Close file handle" << std::endl;
  f.close();

  // Call the appropriate sequence of Solver methods to solve the
  // problem

  try
    {
  std::cout << "GenerateGFN()" << std::endl;
  S.GenerateGFN();          // Generate global freedom numbers for system DOFs
  std::cout << "AssembleK()" << std::endl;
  S.AssembleK();            // Assemble the global stiffness matrix K
  std::cout << "DecomposeK()" << std::endl;
  S.DecomposeK();           // Invert K
  std::cout << "AssembleF()" << std::endl;
  S.AssembleF();            // Assemble the global load vector F
  std::cout << "Solve()"<< std::endl;
  S.Solve();                // Solve the system Ku=F for u
  std::cout << "UpdateDisplacements()" << std::endl;
  S.UpdateDisplacements();  // Copy the solution u's back to the nodes
  std::cout << "Done" << std::endl;

  // Print the solutions (displacements)
  std::cout << "Print displacements: " << std::endl;
  for( ::itk::fem::Solver::NodeArray::iterator n = S.node.begin(); n!=S.node.end(); n++)
  {
    std::cout<<"Node "<<(*n)->GN<<": ";
    /** For each DOF in the node... */
    for( unsigned int d=0, dof; (dof=(*n)->GetDegreeOfFreedom(d))!=::itk::fem::Element::InvalidDegreeOfFreedomID; d++ )
    {
      std::cout<<S.GetSolution(dof);
      std::cout<<",  ";
    }
    std::cout<<"\n";
  }

    }
  catch (itk::ExceptionObject &err)
    {
    std::cerr << "Exception detected: "  << err;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
