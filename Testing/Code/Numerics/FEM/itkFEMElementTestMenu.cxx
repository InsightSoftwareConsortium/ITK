/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTestMenu.cxx
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

int itkFEMElementTestMenu(int argc, char** argv)
//int main(int argc, char** argv)
{
  // NOTE TO THE USER: if you would like to run the menu-based test,
  // you can uncomment the "main" above and edit the CMakeLists.txt
  // file to create a separate executable called
  // itkFEMElementTestMenu.  Note that you will probably also need to
  // change the two paths below to point to the appropriate directory
  // in your ITK tree from your executable folder.

  // File input stream
  ifstream f;

  // Filename containing list of possible input files
  //char listloc[] = "../../../Data/Input/FEM/input-list";
  char listloc[] = "../../Insight/Testing/Data/Input/FEM/input-list";

  // Path to input files
  //char filepath[] = "../../../Data/Input/FEM/";
  char filepath[] = "../../Insight/Testing/Data/Input/FEM/";

  // Storage for list of or user-specified input file(s)
  char** filelist; 
  char buffer[80] = {'\0'};
  int numfiles = 0;
  char *fname = NULL;
  char *loc = NULL;

  // Flag in case command-line arguments are wrong
  int notfound = 0;

  if (argc >= 2)
  {
    // Parse the command-line arguments: 
    //
    // -m             displays the menu allowing the user to select a problem
    // -f <file>      uses the specified <file> as the input problem
    
    // Display the menu
    if (strstr(argv[1], "-m")) {
      // Read the list of input files into memory
      
      std::cout << "Loading menu..." << std::endl;
      
      f.open(listloc);
      if (!f) {
        std::cout << "ERROR: null file handle - couldn't read input file list\n";
        return EXIT_FAILURE;
      }

      f >> numfiles;
      filelist = new char*[numfiles];
      for (int k=0; k < numfiles; k++) {
      f >> buffer;
        filelist[k] = new char[strlen(buffer)+1];
        strcpy(filelist[k], buffer);
      }
      f.close();

      // Prompt the user to select a problem
      int ch = -1;
      while (ch < 0 || ch >= numfiles) {
        for (int j=0; j < numfiles; j++)
        std::cout << j << ": " << filelist[j] << std::endl;
        std::cout << std::endl << "NOTE: some of these problems follow an older data file" << std::endl;
        std::cout << "format, and have not yet been updated.  They may end in \"Abort\"." << std::endl;
        std::cout << std::endl << "Select an FEM problem to solve:  ";
        cin >> ch;
      }

      // Print the name of the selected problem
      std::cout << std::endl << "FEM Problem: " << filelist[ch] << std::endl;
      
      // Construct the file name appropriately from the list
      fname = new char[strlen(filepath)+strlen(filelist[ch])+5];
      strcpy(fname, filepath);
      strcat(fname, filelist[ch]);
    }
    // Accept a user-specified file
    else if ( (loc = strstr(argv[1], "-f")) ) {

      std::cout << "User-specified file..." << std::endl;
      
      fname = new char[strlen(argv[2])+5];
      strcpy(fname, argv[2]);

      // Print the name of the user-specified problem
      std::cout << std::endl << "FEM Input: " << fname << std::endl;
    }
    else { 
      std::cout << "Not found" << std::endl;
      notfound = 1; 
    }
  }

  // No or incorrect command-line arguments - display instructions and quit
  if (argc <= 1 || notfound)
  {
    std::cout << "Usage:" << std::endl;
    std::cout << "\tfor menu of test problems => itkFEMElement [-m]" << std::endl;
    std::cout << "\tto specify a test problem => itkFEMElement [-f] [filename]" << std::endl;

    if (argc <= 1) {
      std::cout << std::endl << "Note that you might also have to edit the input file paths in" << std::endl;
      std::cout << "itkFEMElementTest.cxx to get this program to run properly." << std::endl;
    }

    return EXIT_SUCCESS;
  }
    
  // Declare the FEM solver & associated input stream and read the input file
  f.open(fname);

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
  delete(fname);

  // Call the appropriate sequence of Solver methods to solve the
  // problem

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
//   for(Solver::m_ls->::iterator i=S.GFN2Disp.begin(); 
//       i!=S.GFN2Disp.end(); i++) 
//     std::cout<<(*i)->value<<"\n";

//   for (int j=0; j < S.GetSystemOrder(); j++)
//     std::cout << S.GetSolution(j) << std::endl;
//   std::cout << "End\n\n";

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
    std::cout<<"\b\b\b \b\n";
  }


  return EXIT_SUCCESS;
}
