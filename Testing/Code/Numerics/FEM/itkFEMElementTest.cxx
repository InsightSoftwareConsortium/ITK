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

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEM.h"
#include "itkFEMSolver.h"

#include "itkFEMLinearSystemWrappers.h"

#include "itkFEMNodes.h"

#include "itkExceptionObject.h"

#include <iostream>
#include <fstream>
#include <exception>

#define DEFAULT_COMMENT     '.'
#define MATLAB_COMMENT      '%'
#define IDL_COMMENT         ';'

// Only one of these _OUTPUT variables should be nonzero, otherwise
// things will become confusing!  If both are zero, no output will be
// generated.
#define MATLAB_OUTPUT       0
#define IDL_OUTPUT          0
#define DEBUG_FEM_TESTS     ( MATLAB_OUTPUT || IDL_OUTPUT )

using namespace std;
using namespace itk;
using namespace fem;

#if DEBUG_FEM_TESTS

void PrintResults(Solver& S, int s, char comment)
// Prints the components of the problem for debugging/reporting purposes
{
  // Print K - the global stiffness matrix
  LinearSystemWrapper::Pointer lsw = S.GetLinearSystemWrapper();
  
  std::cout << std::endl << "k" << s << "=[";
  for (int j=0; j < lsw->GetSystemOrder(); j++) {
    if (IDL_OUTPUT) { std::cout << " ["; }
    for (int k=0; k < lsw->GetSystemOrder(); k++) {
      if (k > 0) { std::cout << ",  "; }
      std::cout << lsw->GetMatrixValue(j,k);
    }
    if (IDL_OUTPUT) {
      if (j < lsw->GetSystemOrder()-1) { std::cout << " ], $" << std::endl; }
      else  { std::cout << "]"; }
    } 
    else if (MATLAB_OUTPUT) { std::cout << std::endl; }
  }
  std::cout << "];" << std::endl;
  
  // Print F - the global load vector
  
  std::cout << std::endl << "f" << s << "=[";
  for (int j=0; j < lsw->GetSystemOrder(); j++) {
    if (j > 0) { std::cout << ",  "; }
    std::cout << lsw->GetVectorValue(j);
  }
  std::cout << "];" << std::endl;
  
  // Print the nodal coordinates
  
  NodeXY::Pointer nxy;
  NodeXYZ::Pointer nxyz;
  NodeXYrotZ::Pointer nxyrz;
  
  std::cout << std::endl << comment << "Nodal coordinates: " << std::endl;
  std::cout << "xyz" << s << "=[";
  for (Solver::NodeArray::iterator n = S.node.begin(); n != S.node.end(); n++) {
    if (IDL_OUTPUT) { std::cout << " ["; }
    if ( ( nxyz = dynamic_cast<NodeXYZ*>(&*(*n)) ) ) {
      std::cout << nxyz->X << ", " << nxyz->Y << ", " << nxyz->Z;
    }
    else if ( ( nxy = dynamic_cast<NodeXY*>(&*(*n)) ) ) {
      std::cout << nxy->X << ", " << nxy->Y;
    }
    else if ( ( nxyrz =  dynamic_cast<NodeXYrotZ*>(&*(*n)) ) ) {
      std::cout << nxyrz->X << ", " << nxyrz->Y;
    }
    if (IDL_OUTPUT) { 
      if ((n+1) != S.node.end()) { std::cout << " ], $" << std::endl; }
      else { std::cout << "]"; }
    }
    else if (MATLAB_OUTPUT) { std::cout << std::endl; }
  }
  std::cout << "];" << std::endl;
  
  // Print the displacements
  std::cout << std::endl << comment << "Displacements: " << std::endl;
  std::cout << "u" << s << "=[";
  for( ::itk::fem::Solver::NodeArray::iterator n = S.node.begin(); n!=S.node.end(); n++) {
    if (IDL_OUTPUT) { std::cout << " ["; }
    /** For each DOF in the node... */
    for( unsigned int d=0, dof; (dof=(*n)->GetDegreeOfFreedom(d))!=::itk::fem::Element::InvalidDegreeOfFreedomID; d++ ) {
      if (d > 0 && d != ::itk::fem::Element::InvalidDegreeOfFreedomID) { std::cout<<", "; }
      std::cout<<S.GetSolution(dof);
    }
    if (IDL_OUTPUT) { 
      if ((n+1) != S.node.end()) { std::cout << " ], $" << std::endl; }
      else { std::cout << "]"; }
    }
    else if (MATLAB_OUTPUT) { std::cout << std::endl; }
  }
  std::cout << "];" << std::endl;
}
      
#endif    


int itkFEMElementTest(int ac, char** av)
{
  // NOTE TO THE USER: if you would like to run the menu-based test,
  // you will need to change the two paths below to point to the
  // appropriate directory in your ITK tree from your executable
  // folder.

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

  // Solvers being tested
  int numsolvers = 3;
  int currsolver = -1;
  int s = 0;

  // Output comments
  char comment;
  if (MATLAB_OUTPUT) { comment = MATLAB_COMMENT; }
  else if (IDL_OUTPUT) { comment = IDL_COMMENT; }
  else { comment = DEFAULT_COMMENT; }

  if (ac < 2)
  // Display the menu
  {
    std::cout << "Loading menu..." << std::endl;
    
    f.open(listloc);
    if (!f) {
      std::cout << "ERROR: null file handle - couldn't read input file list" << std::endl;
      std::cout << "Test FAILED" << std::endl;
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
      for (int j=0; j < numfiles; j++) { std::cout << j << ": " << filelist[j] << std::endl; }
      std::cout << std::endl << "NOTE: some of these problems follow an older data file" << std::endl;
      std::cout << "format, and have not yet been updated.  They may end in \"Abort\"." << std::endl;
      std::cout << std::endl << "Select an FEM problem to solve:  ";
      cin >> ch;
    }
    
    // Print the name of the selected problem
    std::cout << std::endl << comment << "FEM Problem: " << filelist[ch] << std::endl;
    
    // Construct the file name appropriately from the list
    fname = new char[strlen(filepath)+strlen(filelist[ch])+5];
    strcpy(fname, filepath);
    strcat(fname, filelist[ch]);
  }
  // Accept a user-specified file
  else {
    std::cout << "User-specified file..." << std::endl;
      
    fname = new char[strlen(av[1])+5];
    strcpy(fname, av[1]);
    
    // Print the name of the user-specified problem
    std::cout << std::endl << comment << "FEM Input: " << fname << std::endl;

    // Check if a solver is specified as well
    if (ac == 3) {
      currsolver = *av[2];
      std::cout << "currsolver = " << currsolver << std::endl;
    }
      
  }
    
  // Open a file handle & associate it with the input file
  f.open(fname);
  if (!f)
  {
    std::cout << "ERROR: null file handle...terminating." << std::endl;
    std::cout << "Test FAILED" << std::endl;
    return EXIT_FAILURE;
  }

  try {

    // Declare the FEM solver & associated input stream and read the
    // input file

    std::cout << comment << "Solver()" << std::endl;
    Solver S;
    std::cout << comment << "Read()" << std::endl;
    S.Read(f);
    //std::cout << "Close file handle" << std::endl;
    f.close();
    delete(fname);
    
    // Call the appropriate sequence of Solver methods to solve the
    // problem
    
    std::cout << comment << "GenerateGFN()" << std::endl;
    S.GenerateGFN();          // Generate global freedom numbers for system DOFs

    LinearSystemWrapperDenseVNL lsw_dvnl;
    LinearSystemWrapperItpack lsw_itpack;
    LinearSystemWrapperVNL lsw_vnl;

    for (s=0; s < numsolvers; s++) {
 
      if (s == 2) {
        // Itpack 
        std::cout << std::endl << comment << ">>>>>Using LinearSystemWrapperItpack" << std::endl;
        lsw_itpack.SetMaximumNonZeroValuesInMatrix(1000);
        S.SetLinearSystemWrapper(&lsw_itpack);
      }
      else if (s == 1) {
        // Dense VNL
        std::cout << std::endl << comment << ">>>>>Using LinearSystemWrapperDenseVNL" << std::endl;
        S.SetLinearSystemWrapper(&lsw_dvnl);
      }
      else {
        // Sparse VNL - default
        std::cout << std::endl << comment << ">>>>>Using LinearSystemWrapperVNL" << std::endl;
        S.SetLinearSystemWrapper(&lsw_vnl);
      }

      std::cout << comment << "AssembleK()" << std::endl;
      S.AssembleK();            // Assemble the global stiffness matrix K
    
      std::cout << comment << "DecomposeK()" << std::endl;
      S.DecomposeK();           // Invert K
      
      std::cout << comment << "AssembleF()" << std::endl;
      S.AssembleF();            // Assemble the global load vector F
      
      std::cout << comment << "Solve()"<< std::endl;
      S.Solve();                // Solve the system Ku=F for u
      
#if DEBUG_FEM_TESTS
      PrintResults(S, s, comment);
#endif
      std::cout << comment << "Done" << std::endl;
      
      std::cout << comment << "Test PASSED" << std::endl;
    }
  }
  catch (::itk::ExceptionObject &err) {
    std::cerr << "ITK exception detected: "  << err;
    std::cout << "Test FAILED" << std::endl;

    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}





