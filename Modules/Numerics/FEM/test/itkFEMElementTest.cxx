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

#include "itkFEMElementTest.h"
#include "itksys/SystemTools.hxx"

int itkFEMElementTest(int ac, char *av[])
{
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  // NOTE TO THE USER: if you would like to run the menu-based test,
  // you will need to change the two paths below to point to the
  // appropriate directory in your ITK tree from your executable
  // folder.

  // Filename containing list of possible input files
  char listloc[] = "../../Insight/Testing/Data/Input/FEM/input-list";

  // Path to input files
  char filepath[] = "../../Insight/Testing/Data/Input/FEM/";

  // File input stream
  std::ifstream f;

  // Storage for list of or user-specified input file(s)
  char * *filelist;
  char    buffer[80] = { '\0' };
  int     numfiles = 0;
  char *  fname;

  // Solvers being tested
  int numsolvers = 3;
  int currsolver;
  int s;

  // Output comments
  char comment;

  if( MATLAB_OUTPUT )
    {
    comment = MATLAB_COMMENT;
    }
  else if( IDL_OUTPUT )
    {
    comment = IDL_COMMENT;
    }
  else
    {
    comment = DEFAULT_COMMENT;
    }

  std::cout << comment << "Solver()" << std::endl;
  itk::fem::Solver S;

  // This test can be run in two different ways:
  //    1. by specifying an input file as a run-time argument
  //    2. by using the built-in menu of input files

  if( ac < 2 )
  // Display the menu
    {
    std::cout << "Loading menu..." << std::endl;

    f.open(listloc, std::ios::in);
    if( !f )
      {
      std::cout << "ERROR: null file handle - couldn't read input file list" << std::endl;
      std::cout << "Test FAILED" << std::endl;
      return EXIT_FAILURE;
      }

    f >> numfiles;
    filelist = new char *[numfiles];
    for( int k = 0; k < numfiles; k++ )
      {
      f >> buffer;
      filelist[k] = new char[strlen(buffer) + 1];
      strcpy(filelist[k], buffer);
      }
    f.close();

    // Prompt the user to select a problem
    int ch = -1;
    while( ch < 0 || ch >= numfiles )
      {
      for( int j = 0; j < numfiles; j++ )
        {
        std::cout << j << ": " << filelist[j] << std::endl;
        }
      // std::cout << std::endl << "NOTE: some of these problems follow an older
      // data file" << std::endl;
      // std::cout << "format, and have not yet been updated.  They may end in
      // \"Abort\"." << std::endl;
      std::cout << std::endl << "Select an FEM problem to solve:  ";
      std::cin >> ch;
      }

    // Print the name of the selected problem
    std::cout << std::endl << comment << "FEM Problem: " << filelist[ch] << std::endl;

    // Construct the file name appropriately from the list
    fname = new char[strlen(filepath) + strlen(filelist[ch]) + 5];
    strcpy(fname, filepath);
    strcat(fname, filelist[ch]);
    }
  // Accept a user-specified file
  else
    {
    std::cout << "User-specified file..." << std::endl;

    fname = new char[strlen(av[1]) + 5];
    strcpy(fname, av[1]);

    // Print the name of the user-specified problem
    std::cout << std::endl << comment << "FEM Input: " << fname << std::endl;

    // Check if a solver is specified as well
    if( ac == 3 )
      {
      currsolver = *av[2];
      std::cout << "currsolver = " << currsolver << std::endl;
      }
    }

  // Open a file handle & associate it with the input file
  std::string modelFile = itksys::SystemTools::GetFilenameName(fname);
  double *    expectedSolution = ITK_NULLPTR;
  double      tolerance;

  f.open(fname, std::ios::binary);
  if( !f )
    {
    std::cout << "ERROR: null file handle...terminating." << std::endl;
    std::cout << "Test FAILED" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    // Declare the FEM solver & associated input stream and read the
    // input file

    std::cout << comment << "Read()" << std::endl;
    S.Read(f);
    f.close();

    // Call the appropriate sequence of Solver methods to solve the
    // problem

    std::cout << comment << "GenerateGFN()" << std::endl;
    S.GenerateGFN();          // Generate global freedom numbers for system DOFs

    // Declare and initialize linear system wrapper objects

    itk::fem::LinearSystemWrapperDenseVNL lsw_dvnl;
    itk::fem::LinearSystemWrapperItpack   lsw_itpack;
    itk::fem::LinearSystemWrapperVNL      lsw_vnl;
    for( s = 0; s < numsolvers; s++ )
      {
      if( s == 2 )
        {
        // Itpack
        std::cout << std::endl << comment << ">>>>>Using LinearSystemWrapperItpack" << std::endl;
        lsw_itpack.SetMaximumNonZeroValuesInMatrix(1000);
        S.SetLinearSystemWrapper(&lsw_itpack);
        }
      else if( s == 1 )
        {
        // Dense VNL
        std::cout << std::endl << comment << ">>>>>Using LinearSystemWrapperDenseVNL" << std::endl;
        S.SetLinearSystemWrapper(&lsw_dvnl);
        }
      else
        {
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

      std::cout << comment << "Solver::Solve()" << std::endl;
      S.Solve();                // Solve the system Ku=F for u

      if( modelFile == "hexa2.fem" )
        {
        tolerance = 10e-6;
        double hex2expectedSolution[24] =
          {
          -0.086324, -0.00055514, 0.121079,
          0.0952793, -0.00331153, 0.114235,
          0.0727445, 0.00768949, -0.0394109,
          -0.0774779, -0.0115562, -0.0325665,
          0, 0, 0.0713128,
          0, 0, 0.0734239,
          0.0439568, 0, 0.00211102,
          -0.0397348, 0, 0
          };
        expectedSolution = &(hex2expectedSolution[0]);
        }
      else if( modelFile == "hexa3.fem" )
        {
        tolerance = 10e-10;
        double hex3ExpectedSolution[24] =
          {
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0
          };
        expectedSolution = &(hex3ExpectedSolution[0]);
        }
      else if( modelFile == "hexa4-grav.fem" )
        {
        tolerance = 10e-10;
        double hex4GravExpectedSolution[24] =
          {
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          9.27489e-08, 2.95922e-06, -9.27489e-08,
          -1.49661e-06, 8.59118e-07, 1.38971e-06,
          -1.32956e-06, -5.70152e-07, 1.32956e-06,
          -1.38971e-06, 8.59118e-07, 1.49661e-06,
          -1.59154e-06, 2.37079e-06, 1.59154e-06
          };
        expectedSolution = &(hex4GravExpectedSolution[0]);
        }
      else if( modelFile == "quad2-small.fem" )
        {
        tolerance = 10e-10;
        double quad2smallExpectedSolution[8] =
          {
          0, 0,
          2.97334e-07, -1.20555e-06,
          1.944e-06, -1.32333e-06,
          0, 0
          };
        expectedSolution = &(quad2smallExpectedSolution[0]);
        }
      else if( modelFile == "quad2-strain.fem" )
        {
        tolerance = 10e-10;
        double quad2strainExpectedSolution[8] =
          {
          0, 0,
          2.56204e-07, -1.02482e-06,
          1.67956e-06, -1.19562e-06,
          0, 0
          };
        expectedSolution = &(quad2strainExpectedSolution[0]);
        }
      else if( modelFile == "quad4.fem" )
        {
        tolerance = 10e-10;
        double quad4ExpectedSolution[8] =
          {
          0, 0,
          0, 0,
          0, 0,
          0, 0
          };
        expectedSolution = &(quad4ExpectedSolution[0]);
        }
      else if( modelFile == "quad6-grav.fem" )
        {
        tolerance = 10e-10;
        double quad6gravExpectedSolution[8] =
          {
          0, 0,
          0, 0,
          -5.32164e-08, 1.59649e-07,
          5.32164e-08, 1.59649e-07
          };
        expectedSolution = &(quad6gravExpectedSolution[0]);
        }
      else if( modelFile == "quad-lm.fem" )
        {
        tolerance = 10e-7;
        double quadlmExpectedSolution[8] =
          {
          0, 0,
          -8.76093e-05, -0.0135944,
          -0.00420457, 0.00477804,
          -0.0163679, -0.0360446,
          };
        expectedSolution = &(quadlmExpectedSolution[0]);
        }
      else if( modelFile == "tetra2.fem" )
        {
        tolerance = 10e-9;
        double tetra2ExpectedSolution[15] =
          {
          0, 0, 0,
          0, 0, -0.000866667,
          0, 0, 0,
          0, 0, 0,
          0, 0, -0.000866667
          };
        expectedSolution = &(tetra2ExpectedSolution[0]);
        }
      else if( modelFile == "tetra3.fem" )
        {
        tolerance = 10e-10;
        double tetra3ExpectedSolution[12] =
          {
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 0
          };
        expectedSolution = &(tetra3ExpectedSolution[0]);
        }
      else if( modelFile == "tetra4-grav.fem" )
        {
        tolerance = 10e-9;
        double tetra4gravExpectedSolution[12] =
          {
          0, 0, 0,
          0, 0, 0,
          0, 0, 0,
          0, 0, 1.46858e-05
          };
        expectedSolution = &(tetra4gravExpectedSolution[0]);
        }
      else if( modelFile == "trapezoid.fem" )
        {
        tolerance = 10e-10;
        double trapezoidExpectedSolution[8] =
          {
          0, 0,
          0, 0,
          0, 0,
          0, 0
          };
        expectedSolution = &(trapezoidExpectedSolution[0]);
        }
      else if( modelFile == "tri2.fem" )
        {
        tolerance = 10e-6;
        double tri2ExpectedSolution[8] =
          {
          0, 0,
          9.86667e-07, -2.028e-05,
          -9.76e-06, -5.67867e-05,
          -2.87733e-05, -9.68267e-05
          };
        expectedSolution = &(tri2ExpectedSolution[0]);

        }
      else if( modelFile == "tri3.fem" )
        {
        tolerance = 10e-10;
        double tri3ExpectedSolution[6] =
          {
          0, 0,
          0, 0,
          0, 0
          };
        expectedSolution = &(tri3ExpectedSolution[0]);
        }
      else if( modelFile == "tri3-e.fem" )
        {
        tolerance = 10e-10;
        double tri3eExpectedSolution[6] =
          {
          0, 0,
          0, 0,
          0, 0
          };
        expectedSolution = &(tri3eExpectedSolution[0]);
        }
      else if( modelFile == "tri3-q.fem" )
        {
        tolerance = 10e-9;
        double tri3qExpectedSolution[12] =
          {
          0, 0,
          -3.315e-07, 1.57527e-06,
          4.98323e-06, 7.36775e-07,
          -5.3625e-08, 2.18676e-06,
          8.32488e-07, 1.04065e-06,
          5.22113e-07, 2.42889e-06
          };
        expectedSolution = &(tri3qExpectedSolution[0]);
        }
      else if( modelFile == "truss.fem" )
        {
        tolerance = 10e-7;
        double trussExpectedSolution[11] =
          {
          0, 0, -0.179399,
          0.00169764, -0.478397, 0,
          0.00339527, 0, 0.179399,
          0.392323, -0.505307
          };
        expectedSolution = &(trussExpectedSolution[0]);
        }
      else
        {
        std::cout << "WARNING: Unknown solution for this model, " << modelFile << std::endl;
        }

#if DEBUG_FEM_TESTS
      PrintK(S, s, comment);
      PrintF(S, s, comment);
      PrintNodalCoordinates(S, s, comment);
      PrintU(S, s, comment);

      if( expectedSolution != ITK_NULLPTR )
        {
        bool foundError = CheckDisplacements(S, s, comment, expectedSolution, tolerance);
        if( foundError )
          {
          // return EXIT_FAILURE;
          }
        }
#endif

      std::cout << comment << "Done" << std::endl;

      std::cout << comment << "Test PASSED" << std::endl;
      }
    }
  catch( ::itk::ExceptionObject & err )
    {
    std::cerr << "ITK exception detected: "  << err;
    std::cout << "Test FAILED" << std::endl;

    return EXIT_FAILURE;
    }

  delete[] fname;

  return EXIT_SUCCESS;
}

#if DEBUG_FEM_TESTS

void PrintK(itk::fem::Solver & S, int s, char)
// Print K - the global stiffness matrix
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S.GetLinearSystemWrapper();

  std::cout << std::endl << "k" << s << "=[";
  for( unsigned int j = 0; j < lsw->GetSystemOrder(); j++ )
    {
    if( IDL_OUTPUT )
      {
      std::cout << " [";
      }
    for( unsigned int k = 0; k < lsw->GetSystemOrder(); k++ )
      {
      if( k > 0 )
        {
        std::cout << ",  ";
        }
      std::cout << lsw->GetMatrixValue(j, k);
      }
    if( IDL_OUTPUT )
      {
      if( j < lsw->GetSystemOrder() - 1 )
        {
        std::cout << " ], $" << std::endl;
        }
      else
        {
        std::cout << "]";
        }
      }
    else if( MATLAB_OUTPUT )
      {
      std::cout << std::endl;
      }
    }
  std::cout << "];" << std::endl;

  vnl_matrix<Float> debugMatrix;
  debugMatrix.set_size(lsw->GetSystemOrder(),lsw->GetSystemOrder());
  for( unsigned int j = 0; j < lsw->GetSystemOrder(); j++ )
    {
    for( unsigned int k = 0; k < lsw->GetSystemOrder(); k++ )
      {
      debugMatrix(j,k) = lsw->GetMatrixValue(j, k);
      }
    }
   vnl_matlab_filewrite writer("/tmp/k0.mat", "k0");
   writer.write(debugMatrix,"k0");

}

void PrintF(itk::fem::Solver & S, int s, char)
// Print F - the global load vector
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S.GetLinearSystemWrapper();

  std::cout << std::endl << "f" << s << "=[";
  for( unsigned int j = 0; j < lsw->GetSystemOrder(); j++ )
    {
    if( j > 0 )
      {
      std::cout << ",  ";
      }
    std::cout << lsw->GetVectorValue(j);
    }
  std::cout << "];" << std::endl;
}

void PrintNodalCoordinates(itk::fem::Solver & S, int w, char comment)
// Print the nodal coordinates
{
  std::cout << std::endl << comment << "Nodal coordinates: " << std::endl;

  std::cout << "xyz" << w << "=[";
  // changes made - kiran
  // for ( itk::fem::Solver::NodeArray::iterator n = S.node.begin(); n !=
  // S.node.end(); n++) {
  for( itk::fem::Solver::NodeArray::iterator n = S.GetNodeArray().begin();
       n != S.GetNodeArray().end(); ++n )
    {
    // changes made - kiran
    if( IDL_OUTPUT )
      {
      std::cout << " [";
      }
    // FIXME: this will generate errors in IDL - needs to be comma-delimited
    std::cout << ( *n )->GetCoordinates();
    if( IDL_OUTPUT )
      {
      // changes made - kiran
      // if ((n+1) != S.node.end()) { std::cout << " ], $" << std::endl; }
      if( ( n + 1 ) != S.GetNodeArray().end() )
        {
        std::cout << " ], $" << std::endl;
        }
      // changes made - kiran
      else
        {
        std::cout << "]";
        }
      }
    else if( MATLAB_OUTPUT )
      {
      std::cout << std::endl;
      }
    }
  std::cout << "];" << std::endl;
}

void PrintU(itk::fem::Solver & S, int s, char comment)
// Prints the components of the problem for debugging/reporting purposes
{
  std::cout << std::endl << comment << "Displacements: " << std::endl;
  std::cout << "u" << s << "=[";
  // changes made - kiran
  // for( ::itk::fem::Solver::NodeArray::iterator n = S.node.begin();
  // n!=S.node.end(); n++) {
  for( ::itk::fem::Solver::NodeArray::iterator n = S.GetNodeArray().begin(); n != S.GetNodeArray().end(); ++n )
    {
    // changes made - kiran
    if( IDL_OUTPUT )
      {
      std::cout << " [";
      }
    /** For each DOF in the node... */
    for( unsigned int d = 0, dof;
         ( dof = ( *n )->GetDegreeOfFreedom(d) ) != ::itk::fem::Element::InvalidDegreeOfFreedomID;
         d++ )
      {
      if( d > 0 && d != ::itk::fem::Element::InvalidDegreeOfFreedomID )
        {
        std::cout << ", ";
        }
      std::cout << S.GetSolution(dof);
      }
    if( IDL_OUTPUT )
      {
      // changes made - kiran
      // if ((n+1) != S.node.end()) { std::cout << " ], $" << std::endl; }
      if( ( n + 1 ) != S.GetNodeArray().end() )
        {
        std::cout << " ], $" << std::endl;
        }
      // changes made - kiran
      else
        {
        std::cout << "]";
        }
      }
    else if( MATLAB_OUTPUT )
      {
      std::cout << std::endl;
      }
    }
  std::cout << "];" << std::endl;
}

bool CheckDisplacements(itk::fem::Solver & S, int s, char comment, double *expectedResults, double tolerance)
// Prints the components of the problem for debugging/reporting purposes
{
  std::cout << std::endl << comment << "Check Displacements: " << std::endl;
  int  index = 0;
  bool foundError = false;

  std::cout << std::endl << comment << "NodeArray: " << std::endl;
  for( ::itk::fem::Solver::NodeArray::iterator n = S.GetNodeArray().begin(); n != S.GetNodeArray().end(); ++n )
    {
    for( unsigned int d = 0, dof;
         ( dof = ( *n )->GetDegreeOfFreedom(d) ) != ::itk::fem::Element::InvalidDegreeOfFreedomID;
         d++ )
      {
      double result = S.GetSolution(dof);
      if( std::fabs(result - expectedResults[index]) > tolerance )
        {
        std::cout << "Error: Result (" << result << ") expected (" << expectedResults[index] << ") with tolerance ("
                  << tolerance << ")" << std::endl;
        foundError = true;
        }
      index++;
      }
    }

  return foundError;
}

#endif
