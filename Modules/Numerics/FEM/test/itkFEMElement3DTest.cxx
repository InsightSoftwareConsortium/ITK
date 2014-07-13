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


#include "itkFEMSolver.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMLinearSystemWrapperItpack.h"

typedef itk::fem::Solver<3> SolverType;

bool CheckDisplacements1(SolverType *S, int s, double *expectedResults, double tolerance);

void PrintF1(SolverType *S, int s);

void PrintNodalCoordinates1(SolverType *S, int w);

void PrintK1(SolverType *S, int s);

int itkFEMElement3DTest(int argc, char *argv[])
{
  if(argc < 1)
    {
    std::cerr << "Missing Spatial Object Filename" << std::endl;
    return EXIT_FAILURE;
    }
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();


  // Solvers being tested
  int numsolvers = 3;

  typedef itk::FEMSpatialObjectReader<3>      FEMSpatialObjectReaderType;
  typedef FEMSpatialObjectReaderType::Pointer FEMSpatialObjectReaderPointer;
  FEMSpatialObjectReaderPointer SpatialReader = FEMSpatialObjectReaderType::New();
  SpatialReader->SetFileName( argv[1] );
  SpatialReader->Update();

  FEMSpatialObjectReaderType::ScenePointer myScene = SpatialReader->GetScene();

  std::cout << "Scene Test: ";
  if( !myScene )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[PASSED]" << std::endl;
    }

  // Testing the fe mesh validity
  typedef itk::FEMObjectSpatialObject<3>      FEMObjectSpatialObjectType;

  FEMObjectSpatialObjectType::ChildrenListType* children = SpatialReader->GetGroup()->GetChildren();

  std::cout << "FEM Spatial Object Test: ";
  if( strcmp( (*(children->begin() ) )->GetTypeName(), "FEMObjectSpatialObject") )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[PASSED]" << std::endl;
    }

  FEMObjectSpatialObjectType::Pointer femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>( (*(children->begin() ) ).GetPointer() );
  if (!femSO)
    {
    std::cout << " dynamic_cast [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  delete children;

  femSO->GetFEMObject()->FinalizeMesh();

  double *    expectedSolution = ITK_NULLPTR;
  bool        foundError = false;
  std::string modelFile = itksys::SystemTools::GetFilenameName( argv[1] );

  // Define all expected solutions here
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
  double tetra2ExpectedSolution[15] =
    {
      0, 0, 0,
      0, 0, -0.000866667,
      0, 0, -0.000866667,
      0, 0, 0,
      0, 0, 0
    };
  double tetra3ExpectedSolution[12] =
    {
      0, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    };
  double tetra4gravExpectedSolution[12] =
    {
      0, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 1.46858e-05
    };

  // Run the Solver using all of the available numeric solvers

  try
    {
    // Declare the FEM solver & associated input stream and read the
    // input file

    // Declare and initialize linear system wrapper objects

    itk::fem::LinearSystemWrapperDenseVNL lsw_dvnl;
    itk::fem::LinearSystemWrapperItpack   lsw_itpack;
    itk::fem::LinearSystemWrapperVNL      lsw_vnl;
    for( int s = 0; s < numsolvers; s++ )
      {
      SolverType::Pointer solver = SolverType::New();
      solver->SetInput( femSO->GetFEMObject() );

      if( s == 2 )
        {
        // Itpack
        std::cout << std::endl << ">>>>>Using LinearSystemWrapperItpack" << std::endl;
        lsw_itpack.SetMaximumNonZeroValuesInMatrix(1000);
        solver->SetLinearSystemWrapper(&lsw_itpack);
        }
      else if( s == 1 )
        {
        // Dense VNL
        std::cout << std::endl << ">>>>>Using LinearSystemWrapperDenseVNL" << std::endl;
        solver->SetLinearSystemWrapper(&lsw_dvnl);
        }
      else
        {
        // Sparse VNL - default
        std::cout << std::endl << ">>>>>Using LinearSystemWrapperVNL" << std::endl;
        solver->SetLinearSystemWrapper(&lsw_vnl);
        }

      solver->Update();

      double      tolerance= 0.0;
      if( modelFile == "hexa2.meta" )
        {
        tolerance = 10e-6;
        expectedSolution = &(hex2expectedSolution[0]);
        }
      else if( modelFile == "hexa3.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(hex3ExpectedSolution[0]);
        }
      else if( modelFile == "hexa4-grav.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(hex4GravExpectedSolution[0]);
        }
      else if( modelFile == "tetra2.meta" )
        {
        tolerance = 10e-9;
        expectedSolution = &(tetra2ExpectedSolution[0]);
        }
      else if( modelFile == "tetra3.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(tetra3ExpectedSolution[0]);
        }
      else if( modelFile == "tetra4-grav.meta" )
        {
        tolerance = 10e-9;
        expectedSolution = &(tetra4gravExpectedSolution[0]);
        }
      else
        {
        std::cout << "WARNING: Unknown solution for this model, " << modelFile << std::endl;
        }

      PrintK1(solver, s);
      PrintF1(solver, s);
      PrintNodalCoordinates1(solver, s);
      // PrintU(S, s, );

      if( expectedSolution != ITK_NULLPTR )
        {
        bool testError = CheckDisplacements1(solver, s, expectedSolution, tolerance);
        if( testError )
          {
          std::cout << "Displacement Test : [FAILED]" << std::endl;
          }
        else
          {
          std::cout << "Displacement Test : [PASSED]" << std::endl;
          }
        foundError |= testError;
        }

      }
    }
  catch( ::itk::ExceptionObject & err )
    {
    std::cerr << "ITK exception detected: "  << err;
    std::cout << "Test FAILED" << std::endl;
    myScene = ITK_NULLPTR;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << ">>>>>" << std::endl;
  if( foundError )
    {
    std::cout << "Overall Test : [FAILED]" << std::endl;
    myScene = ITK_NULLPTR;
    return EXIT_FAILURE;
    }
  myScene = ITK_NULLPTR;
  std::cout << "Overall Test : [PASSED]" << std::endl;
  return EXIT_SUCCESS;
}

void PrintK1(SolverType *S, int s)
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

  std::cout << std::endl << "k" << s << "=[";
  for( unsigned int j = 0; j < lsw->GetSystemOrder(); j++ )
    {
    std::cout << " [";
    for( unsigned int k = 0; k < lsw->GetSystemOrder(); k++ )
      {
      std::cout << lsw->GetMatrixValue(j, k);
      if( (k + 1) < lsw->GetSystemOrder() )
        {
        std::cout << ", ";
        }
      }
    if( j < lsw->GetSystemOrder() - 1 )
      {
      std::cout << " ]," << std::endl;
      }
    else
      {
      std::cout << "]";
      }
    }
  std::cout << "];" << std::endl;
}

void PrintF1(SolverType *S, int s)
// Print F - the global load vector
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

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

void PrintNodalCoordinates1(SolverType *S, int w)
// Print the nodal coordinates
{
  std::cout << std::endl << "Nodal coordinates: " << std::endl;

  std::cout << "xyz" << w << "=[";

  int numberOfNodes = S->GetInput()->GetNumberOfNodes();
  for( int i = 0; i < numberOfNodes; i++ )
    {
    std::cout << " [";
    std::cout << S->GetInput()->GetNode(i)->GetCoordinates();
    std::cout << "]";
    }
  std::cout << "];" << std::endl;
}

bool CheckDisplacements1(SolverType *S, int s, double *expectedResults, double tolerance)
// Prints the components of the problem for debugging/reporting purposes
{
  // std::cout << std::endl << "Check Displacements: " << std::endl;

  int numDOF = S->GetInput()->GetNumberOfDegreesOfFreedom();
  // std::cout << "Degrees of Freedom : " << numDOF << std::endl;

  bool foundError = false;
  for( int i = 0; i < numDOF; i++ )
    {
    double result = S->GetSolution(i);
    // std::cout  << result << " " << expectedResults[i] << " " << tolerance << std::endl;
    if( std::fabs(expectedResults[i] - result) > tolerance )
      {
      std::cout << "ERROR: Solver " << s << " Index " << i << ". Expected " << expectedResults[i] << " Solution "
                << result << std::endl;
      foundError = true;
      }
    }

  return foundError;
}
