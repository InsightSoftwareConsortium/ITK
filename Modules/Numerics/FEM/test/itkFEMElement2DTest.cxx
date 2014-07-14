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

typedef itk::fem::Solver<2> Solver2DType;

bool CheckDisplacements1(Solver2DType *S, int s, double *expectedResults, double tolerance);

void PrintF1(Solver2DType *S, int s);

void PrintNodalCoordinates1(Solver2DType *S, int w);

void PrintK1(Solver2DType *S, int s);

int itkFEMElement2DTest(int argc, char *argv[])
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

  typedef itk::FEMSpatialObjectReader<2>      FEMSpatialObjectReaderType;
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
  typedef itk::FEMObjectSpatialObject<2>      FEMObjectSpatialObjectType;

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
  double quad2smallExpectedSolution[8] =
    {
      0, 0,
      2.97334e-07, -1.20555e-06,
      1.944e-06, -1.32333e-06,
      0, 0
    };
  double quad2strainExpectedSolution[8] =
    {
      0, 0,
      2.56204e-07, -1.02482e-06,
      1.67956e-06, -1.19562e-06,
      0, 0
    };
  double quad4ExpectedSolution[8] =
    {
      0, 0,
      0, 0,
      0, 0,
      0, 0
    };
  double quad6gravExpectedSolution[8] =
    {
      0, 0,
      0, 0,
      -5.32164e-08, 1.59649e-07,
      5.32164e-08, 1.59649e-07
    };
  double quadlmExpectedSolution[8] =
    {
      0, 0,
      -8.76093e-05, -0.0135944,
      -0.00420457, 0.00477804,
      -0.0163679, -0.0360446,
    };
  double trapezoidExpectedSolution[8] =
    {
      0, 0,
      0, 0,
      0, 0,
      0, 0
    };
  double tri2ExpectedSolution[8] =
    {
      0, 0,
      9.86667e-07, -2.028e-05,
      -9.76e-06, -5.67867e-05,
      -2.87733e-05, -9.68267e-05
    };
  double tri3ExpectedSolution[6] =
    {
      0, 0,
      0, 0,
      0, 0
    };
  double tri3eExpectedSolution[6] =
    {
      0, 0,
      0, 0,
      0, 0
    };
  double tri3qExpectedSolution[12] =
    {
      0, 0,
      -3.315e-07, 1.57527e-06,
      4.98323e-06, 7.36775e-07,
      -5.3625e-08, 2.18676e-06,
      8.32488e-07, 1.04065e-06,
      5.22113e-07, 2.42889e-06
    };
  double trussExpectedSolution[11] =
    {
      0, 0, -0.179399,
      0.00169764, -0.478397, 0,
      0.00339527, 0, 0.179399,
      0.392323, -0.505307
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
      Solver2DType::Pointer solver = Solver2DType::New();
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

      double      tolerance;
      if( modelFile == "quad2-small.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(quad2smallExpectedSolution[0]);
        }
      else if( modelFile == "quad2-strain.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(quad2strainExpectedSolution[0]);
        }
      else if( modelFile == "quad4.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(quad4ExpectedSolution[0]);
        }
      else if( modelFile == "quad6-grav.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(quad6gravExpectedSolution[0]);
        }
      else if( modelFile == "quad-lm.meta" )
        {
        tolerance = 10e-7;
        expectedSolution = &(quadlmExpectedSolution[0]);
        }
      else if( modelFile == "trapezoid.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(trapezoidExpectedSolution[0]);
        }
      else if( modelFile == "tri2.meta" )
        {
        tolerance = 10e-5;
        expectedSolution = &(tri2ExpectedSolution[0]);

        }
      else if( modelFile == "tri3.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(tri3ExpectedSolution[0]);
        }
      else if( modelFile == "tri3-e.meta" )
        {
        tolerance = 10e-10;
        expectedSolution = &(tri3eExpectedSolution[0]);
        }
      else if( modelFile == "tri3-q.meta" )
        {
        tolerance = 10e-9;
        expectedSolution = &(tri3qExpectedSolution[0]);
        }
      else if( modelFile == "truss.meta" )
        {
        tolerance = 10e-7;
        expectedSolution = &(trussExpectedSolution[0]);
        }
      else
        {
        tolerance = 0.0;
        std::cout << "WARNING: Unknown solution for this model, " << modelFile << std::endl;
        }

      PrintK1(solver, s);
      PrintF1(solver, s);
      PrintNodalCoordinates1(solver, s);

      // itkpack and VNLDense solvers are sensitive to slight numerical
      // instabilities on windows. Ignore results for this FE problem
      if ( ( modelFile == "tri3-q.meta" ) && ((s == 2) || (s == 1)) )
        {
        /* itpack does not correctly solve this problem */
        if (s== 1)
          {
          std::cout << "Ignore DenseVNL results for " << modelFile << std::endl;
          }
        else
          {
          std::cout << "Ignore itpack results for " << modelFile << std::endl;
          }
        }
      else
        {
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
    }
  catch( ::itk::ExceptionObject & err )
    {
    std::cerr << "ITK exception detected: "  << err;
    std::cout << "Test FAILED" << std::endl;

    return EXIT_FAILURE;
    }

  std::cout << std::endl << ">>>>>" << std::endl;
  if( foundError )
    {
    std::cout << "Overall Test : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Overall Test : [PASSED]" << std::endl;
  return EXIT_SUCCESS;
}

void PrintK1(Solver2DType *S, int s)
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

void PrintF1(Solver2DType *S, int s)
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

void PrintNodalCoordinates1(Solver2DType *S, int w)
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

bool CheckDisplacements1(Solver2DType *S, int s, double *expectedResults, double tolerance)
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
