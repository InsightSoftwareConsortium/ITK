/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFEMSolverHyperbolic.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMLinearSystemWrapperItpack.h"
#include "itkTestingMacros.h"


using FEMSolverType = itk::fem::SolverHyperbolic<2>;


// Print K - the global stiffness matrix
void
PrintK(FEMSolverType * S)
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

  std::cout << '\n' << 'k' << "=[";
  for (unsigned int j = 0; j < lsw->GetSystemOrder(); ++j)
  {
    std::cout << " [";
    for (unsigned int k = 0; k < lsw->GetSystemOrder(); ++k)
    {
      std::cout << lsw->GetMatrixValue(j, k);
      if ((k + 1) < lsw->GetSystemOrder())
      {
        std::cout << ", ";
      }
    }
    if (j < lsw->GetSystemOrder() - 1)
    {
      std::cout << " ]," << '\n';
    }
    else
    {
      std::cout << ']';
    }
  }
  std::cout << "];" << '\n';
}

// Print F - the global load vector
void
PrintF(FEMSolverType * S)
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

  std::cout << '\n' << 'f' << "=[";
  for (unsigned int j = 0; j < lsw->GetSystemOrder(); ++j)
  {
    if (j > 0)
    {
      std::cout << ",  ";
    }
    std::cout << lsw->GetVectorValue(j);
  }
  std::cout << "];" << '\n';
}

void
PrintNodalCoordinates(FEMSolverType * S)
// Print the nodal coordinates
{
  std::cout << '\n' << "Nodal coordinates: " << '\n';

  std::cout << "xyz=[";

  int numberOfNodes = S->GetInput()->GetNumberOfNodes();
  for (int i = 0; i < numberOfNodes; ++i)
  {
    std::cout << " [";
    std::cout << S->GetInput()->GetNode(i)->GetCoordinates();
    std::cout << ']';
  }
  std::cout << "];" << '\n';
}


// Useful for display purposes - lets you draw each element
// individually, instead of just a stream of nodes
void
PrintElementCoordinates(FEMSolverType * S)
{
  std::cout << '\n' << "Element coordinates: " << '\n';
  int ct = 1;

  const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;

  int numberOfElements = S->GetInput()->GetNumberOfElements();

  for (int i = 0; i < numberOfElements; ++i)
  {
    std::cout << "e(" << ct << ",:,:)=[";

    for (unsigned int n = 0; n < S->GetInput()->GetElement(i)->GetNumberOfNodes(); ++n)
    {
      itk::fem::Element::VectorType nc = S->GetInput()->GetElement(i)->GetNodeCoordinates(n);

      for (unsigned int d = 0, dof;
           (dof = S->GetInput()->GetElement(i)->GetNode(n)->GetDegreeOfFreedom(d)) != invalidID;
           d++)
      {
        nc[d] += S->GetSolution(dof);
      }
      std::cout << nc << '\n';
    }
    std::cout << "];" << '\n';
    ct++;
  }
}

// Useful for display purposes - lets you draw each element
// individually, instead of just a stream of nodes
void
PrintSolution(FEMSolverType * S)
{
  std::cout << '\n' << "Solution: " << '\n';
  const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;
  int                numberOfNodes = S->GetInput()->GetNumberOfNodes();

  for (int i = 0; i < numberOfNodes; ++i)
  {
    std::cout << "Solution Node " << i << ':';
    for (unsigned int d = 0, dof; (dof = S->GetInput()->GetNode(i)->GetDegreeOfFreedom(d)) != invalidID; ++d)
    {
      std::cout << ' ' << S->GetSolution(dof);
    }
    std::cout << '\n';
  }
}

int
itkFEMSolverHyperbolicTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cout << " inputFileName iterations lsw (0=VNL, 1=Dense VNL, 2=Itpack)" << '\n';
    return EXIT_FAILURE;
  }

  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  unsigned int        niter = std::stoi(argv[2]);
  unsigned int        w = std::stoi(argv[3]);
  std::vector<double> solution;
  if (argc > 4)
  {
    solution.resize(argc - 4);
    for (int i = 4; i < argc; ++i)
    {
      solution[i - 4] = std::stod(argv[i]);
    }
  }

  using FEMSpatialObjectReaderType = itk::FEMSpatialObjectReader<2>;
  using FEMSpatialObjectReaderPointer = FEMSpatialObjectReaderType::Pointer;
  FEMSpatialObjectReaderPointer spatialReader = FEMSpatialObjectReaderType::New();
  spatialReader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(spatialReader->Update());


  using FEMObjectSpatialObjectType = itk::FEMObjectSpatialObject<2>;
  FEMObjectSpatialObjectType::ChildrenListType * children = spatialReader->GetGroup()->GetChildren();
  FEMObjectSpatialObjectType::Pointer            femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>((*(children->begin())).GetPointer());
  if (!femSO)
  {
    std::cout << " dynamic_cast [FAILED]" << '\n';
    return EXIT_FAILURE;
  }
  delete children;

  femSO->GetFEMObject()->FinalizeMesh();

  // Third, create the FEM solver object and generate the solution

  auto SH = FEMSolverType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(SH, SolverHyperbolic, Solver);


  SH->SetInput(femSO->GetFEMObject());
  ITK_TEST_SET_GET_VALUE(femSO->GetFEMObject(), SH->GetInput());

  FEMSolverType::Float gamma = 0.5;
  SH->SetGamma(gamma);
  ITK_TEST_SET_GET_VALUE(gamma, SH->GetGamma());

  FEMSolverType::Float beta = 0.25;
  SH->SetBeta(beta);
  ITK_TEST_SET_GET_VALUE(beta, SH->GetBeta());

  SH->SetNumberOfIterations(niter);
  ITK_TEST_SET_GET_VALUE(niter, SH->GetNumberOfIterations());

  FEMSolverType::Float timeStep = 0.5;
  SH->SetTimeStep(timeStep);
  ITK_TEST_SET_GET_VALUE(timeStep, SH->GetTimeStep());


  itk::fem::LinearSystemWrapperDenseVNL lsw_dvnl;
  itk::fem::LinearSystemWrapperItpack   lsw_itpack;
  itk::fem::LinearSystemWrapperVNL      lsw_vnl;

  switch (w)
  {
    case 0:
      // VNL
      std::cout << '\n' << "Using LinearSystemWrapperVNL" << '\n';
      SH->SetLinearSystemWrapper(&lsw_vnl);
      break;
    case 1:
      // Dense VNL
      std::cout << '\n' << "Using LinearSystemWrapperDenseVNL" << '\n';
      SH->SetLinearSystemWrapper(&lsw_dvnl);
      break;
    case 2:
      // IT Pack
      std::cout << '\n' << "Using LinearSystemWrapperItpack" << '\n';
      SH->SetLinearSystemWrapper(&lsw_itpack);
      break;
    default:
      // Sparse VNL - default
      std::cout << '\n' << "Using LinearSystemWrapperVNL" << '\n';
      SH->SetLinearSystemWrapper(&lsw_vnl);
      break;
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(SH->Update());


  PrintK(SH);
  PrintF(SH);
  PrintNodalCoordinates(SH);
  PrintSolution(SH);

  if (argc > 4)
  {
    int                numberOfNodes = SH->GetInput()->GetNumberOfNodes();
    const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;
    for (int i = 0; i < numberOfNodes; ++i)
    {
      for (unsigned int d = 0, dof; (dof = SH->GetInput()->GetNode(i)->GetDegreeOfFreedom(d)) != invalidID; ++d)
      {
        double result = SH->GetSolution(dof);
        if (itk::Math::abs(result - solution[dof]) > 1.0e-5)
        {
          std::cerr << "Error: Solution outside the expected range: " << result << ", " << dof << '\n';
          return EXIT_FAILURE;
        }
      }
    }
  }


  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
