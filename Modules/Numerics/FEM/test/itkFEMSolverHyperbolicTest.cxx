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

#include "itkFEMSolverHyperbolic.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMLinearSystemWrapperItpack.h"


typedef itk::fem::SolverHyperbolic<2> FEMSolverType;


// Print K - the global stiffness matrix
void PrintK(FEMSolverType *S)
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

  std::cout << std::endl << "k" << "=[";
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

// Print F - the global load vector
void PrintF(FEMSolverType *S)
{
  itk::fem::LinearSystemWrapper::Pointer lsw = S->GetLinearSystemWrapper();

  std::cout << std::endl << "f" << "=[";
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

void PrintNodalCoordinates(FEMSolverType *S)
// Print the nodal coordinates
{
  std::cout << std::endl << "Nodal coordinates: " << std::endl;

  std::cout << "xyz" << "=[";

  int numberOfNodes = S->GetInput()->GetNumberOfNodes();
  for( int i = 0; i < numberOfNodes; i++ )
    {
    std::cout << " [";
    std::cout << S->GetInput()->GetNode(i)->GetCoordinates();
    std::cout << "]";
    }
  std::cout << "];" << std::endl;
}


// Useful for display purposes - lets you draw each element
// individually, instead of just a stream of nodes
void PrintElementCoordinates(FEMSolverType *S )
{
  std::cout << std::endl << "Element coordinates: " << std::endl;
  int ct = 1;

  const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;

  int numberOfElements = S->GetInput()->GetNumberOfElements();

  for(int i = 0; i < numberOfElements; i++ )
    {
    std::cout << "e(" << ct << ",:,:)=[";

    for (unsigned int n=0; n < S->GetInput()->GetElement(i)->GetNumberOfNodes(); n++)
      {
      itk::fem::Element::VectorType nc = S->GetInput()->GetElement(i)->GetNodeCoordinates(n);

      for (unsigned int d=0, dof; ( dof = S->GetInput()->GetElement(i)->GetNode(n)->GetDegreeOfFreedom(d) ) != invalidID; d++)
        {
        nc[d] += S->GetSolution( dof );
        }
      std::cout << nc << std::endl;
      }
    std::cout << "];" << std::endl;
    ct++;
  }
}

// Useful for display purposes - lets you draw each element
// individually, instead of just a stream of nodes
void PrintSolution(FEMSolverType *S )
{
  std::cout << std::endl << "Solution: " << std::endl;
  const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;
  int numberOfNodes = S->GetInput()->GetNumberOfNodes();

  for (int i = 0; i < numberOfNodes; i++ )
    {
    std::cout << "Solution Node " << i << ":";
    for (unsigned int d=0, dof; ( dof = S->GetInput()->GetNode(i)->GetDegreeOfFreedom(d) ) != invalidID; d++)
      {
        std::cout << " " << S->GetSolution( dof );
      }
      std::cout << std::endl;
    }
}

int itkFEMSolverHyperbolicTest(int ac, char* av[])
{

  if (ac < 4)
    {
    std::cout << "Usage: " << av[0];
    std::cout << " input-file iterations lsw (0=VNL, 1=Dense VNL, 2=Itpack)";
    std::cout << std::endl;
    return EXIT_FAILURE;
    }

  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  unsigned int niter = atoi ( av[2] );
  unsigned int w = atoi( av[3] );
  std::vector<double> solution;
  if (ac > 4)
    {
    solution.resize( ac - 4 );
    for (int i=4;i<ac;i++)
      {
      solution[i-4] = atof(av[i]);
      }
    }

  typedef itk::FEMSpatialObjectReader<2>      FEMSpatialObjectReaderType;
  typedef FEMSpatialObjectReaderType::Pointer FEMSpatialObjectReaderPointer;
  FEMSpatialObjectReaderPointer SpatialReader = FEMSpatialObjectReaderType::New();
  SpatialReader->SetFileName( av[1] );
  try
    {
    SpatialReader->Update();
    }
  catch (::itk::fem::FEMException & e)
    {
    std::cout<<"Error reading FEM problem: "<< av[1] <<"!\n";
    e.Print(std::cout);
    return EXIT_FAILURE;
    }

  typedef itk::FEMObjectSpatialObject<2>      FEMObjectSpatialObjectType;
  FEMObjectSpatialObjectType::ChildrenListType* children = SpatialReader->GetGroup()->GetChildren();
  FEMObjectSpatialObjectType::Pointer femSO =
    dynamic_cast<FEMObjectSpatialObjectType *>( (*(children->begin() ) ).GetPointer() );
  if (!femSO)
    {
    std::cout << " dynamic_cast [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  delete children;

  femSO->GetFEMObject()->FinalizeMesh();

  /**
   * Third, create the FEM solver object and generate the solution
   */

  FEMSolverType::Pointer SH = FEMSolverType::New();
  SH->SetInput( femSO->GetFEMObject() );
  SH->SetTimeStep( .5 );
  SH->SetNumberOfIterations( niter );

  itk::fem::LinearSystemWrapperDenseVNL lsw_dvnl;
  itk::fem::LinearSystemWrapperItpack   lsw_itpack;
  itk::fem::LinearSystemWrapperVNL      lsw_vnl;

  switch (w)
    {
    case 0:
      // VNL
      std::cout << std::endl << ">>>>>Using LinearSystemWrapperVNL" << std::endl;
      SH->SetLinearSystemWrapper(&lsw_vnl);
      break;
    case 1:
      // Dense VNL
      std::cout << std::endl << ">>>>>Using LinearSystemWrapperDenseVNL" << std::endl;
      SH->SetLinearSystemWrapper(&lsw_dvnl);
      break;
    case 2:
      // IT Pack
      std::cout << std::endl << ">>>>>Using LinearSystemWrapperItpack" << std::endl;
      SH->SetLinearSystemWrapper(&lsw_itpack);
      break;
    default:
      // Sparse VNL - default
      std::cout << std::endl << ">>>>>Using LinearSystemWrapperVNL" << std::endl;
      SH->SetLinearSystemWrapper(&lsw_vnl);
      break;
    }

  try
    {
    SH->Update();
    }
  catch (itk::ExceptionObject &err)
    {
    std::cerr << "ITK exception detected: "  << err;
    return EXIT_FAILURE;
    }

  PrintK( SH );
  PrintF( SH );
  PrintNodalCoordinates( SH );
  PrintSolution( SH );

  if (ac > 4)
    {
    int numberOfNodes = SH->GetInput()->GetNumberOfNodes();
    const unsigned int invalidID = itk::fem::Element::InvalidDegreeOfFreedomID;
    for (int i = 0; i < numberOfNodes; i++ )
      {
      for (unsigned int d=0, dof; ( dof = SH->GetInput()->GetNode(i)->GetDegreeOfFreedom(d) ) != invalidID; d++)
        {
        double result = SH->GetSolution( dof );
        if (fabs(result-solution[dof]) > 1.0e-5)
          {
          std::cerr << "Error: Solution outside the expected range: "  << result << ", " << dof << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }
  return EXIT_SUCCESS;
}
