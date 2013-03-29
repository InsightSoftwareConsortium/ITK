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

#include <iostream>

//  Example taken from 'Fundamentals of the Finite ELement Method' - Grandin
int itkFEMElement2DC0LinearQuadrilateralStrainItpackTest(int argc, char *argv[])
{
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  typedef itk::fem::Solver<2> SolverType;
  typedef SolverType *        SolverPointerType;
  SolverPointerType m_Solver = new SolverType;
  // std::ifstream     fileInput;

  itk::fem::LinearSystemWrapperItpack WrapperItpack;
  WrapperItpack.SetMaximumNonZeroValuesInMatrix(100);

  // FIXME
  // fileInput.open("C:/Research/ITKGit/ITK/Testing/Data/Input/FEM/2DC0LinearQuadrilateralStrainTest.fem");
  // m_Solver->Read(fileInput);
  // m_Solver->GenerateGFN();
  // m_Solver->SetLinearSystemWrapper(&WrapperItpack);
  // m_Solver->AssembleK();
  // m_Solver->DecomposeK();
  // m_Solver->AssembleF();
  // m_Solver->Solve();
  return EXIT_FAILURE;
  // float soln[8];
  // for( int i = 0; i < 8; i++ )
  //   {
  //   soln[i] = m_Solver->GetSolution(i);
  //   }

  // std::cout << "Test PASSED!" << std::endl;
  // return EXIT_SUCCESS;
}
