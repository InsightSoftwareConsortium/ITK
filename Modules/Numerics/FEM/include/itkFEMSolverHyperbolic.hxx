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

#ifndef itkFEMSolverHyperbolic_hxx
#define itkFEMSolverHyperbolic_hxx

#include "itkFEMSolverHyperbolic.h"
#include "itkMath.h"

namespace itk {
namespace fem {

template <unsigned int VDimension>
SolverHyperbolic<VDimension>
::SolverHyperbolic()
{
  this->InitializeLinearSystemWrapper();
  this->InitializeMatrixForAssembly( 2 );
  this->m_Beta=0.25;
  this->m_Gamma=0.5;
  this->m_TimeStep=1.0;
  this->m_NumberOfIterations = 1;
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::InitializeLinearSystemWrapper(void)
{
  // Set the maximum number of matrices and vectors that
  // we will need to store inside.
  this->m_LinearSystem->SetNumberOfMatrices(5);
  this->m_LinearSystem->SetNumberOfVectors(6);
  this->m_LinearSystem->SetNumberOfSolutions(3);
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::AssembleElementMatrix(Element::Pointer e)
{
  // Copy the element stiffness matrix for faster access.
  Element::MatrixType Ke;
  e->GetStiffnessMatrix(Ke);
  Element::MatrixType Me;
  e->GetMassMatrix(Me);

  // ... same for number of DOF
  int Ne=e->GetNumberOfDegreesOfFreedom();

  // Step over all rows in element matrix
  for(int j=0; j<Ne; j++)
    {
    // Step over all columns in element matrix
    for(int k=0; k<Ne; k++)
      {
      // error checking. all GFN should be =>0 and <NGFN
      if ( e->GetDegreeOfFreedom(j) >= this->GetInput()->GetNumberOfDegreesOfFreedom() ||
           e->GetDegreeOfFreedom(k) >= this->GetInput()->GetNumberOfDegreesOfFreedom()  )
        {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleElementMatrix()","Illegal GFN!");
        }

      //
      // Here we finally update the corresponding element
      // in the master stiffness matrix. We first check if
      // element in Ke is zero, to prevent zeros from being
      // allocated in sparse matrix.
      //
      if ( Math::NotExactlyEquals(Ke[j][k],Float(0.0)) )
        {
        this->m_LinearSystem->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Ke[j][k], matrix_K );
        }
      if ( Math::NotExactlyEquals(Me[j][k],Float(0.0)) )
        {
        this->m_LinearSystem->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Me[j][k], matrix_M );
        }
      }
    }

}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::InitializeMatrixForAssembly(unsigned int N)
{
  this->m_LinearSystem->SetSystemOrder(N);
  this->m_LinearSystem->InitializeMatrix();
  this->m_LinearSystem->InitializeMatrix(matrix_K);
  this->m_LinearSystem->InitializeMatrix(matrix_M);
  this->m_LinearSystem->InitializeMatrix(matrix_C);
  for(unsigned int i=0; i<N; i++)
    {
    this->m_LinearSystem->SetMatrixValue(i,i,1.0,matrix_C);
    }
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::FinalizeMatrixAfterAssembly( void )
{
  // Apply the boundary conditions to the matrix
  // FIXME: this doesn't work in general
  this->ApplyBC(0,matrix_M);
  this->ApplyBC(0,matrix_K);

  // Calculate initial values of vector_a
  // M*a0=F - C*v0 - K*d0
  // FIXME: take into account the d0 and v0.
  this->m_LinearSystem->InitializeSolution(0);
  this->m_LinearSystem->InitializeSolution(solution_a);

  this->m_LinearSystem->CopyMatrix(matrix_M,0);
  this->AssembleF();
  this->m_LinearSystem->Solve();
  this->m_LinearSystem->InitializeVector(vector_tmp);
  this->m_LinearSystem->CopySolution2Vector(0,vector_tmp);
  this->m_LinearSystem->InitializeSolution(solution_a);
  this->m_LinearSystem->CopyVector2Solution(vector_tmp,solution_a);
  this->m_LinearSystem->DestroyVector(vector_tmp);

  this->m_LinearSystem->InitializeSolution(solution_d);
  this->m_LinearSystem->InitializeSolution(solution_v);

  // Compose the lhs of system of lin. eq.
  this->m_LinearSystem->InitializeMatrix(matrix_tmp);
  this->m_LinearSystem->CopyMatrix(matrix_C,matrix_tmp);
  this->m_LinearSystem->ScaleMatrix(this->m_Gamma*this->m_TimeStep, matrix_tmp);
  this->m_LinearSystem->AddMatrixMatrix(0,matrix_tmp);

  this->m_LinearSystem->CopyMatrix(matrix_K,matrix_tmp);
  this->m_LinearSystem->ScaleMatrix(this->m_Beta*this->m_TimeStep*this->m_TimeStep, matrix_tmp);
  this->m_LinearSystem->AddMatrixMatrix(0,matrix_tmp);
  this->m_LinearSystem->DestroyMatrix(matrix_tmp);
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::Solve()
{
  this->m_LinearSystem->InitializeVector(vector_tmp);
  this->m_LinearSystem->InitializeVector(vector_dhat);
  this->m_LinearSystem->InitializeVector(vector_vhat);
  this->m_LinearSystem->InitializeVector(vector_ahat);

  // We're using the Newmark method to obtain the solution
  // Assume that vectors solution_a solution_v and solution_d contain
  // solutions obtained at the previous time step.

  // Calculate the predictors
  for(unsigned int i=0; i<this->m_LinearSystem->GetSystemOrder(); i++)
    {
    Float d0=this->m_LinearSystem->GetSolutionValue(i,solution_d);
    Float v0=this->m_LinearSystem->GetSolutionValue(i,solution_v);
    Float a0=this->m_LinearSystem->GetSolutionValue(i,solution_a);
    this->m_LinearSystem->SetVectorValue( i, -(d0+this->m_TimeStep*v0+0.5*this->m_TimeStep*this->m_TimeStep*(1.0-2.0*this->m_Beta)*a0), vector_dhat);
    this->m_LinearSystem->SetVectorValue( i, -(v0+this->m_TimeStep*(1.0-this->m_Gamma)*a0), vector_vhat);
    }

  // Calculate the rhs of master equation
  this->m_LinearSystem->MultiplyMatrixVector(vector_tmp,matrix_C,vector_vhat);
  this->m_LinearSystem->AddVectorVector(0,vector_tmp);
  this->m_LinearSystem->MultiplyMatrixVector(vector_tmp,matrix_K,vector_dhat);
  this->m_LinearSystem->AddVectorVector(0,vector_tmp);

  // Solve the system of linear equations for accelerations
  this->m_LinearSystem->Solve();

  // move the solution for a to the correct vector
  this->m_LinearSystem->CopySolution2Vector(0,vector_tmp);
  this->m_LinearSystem->CopyVector2Solution(vector_tmp,solution_a);

  // Calculate displacements and velocities
  for(unsigned int i=0; i<this->m_LinearSystem->GetSystemOrder(); i++)
    {
    Float dhat=-this->m_LinearSystem->GetVectorValue(i,vector_dhat);
    Float vhat=-this->m_LinearSystem->GetVectorValue(i,vector_vhat);
    Float a1=this->m_LinearSystem->GetSolutionValue(i,solution_a);

    this->m_LinearSystem->SetSolutionValue(i, dhat +
                           this->m_Beta*this->m_TimeStep*this->m_TimeStep*a1
                           , solution_d);

    this->m_LinearSystem->SetSolutionValue(i, vhat +
                           this->m_Gamma*this->m_TimeStep*a1
                           , solution_v);
    }

  this->m_LinearSystem->DestroyVector(vector_tmp);
  this->m_LinearSystem->DestroyVector(vector_dhat);
  this->m_LinearSystem->DestroyVector(vector_vhat);
  this->m_LinearSystem->DestroyVector(vector_ahat);

}

// ----------------------------------------------------------------------------
template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::GenerateData()
{
  // Call Solver
  this->RunSolver();
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::RunSolver()
{
  this->AssembleK();            // Assemble the global stiffness matrix K
  this->DecomposeK();           // Invert K

  for (unsigned int nit = 0; nit < m_NumberOfIterations; nit++)
    {
    this->AssembleF();
    this->Solve();
    }
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Number Of Iterations: " << this->m_NumberOfIterations << std::endl;
  os << indent << "Time Step: " << this->m_TimeStep << std::endl;
  os << indent << "Beta: " << this->m_Beta << std::endl;
  os << indent << "Gamma: " << this->m_Gamma << std::endl;
}

} // end namespace fem
} // end namespace itk

#endif // itkFEMSolverHyperbolic_hxx
