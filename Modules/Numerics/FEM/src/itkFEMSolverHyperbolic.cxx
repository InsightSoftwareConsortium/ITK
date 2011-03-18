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
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMSolverHyperbolic.h"

namespace itk {
namespace fem {

SolverHyperbolic::SolverHyperbolic()
{
  this->InitializeLinearSystemWrapper();
  m_beta=0.25;
  m_gamma=0.5;
  m_deltaT=1.0;
}

void
SolverHyperbolic
::InitializeLinearSystemWrapper(void)
{
  // set the maximum number of matrices and vectors that
  // we will need to store inside.
  m_ls->SetNumberOfMatrices(5);
  m_ls->SetNumberOfVectors(6);
  m_ls->SetNumberOfSolutions(3);
}

void
SolverHyperbolic
::AssembleElementMatrix(Element::Pointer e)
{
  // Copy the element stiffness matrix for faster access.
  Element::MatrixType Ke;
  e->GetStiffnessMatrix(Ke);
  Element::MatrixType Me;
  e->GetMassMatrix(Me);

  // ... same for number of DOF
  int Ne=e->GetNumberOfDegreesOfFreedom();

  // step over all rows in element matrix
  for(int j=0; j<Ne; j++)
    {
    // step over all columns in element matrix
    for(int k=0; k<Ne; k++)
      {
      // error checking. all GFN should be =>0 and <NGFN
      if ( e->GetDegreeOfFreedom(j) >= NGFN ||
           e->GetDegreeOfFreedom(k) >= NGFN  )
        {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleElementMatrix()","Illegal GFN!");
        }

      /**
       * Here we finaly update the corresponding element
       * in the master stiffness matrix. We first check if
       * element in Ke is zero, to prevent zeros from being
       * allocated in sparse matrix.
       */
      if ( Ke[j][k]!=Float(0.0) )
        {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Ke[j][k], matrix_K );
        }
      if ( Me[j][k]!=Float(0.0) )
        {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Me[j][k], matrix_M );
        }

      }

    }

}

void
SolverHyperbolic
::InitializeMatrixForAssembly(unsigned int N)
{
  this->m_ls->SetSystemOrder(N);
  this->m_ls->InitializeMatrix();
  this->m_ls->InitializeMatrix(matrix_K);
  this->m_ls->InitializeMatrix(matrix_M);
  this->m_ls->InitializeMatrix(matrix_C);
  for(unsigned int i=0; i<N; i++)
    {
    m_ls->SetMatrixValue(i,i,1.0,matrix_C);
    }
}

void
SolverHyperbolic
::FinalizeMatrixAfterAssembly( void )
{
  // Apply the boundary conditions to the matrix
  // FIXME: this doesn't work in general
  this->ApplyBC(0,matrix_M);
  this->ApplyBC(0,matrix_K);

  // Calculate initial values of vector_a
  // M*a0=F - C*v0 - K*d0
  // FIXME: take into account the d0 and v0.
  m_ls->InitializeSolution(0);
  m_ls->InitializeSolution(solution_a);

  m_ls->CopyMatrix(matrix_M,0);
  this->AssembleF();
  m_ls->Solve();
  m_ls->InitializeVector(vector_tmp);
  m_ls->CopySolution2Vector(0,vector_tmp);
  m_ls->InitializeSolution(solution_a);
  m_ls->CopyVector2Solution(vector_tmp,solution_a);
  m_ls->DestroyVector(vector_tmp);

  m_ls->InitializeSolution(solution_d);
  m_ls->InitializeSolution(solution_v);

  // Compose the lhs of system of lin. eq.
  m_ls->InitializeMatrix(matrix_tmp);
  m_ls->CopyMatrix(matrix_C,matrix_tmp);
  m_ls->ScaleMatrix(this->m_gamma*this->m_deltaT, matrix_tmp);
  m_ls->AddMatrixMatrix(0,matrix_tmp);

  m_ls->CopyMatrix(matrix_K,matrix_tmp);
  m_ls->ScaleMatrix(this->m_beta*this->m_deltaT*this->m_deltaT, matrix_tmp);
  m_ls->AddMatrixMatrix(0,matrix_tmp);
  m_ls->DestroyMatrix(matrix_tmp);

}

void
SolverHyperbolic
::Solve()
{
  m_ls->InitializeVector(vector_tmp);
  m_ls->InitializeVector(vector_dhat);
  m_ls->InitializeVector(vector_vhat);
  m_ls->InitializeVector(vector_ahat);

  // We're using the Newmark method to obtain the solution

  // Assume that vectors solution_a solution_v and solution_d contain
  // solutions obtained at the previous time step.

  // Calculate the predictors
  for(unsigned int i=0; i<m_ls->GetSystemOrder(); i++)
    {
    Float d0=m_ls->GetSolutionValue(i,solution_d);
    Float v0=m_ls->GetSolutionValue(i,solution_v);
    Float a0=m_ls->GetSolutionValue(i,solution_a);
    m_ls->SetVectorValue( i, -(d0+this->m_deltaT*v0+0.5*this->m_deltaT*this->m_deltaT*(1.0-2.0*this->m_beta)*a0), vector_dhat);
    m_ls->SetVectorValue( i, -(v0+this->m_deltaT*(1.0-this->m_gamma)*a0), vector_vhat);
    }

  // Calculate the rhs of master equation
  m_ls->MultiplyMatrixVector(vector_tmp,matrix_C,vector_vhat);
  m_ls->AddVectorVector(0,vector_tmp);
  m_ls->MultiplyMatrixVector(vector_tmp,matrix_K,vector_dhat);
  m_ls->AddVectorVector(0,vector_tmp);

  // Solve the system of linear equations for accelerations
  m_ls->Solve();

  // move the solution for a to the correct vector
  m_ls->CopySolution2Vector(0,vector_tmp);
  m_ls->CopyVector2Solution(vector_tmp,solution_a);

  // Calculate displacements and velocities
  for(unsigned int i=0; i<m_ls->GetSystemOrder(); i++)
    {
    Float dhat=-m_ls->GetVectorValue(i,vector_dhat);
    Float vhat=-m_ls->GetVectorValue(i,vector_vhat);
    Float a1=m_ls->GetSolutionValue(i,solution_a);

    m_ls->SetSolutionValue(i, dhat +
                           this->m_beta*this->m_deltaT*this->m_deltaT*a1
                           , solution_d);

    m_ls->SetSolutionValue(i, vhat +
                           this->m_gamma*this->m_deltaT*a1
                           , solution_v);
    }

  m_ls->DestroyVector(vector_tmp);
  m_ls->DestroyVector(vector_dhat);
  m_ls->DestroyVector(vector_vhat);
  m_ls->DestroyVector(vector_ahat);

}

}} // end namespace itk::fem
