/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkFEMSolverHyperbolic.cxx,v $
  Language:  C++
  Date:      $Date: 2009-01-30 21:53:03 $
  Version:   $Revision: 1.7 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
  // set the maximum number of matrices and vectors that
  // we will need to store inside.
  this->m_ls->SetNumberOfMatrices(5);
  this->m_ls->SetNumberOfVectors(6);
  this->m_ls->SetNumberOfSolutions(3);
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

  // step over all rows in element matrix
  for(int j=0; j<Ne; j++)
    {
    // step over all columns in element matrix
    for(int k=0; k<Ne; k++)
      {
      // error checking. all GFN should be =>0 and <NGFN
      if ( e->GetDegreeOfFreedom(j) >= this->GetInput()->GetNumberOfDegreesOfFreedom() ||
           e->GetDegreeOfFreedom(k) >= this->GetInput()->GetNumberOfDegreesOfFreedom()  )
        {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleElementMatrix()","Illegal GFN!");
        }

      /**
       * Here we finally update the corresponding element
       * in the master stiffness matrix. We first check if
       * element in Ke is zero, to prevent zeros from being
       * allocated in sparse matrix.
       */
      if ( Math::NotExactlyEquals(Ke[j][k],Float(0.0)) )
        {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Ke[j][k], matrix_K );
        }
      if ( Math::NotExactlyEquals(Me[j][k],Float(0.0)) )
        {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Me[j][k], matrix_M );
        }
      }
    }

}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::InitializeMatrixForAssembly(unsigned int N)
{
  this->m_ls->SetSystemOrder(N);
  this->m_ls->InitializeMatrix();
  this->m_ls->InitializeMatrix(matrix_K);
  this->m_ls->InitializeMatrix(matrix_M);
  this->m_ls->InitializeMatrix(matrix_C);
  for(unsigned int i=0; i<N; i++)
    {
    this->m_ls->SetMatrixValue(i,i,1.0,matrix_C);
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
  this->m_ls->InitializeSolution(0);
  this->m_ls->InitializeSolution(solution_a);

  this->m_ls->CopyMatrix(matrix_M,0);
  this->AssembleF();
  this->m_ls->Solve();
  this->m_ls->InitializeVector(vector_tmp);
  this->m_ls->CopySolution2Vector(0,vector_tmp);
  this->m_ls->InitializeSolution(solution_a);
  this->m_ls->CopyVector2Solution(vector_tmp,solution_a);
  this->m_ls->DestroyVector(vector_tmp);

  this->m_ls->InitializeSolution(solution_d);
  this->m_ls->InitializeSolution(solution_v);

  // Compose the lhs of system of lin. eq.
  this->m_ls->InitializeMatrix(matrix_tmp);
  this->m_ls->CopyMatrix(matrix_C,matrix_tmp);
  this->m_ls->ScaleMatrix(this->m_Gamma*this->m_TimeStep, matrix_tmp);
  this->m_ls->AddMatrixMatrix(0,matrix_tmp);

  this->m_ls->CopyMatrix(matrix_K,matrix_tmp);
  this->m_ls->ScaleMatrix(this->m_Beta*this->m_TimeStep*this->m_TimeStep, matrix_tmp);
  this->m_ls->AddMatrixMatrix(0,matrix_tmp);
  this->m_ls->DestroyMatrix(matrix_tmp);
}

template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::Solve()
{
  this->m_ls->InitializeVector(vector_tmp);
  this->m_ls->InitializeVector(vector_dhat);
  this->m_ls->InitializeVector(vector_vhat);
  this->m_ls->InitializeVector(vector_ahat);

  // We're using the Newmark method to obtain the solution
  // Assume that vectors solution_a solution_v and solution_d contain
  // solutions obtained at the previous time step.

  // Calculate the predictors
  for(unsigned int i=0; i<this->m_ls->GetSystemOrder(); i++)
    {
    Float d0=this->m_ls->GetSolutionValue(i,solution_d);
    Float v0=this->m_ls->GetSolutionValue(i,solution_v);
    Float a0=this->m_ls->GetSolutionValue(i,solution_a);
    this->m_ls->SetVectorValue( i, -(d0+this->m_TimeStep*v0+0.5*this->m_TimeStep*this->m_TimeStep*(1.0-2.0*this->m_Beta)*a0), vector_dhat);
    this->m_ls->SetVectorValue( i, -(v0+this->m_TimeStep*(1.0-this->m_Gamma)*a0), vector_vhat);
    }

  // Calculate the rhs of master equation
  this->m_ls->MultiplyMatrixVector(vector_tmp,matrix_C,vector_vhat);
  this->m_ls->AddVectorVector(0,vector_tmp);
  this->m_ls->MultiplyMatrixVector(vector_tmp,matrix_K,vector_dhat);
  this->m_ls->AddVectorVector(0,vector_tmp);

  // Solve the system of linear equations for accelerations
  this->m_ls->Solve();

  // move the solution for a to the correct vector
  this->m_ls->CopySolution2Vector(0,vector_tmp);
  this->m_ls->CopyVector2Solution(vector_tmp,solution_a);

  // Calculate displacements and velocities
  for(unsigned int i=0; i<this->m_ls->GetSystemOrder(); i++)
    {
    Float dhat=-this->m_ls->GetVectorValue(i,vector_dhat);
    Float vhat=-this->m_ls->GetVectorValue(i,vector_vhat);
    Float a1=this->m_ls->GetSolutionValue(i,solution_a);

    this->m_ls->SetSolutionValue(i, dhat +
                           this->m_Beta*this->m_TimeStep*this->m_TimeStep*a1
                           , solution_d);

    this->m_ls->SetSolutionValue(i, vhat +
                           this->m_Gamma*this->m_TimeStep*a1
                           , solution_v);
    }

  this->m_ls->DestroyVector(vector_tmp);
  this->m_ls->DestroyVector(vector_dhat);
  this->m_ls->DestroyVector(vector_vhat);
  this->m_ls->DestroyVector(vector_ahat);

}

// ----------------------------------------------------------------------------
template <unsigned int VDimension>
void
SolverHyperbolic<VDimension>
::GenerateData()
{
  /* Call Solver */
  this->RunSolver();
}

/**
 * Solve for the displacement vector u
 */
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

/**
 * PrintSelf
 */
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
