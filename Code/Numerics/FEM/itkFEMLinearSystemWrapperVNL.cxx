/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperVNL.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkFEMLinearSystemWrapperVNL.h"

namespace itk {
namespace fem {

void LinearSystemWrapperVNL::InitializeMatrix(unsigned int MatrixIndex)
{

  // allocate if necessary
  if (m_Matrices == NULL)
  {
    m_Matrices = new std::vector< vnl_sparse_matrix<Float>* >(m_NumberOfMatrices);
  }

  // out with old, in with new
  delete (*m_Matrices)[MatrixIndex];
  (*m_Matrices)[MatrixIndex] = new vnl_sparse_matrix<Float>(this->GetSystemOrder(), this->GetSystemOrder() );

}


void LinearSystemWrapperVNL::DestroyMatrix(unsigned int MatrixIndex)
{
  if (m_Matrices == NULL) return;
  delete (*m_Matrices)[MatrixIndex];
}


void LinearSystemWrapperVNL::InitializeVector(unsigned int VectorIndex)
{

  // allocate if necessary
  if (m_Vectors == NULL)
  {
    m_Vectors = new std::vector< vnl_vector<Float>* >(m_NumberOfVectors);
  }

  // out with old, in with new
  delete (*m_Vectors)[VectorIndex];
  (*m_Vectors)[VectorIndex] = new vnl_vector<Float>(this->GetSystemOrder());
  (*m_Vectors)[VectorIndex]->fill(0.0);

}


void LinearSystemWrapperVNL::DestroyVector(unsigned int VectorIndex)
{
  if (m_Vectors == NULL) return;
  delete (*m_Vectors)[VectorIndex];
}


void LinearSystemWrapperVNL::InitializeSolution(unsigned int SolutionIndex)
{
  // allocate if necessary
  if (m_Solutions == NULL)
  {
    m_Solutions = new std::vector< vnl_vector<Float>* >(m_NumberOfSolutions);
  }

  // out with old, in with new
  delete (*m_Solutions)[SolutionIndex];
  (*m_Solutions)[SolutionIndex] = new vnl_vector<Float>(this->GetSystemOrder());
  (*m_Solutions)[SolutionIndex]->fill(0.0);
}


void LinearSystemWrapperVNL::DestroySolution(unsigned int SolutionIndex)
{
  if (m_Solutions == NULL) return;
  delete (*m_Solutions)[SolutionIndex];
}


LinearSystemWrapperVNL::Float LinearSystemWrapperVNL::GetSolutionValue(unsigned int i, unsigned int SolutionIndex) const
{
  if ( ((*m_Solutions)[SolutionIndex])->size() <= i) return 0.0;
  else return (*((*m_Solutions)[SolutionIndex]))(i);
}


void LinearSystemWrapperVNL::Solve(void)
{

  if( (m_Matrices->size() == 0) || (m_Vectors->size() == 0) || (m_Solutions->size() == 0) ) throw;

  /* use functions to make sure that zero based matrix, vector, & index store final system to solve */
  if (m_PrimaryMatrixSetupFunction != NULL) (*m_PrimaryMatrixSetupFunction)(static_cast<SuperClass*>(this));
  if (m_PrimaryVectorSetupFunction != NULL) (*m_PrimaryVectorSetupFunction)(static_cast<SuperClass*>(this));
  if (m_PrimarySolutionSetupFunction != NULL) (*m_PrimarySolutionSetupFunction)(static_cast<SuperClass*>(this));

  /*
   * Solve the sparse system of linear equation and store the result in m_Solutions(0).
   * Here we use the iterative least squares solver.
   */
  vnl_sparse_matrix_linear_system<Float> ls( (*((*m_Matrices)[0])), (*((*m_Vectors)[0])) );
  vnl_lsqr lsq(ls);

  /*
   * Set max number of iterations to 3*size of the K matrix.
   * FIXME: There should be a better way to determine the number of iterations needed.
   */
  lsq.set_max_iterations(3*this->GetSystemOrder());
  lsq.minimize(*((*m_Solutions)[0]));

}


void  LinearSystemWrapperVNL::SwapMatrices(unsigned int MatrixIndex1, unsigned int MatrixIndex2)
{
  vnl_sparse_matrix<Float> *tmp;
  tmp = (*m_Matrices)[MatrixIndex1];
  (*m_Matrices)[MatrixIndex1] = (*m_Matrices)[MatrixIndex2];
  (*m_Matrices)[MatrixIndex2] = tmp;
}


void  LinearSystemWrapperVNL::SwapVectors(unsigned int VectorIndex1, unsigned int VectorIndex2)
{
  vnl_vector<Float> *tmp;
  tmp = (*m_Vectors)[VectorIndex1];
  (*m_Vectors)[VectorIndex1] = (*m_Vectors)[VectorIndex2];
  (*m_Vectors)[VectorIndex2] = tmp;
}


void  LinearSystemWrapperVNL::SwapSolutions(unsigned int SolutionIndex1, unsigned int SolutionIndex2)
{
  vnl_vector<Float> *tmp;
  tmp = (*m_Solutions)[SolutionIndex1];
  (*m_Solutions)[SolutionIndex1] = (*m_Solutions)[SolutionIndex2];
  (*m_Solutions)[SolutionIndex2] = tmp;
}


void LinearSystemWrapperVNL::Solution2Vector(unsigned int SolutionIndex, unsigned int VectorIndex)
{
  delete (*m_Vectors)[VectorIndex];
  (*m_Vectors)[VectorIndex] = new vnl_vector<Float>( *((*m_Solutions)[SolutionIndex]) );
}


void LinearSystemWrapperVNL::MultiplyMatrixMatrix(unsigned int ResultMatrixIndex, unsigned int LeftMatrixIndex, unsigned int RightMatrixIndex)
{
  
  delete (*m_Matrices)[ResultMatrixIndex];
  (*m_Matrices)[ResultMatrixIndex] = new vnl_sparse_matrix<Float>( this->GetSystemOrder(), this->GetSystemOrder() );

  ((*m_Matrices)[LeftMatrixIndex])->mult( *((*m_Matrices)[RightMatrixIndex]), *((*m_Matrices)[ResultMatrixIndex]) );

}


void LinearSystemWrapperVNL::MultiplyMatrixVector(unsigned int ResultVectorIndex, unsigned int MatrixIndex, unsigned int VectorIndex)
{

  delete (*m_Vectors)[ResultVectorIndex];
  (*m_Vectors)[ResultVectorIndex] = new vnl_vector<Float>(this->GetSystemOrder());

  ((*m_Matrices)[MatrixIndex])->mult( *((*m_Vectors)[VectorIndex]), *((*m_Vectors)[ResultVectorIndex]) );

}


LinearSystemWrapperVNL::~LinearSystemWrapperVNL()
{
  unsigned int i;
  for (i=0; i<m_NumberOfMatrices; i++)
  {
    this->DestroyMatrix(i);
  }
  for (i=0; i<m_NumberOfVectors; i++)
  {
    this->DestroyVector(i);
  }
  for (i=0; i<m_NumberOfSolutions; i++)
  {
    this->DestroySolution(i);
  }

  delete m_Matrices;
  delete m_Vectors;
  delete m_Solutions;
}

}} // end namespace itk::fem
