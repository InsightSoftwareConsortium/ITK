/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapper.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLinearSystemWrapper.h"

namespace itk {
namespace fem {

void LinearSystemWrapper::ScaleMatrix(Float scale, unsigned int matrixIndex)
{
  /* FIX ME: error checking */

  /* check for no scaling */
  if (scale == 1.0)
  {
    return;
  }

  unsigned int i;
  unsigned int j;
  for (i=0; i<m_Order; i++)
  {
    for (j=0; j<m_Order; j++)
    {
      this->SetMatrixValue(i, j, scale*GetMatrixValue(i,j,matrixIndex), matrixIndex);
    }
  }

  return;
}

void LinearSystemWrapper::ScaleVector(Float scale, unsigned int vectorIndex)
{
  /* FIX ME: error checking */

  /* check for no scaling */
  if (scale == 1.0)
  {
    return;
  }

  unsigned int i;
  for (i=0; i<m_Order; i++)
  {
    this->SetVectorValue(i, scale * GetVectorValue(i, vectorIndex), vectorIndex);
  }

  return;
}

void LinearSystemWrapper::ScaleSolution(Float scale, unsigned int solutionIndex)
{
  /* FIX ME: error checking */

  /* check for no scaling */
  if (scale == 1.0)
  {

    return;
  }

  unsigned int i;
  for (i=0; i<m_Order; i++)
  {
    this->SetSolutionValue(i, scale * GetSolutionValue(i, solutionIndex), solutionIndex);
  }

  return;
}

void LinearSystemWrapper::AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  this->SetVectorValue(i, value+this->GetVectorValue(i, vectorIndex), vectorIndex);
}

void LinearSystemWrapper::AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  this->SetMatrixValue(i, j, value+this->GetMatrixValue(i, j, matrixIndex), matrixIndex);
}

void LinearSystemWrapper::AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  this->SetSolutionValue(i, value+this->GetSolutionValue(i, solutionIndex), solutionIndex);
}


void LinearSystemWrapper::MultiplyMatrixVector(unsigned int resultVector, unsigned int matrixIndex, unsigned int vectorIndex)
{
  /* FIX ME: error checking */

  unsigned int i;
  unsigned int j;

  this->InitializeVector(resultVector);

  /* perform multiply */
  for (i=0; i<m_Order; i++) 
  {
    for (j=0; j<m_Order; j++)
    {
      this->AddVectorValue(i, this->GetMatrixValue(i,j,matrixIndex) * this->GetVectorValue(j, vectorIndex), resultVector);
    }
  }

}


void LinearSystemWrapper::GetColumnsOfNonZeroMatrixElementsInRow( unsigned int row, ColumnArray& cols, unsigned int matrixIndex )
{
  // By default we assume full matrices and return indices of all columns
  cols=ColumnArray(m_Order);
  for(unsigned int i=0;i<m_Order;i++)
  {
    cols[i]=i;
  }
}



}}
