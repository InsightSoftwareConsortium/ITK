/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMItpackLinearSystemWrapper.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itpack.h"
#include "itpack_f2c.h"
#include "itkFEMItpackLinearSystemWrapper.h"
#include <vector>

namespace itk {
namespace fem {

typedef ItpackLinearSystemWrapper::integer integer;
typedef ItpackLinearSystemWrapper::doublereal doublereal;

/**
 * constructor 
 */
ItpackLinearSystemWrapper::ItpackLinearSystemWrapper()
{

  /* fill m_Methods with pointers to solver functions */
  m_Methods[0] = itpack::jcg_;
  m_Methods[1] = itpack::jsi_;
  m_Methods[2] = itpack::sor_;
  m_Methods[3] = itpack::ssorcg_;
  m_Methods[4] = itpack::ssorsi_;
  m_Methods[5] = itpack::rscg_;
  m_Methods[6] = itpack::rssi_;
  m_Method = 0;                   /* set default method to jcg_ */

  /* Set solving parameters */
  itpack::dfault_( &(m_IPARM[0]) , &(m_RPARM[0]) );
  m_IPARM[0] = 500;  /* number of iterations */
  m_IPARM[1] = -1;   /* no error message output */
  m_IPARM[4] = 1;    /* non-symmetric matrix */

  m_MaximumNonZeroValues = 0;
  m_Matrices = 0;
  m_Vectors = 0;
  m_Solutions = 0;

}


void ItpackLinearSystemWrapper::InitializeMatrix(unsigned int matrixIndex)
{

  /* FIX ME: exceptions */
  if (!m_Order || (matrixIndex >= m_NumberOfMatrices) ) throw FEMException(__FILE__, __LINE__, "FEM error");

  // allocate if necessay
  if (m_Matrices == 0)
  {
    m_Matrices = new MatrixHolder(m_NumberOfMatrices);
    if (m_Matrices == NULL) throw FEMException(__FILE__, __LINE__, "FEM error");
  }

  /* Set required variables */
  (*m_Matrices)[matrixIndex].Clear();
  (*m_Matrices)[matrixIndex].SetOrder(m_Order);
  (*m_Matrices)[matrixIndex].SetMaxNonZeroValues( m_MaximumNonZeroValues );

  return;

}


bool ItpackLinearSystemWrapper::IsMatrixInitialized(unsigned int matrixIndex)
{
  if (!m_Matrices) return false;
  if ( !(*m_Matrices)[matrixIndex].GetOrder() ) return false;
  if ( !(*m_Matrices)[matrixIndex].GetMaxNonZeroValues() ) return false;


  return true;
}

void ItpackLinearSystemWrapper::InitializeVector(unsigned int vectorIndex)
{

  /* FIX ME: exceptions */
  if (!m_Order || !m_NumberOfVectors) throw FEMException(__FILE__, __LINE__, "FEM error");

  /* allocate if necessay */
  if (m_Vectors == 0)
  {
    m_Vectors = new VectorHolder(m_NumberOfVectors);
    if (m_Vectors == NULL) throw FEMException(__FILE__, __LINE__, "FEM error");
  }
  
  /* delete old vector */
  if ( (*m_Vectors)[vectorIndex] != 0 )
  {
    delete [] (*m_Vectors)[vectorIndex];
  }

  /* insert new vector */
  (*m_Vectors)[vectorIndex] = new doublereal [m_Order];
  if ( (*m_Vectors)[vectorIndex] == NULL) throw FEMException(__FILE__, __LINE__, "FEM error");

  /* fill with zeros */
  for (unsigned int i=0; i<m_Order; i++)
  {
    (*m_Vectors)[vectorIndex][i] = 0.0;
  }

  return;

}


bool ItpackLinearSystemWrapper::IsVectorInitialized(unsigned int vectorIndex)
{
  if (!m_Vectors) return false;
  if ( !(*m_Vectors)[vectorIndex] ) return false;

  return true;
}


void ItpackLinearSystemWrapper::InitializeSolution(unsigned int solutionIndex)
{

  /* FIX ME: exceptions */
  if (!m_Order || !m_NumberOfSolutions) throw FEMException(__FILE__, __LINE__, "FEM error");

  // allocate if necessay
  if (m_Solutions == 0)
  {
    m_Solutions = new VectorHolder(m_NumberOfSolutions);
    if (m_Solutions == NULL) throw;
  }

  /* delete old vector */
  if ( (*m_Solutions)[solutionIndex] != 0 )
  {
    delete [] (*m_Solutions)[solutionIndex];
  }

  /* insert new vector */
  (*m_Solutions)[solutionIndex] = new doublereal [m_Order];
  if ( (*m_Solutions)[solutionIndex] == NULL) throw;

  /* fill with zeros */
  for (unsigned int i=0; i<m_Order; i++)
  {
    (*m_Solutions)[solutionIndex][i] = 0.0;
  }

  return;

}


bool ItpackLinearSystemWrapper::IsSolutionInitialized(unsigned int solutionIndex)
{
  if (!m_Solutions) return false;
  if ( !(*m_Solutions)[solutionIndex] ) return false;

  return true;
}

void ItpackLinearSystemWrapper::DestroyMatrix(unsigned int matrixIndex)
{

  /* FIX ME: exceptions */
  if ( !m_Matrices ) return;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  (*m_Matrices)[matrixIndex].Clear();
}


void ItpackLinearSystemWrapper::DestroyVector(unsigned int vectorIndex)
{
  /* FIX ME: exceptions */
  if (!m_Vectors) return;
  if (vectorIndex >= m_NumberOfVectors) throw;
  if ( !(*m_Vectors)[vectorIndex] ) return;

  /* delete vector */
  delete [] (*m_Vectors)[vectorIndex];
  (*m_Vectors)[vectorIndex] = 0;

}


void ItpackLinearSystemWrapper::DestroySolution(unsigned int solutionIndex)
{
  // FIX ME: exceptions
  if (!m_Solutions) return;
  if (solutionIndex >= m_NumberOfSolutions) throw;
  if ( !(*m_Solutions)[solutionIndex] ) return;

  /* delete vector */
  delete [] (*m_Solutions)[solutionIndex];
  (*m_Solutions)[solutionIndex] = 0;

}


ItpackLinearSystemWrapper::Float ItpackLinearSystemWrapper::GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex) const
{
  // FIX ME: error checking
  if ( (i >= m_Order) || (j >= m_Order) || !m_Matrices) throw;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  return (*m_Matrices)[matrixIndex].Get(i,j);
}


void ItpackLinearSystemWrapper::SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || (j >= m_Order) || !m_Matrices) throw;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  ((*m_Matrices)[matrixIndex]).Set(i,j,value);
}


void ItpackLinearSystemWrapper::AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || (j >= m_Order) || !m_Matrices) throw;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  ((*m_Matrices)[matrixIndex]).Add(i,j,value);
}


void ItpackLinearSystemWrapper::GetColumnsOfNonZeroMatrixElementsInRow( unsigned int row, ColumnArray& cols, unsigned int matrixIndex )
{
  cols.clear();
  // FIX ME: error checking
  if ( (row >= m_Order) || !m_Matrices) throw;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  ItpackSparseMatrix* mat=&(*m_Matrices)[matrixIndex];

  /* Check if matrix is in readable form */
  if (mat->m_MatrixFinalized )
  {
    /* get search bounds in appropriate row */
    unsigned int lower = mat->m_IA[row]-1;
    unsigned int upper = mat->m_IA[row+1]-1;
  
    for(unsigned int j=lower; j<upper; j++)
    {
      cols.push_back(mat->m_JA[j]-1);
    }
  }
  else /* Scan the linked list to obtain the correct indices. */
  {
    int wrk=mat->m_IA[row]-1;
    while(wrk>0)
    {
      cols.push_back(mat->m_JA[wrk]-1);
      wrk=mat->m_IWORK[wrk]-1;
    }
  }

}


void ItpackLinearSystemWrapper::ScaleMatrix(Float scale, unsigned int matrixIndex)
{
  // FIX ME: error checking
  if ( !m_Matrices || (matrixIndex >= m_NumberOfMatrices) ) throw;
  if ( matrixIndex >= m_NumberOfMatrices ) throw;

  int i;
  doublereal *values = (*m_Matrices)[matrixIndex].GetA();
  for (i=0; i<(*m_Matrices)[matrixIndex].GetMaxNonZeroValues(); i++) 
  {
    values[i] = values[i]*scale;
  }
  
}


ItpackLinearSystemWrapper::Float ItpackLinearSystemWrapper::GetVectorValue(unsigned int i, unsigned int vectorIndex) const
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Vectors || (vectorIndex >= m_NumberOfVectors) ) throw;
  if ( !(*m_Vectors)[vectorIndex] ) throw;

  return (*m_Vectors)[vectorIndex][i];
}


void ItpackLinearSystemWrapper::SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Vectors || (vectorIndex >= m_NumberOfVectors) ) throw;
  if ( !(*m_Vectors)[vectorIndex] ) throw;

  (*m_Vectors)[vectorIndex][i] = value;
}


void ItpackLinearSystemWrapper::AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Vectors || (vectorIndex >= m_NumberOfVectors) ) throw;
  if ( !(*m_Vectors)[vectorIndex] ) throw;

  (*m_Vectors)[vectorIndex][i] += value;
}


ItpackLinearSystemWrapper::Float ItpackLinearSystemWrapper::GetSolutionValue(unsigned int i, unsigned int solutionIndex) const
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Solutions || (solutionIndex >= m_NumberOfSolutions) || !(*m_Solutions)[solutionIndex] )
  {
    return 0.0;
  }
  return (*m_Solutions)[solutionIndex][i];
}


void ItpackLinearSystemWrapper::SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Solutions || (solutionIndex >= m_NumberOfSolutions) ) throw;
  if ( !(*m_Solutions)[solutionIndex] ) throw;

  (*m_Solutions)[solutionIndex][i] = value;

}


void ItpackLinearSystemWrapper::AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  // FIX ME: error checking
  if ( (i >= m_Order) || !m_Solutions || (solutionIndex >= m_NumberOfSolutions) ) throw;
  if ( !(*m_Solutions)[solutionIndex] ) throw;

  (*m_Solutions)[solutionIndex][i] += value;
}


void ItpackLinearSystemWrapper::Solve(void)
{

  // FIX ME: error checking
  if (!m_Order || !m_Matrices || !m_Vectors || !m_Solutions) throw;
  if ( !(*m_Matrices)[0].GetOrder() ) throw;

  /* itpack variables */
  integer N;
  integer NW;
  integer NCG;
  integer *IWKSP;
  doublereal *WKSP;
  integer IERR = 0;

  /* *******************************************************************
   * FIX ME: itpack does not allow for any non-zero diagonal elements
   * so "very small" numbers are inserted to allow for a solution
   */
  int i;
  doublereal fakeZero = 1.0e-16;

  /* insert "fake" zeros */
  for (i=0; i<static_cast<int>(m_Order); i++)
  {
    if ( (*m_Matrices)[0].Get(i,i) == 0.0)
    {
      (*m_Matrices)[0].Set(i,i,fakeZero);
    }
  }
  /* END FIX ME 
   *********************************************************************/


  /* Initialize solution values (set to zero) */
  this->InitializeSolution(0);


 /* Set up itpack workspace variables
  *
  * N -> Order of system
  *
  * NCG -> 
  *
  * NW -> on input: length of wksp, on output: actual amount used
  *  jcg_()    - 4*N + NCG
  *  jsi_()    - 2*N
  *  sor_()    - N
  *  ssorcg_() - 6*N + NCG
  *  ssorsi_() - 5*N
  *  rscg_()   - N + 3*NB + NCG 
  *  rssi_()   - N + NB
  *
  * IWKSP -> temp variable used by itpack (integer[3*N])
  * WKSP -> temp variable used by itpack (doublereal[NW])
  */
  N = m_Order;
  if (m_IPARM[4] == 1)
  {
    NCG = 4 * m_IPARM[0];
  }
  else 
  {
    NCG = 2 * m_IPARM[1];
  }
  switch ( m_Method ) {
  case 0:
    NW = 4*N + NCG;
    break;
  case 1:
    NW = 2*N;
    break;
  case 2:
    NW = N;
    break;
  case 3:
    NW = 6*N + NCG;
    break;
  case 4:
    NW = 5*N;
    break;
  case 5:
    NW = N + m_IPARM[8] + NCG;
    break;
  case 6:
    NW = N + m_IPARM[8];
    break;
  }
  m_IPARM[7] = NW;
  IWKSP = new integer [ 3*N ];
  WKSP = new doublereal [ NW ];

  for (i=0; i<NW; i++) 
  {
    WKSP[i] = 0.0;
  }
  for (i=0; i<(3*N); i++) 
  {
    IWKSP[i] = 0;
  }


  /* crap
  for (i=0; i<=N; i++) {
    std::cout << (*m_Matrices)[0].GetIA()[i] << " ";
  }
  std::cout << std::endl;

  for (i=0; i<(*m_Matrices)[0].GetMaxNonZeroValues(); i++)
  {
    std::cout << (*m_Matrices)[0].GetJA()[i] << " ";
  }
  std::cout << std::endl;

  for (i=0; i<(*m_Matrices)[0].GetMaxNonZeroValues(); i++)
  {
    std::cout << (*m_Matrices)[0].GetA()[i] << " ";
  }
  std::cout << std::endl;
  */

  /* call to itpack solver routine */
  (*m_Methods[m_Method])( &N, (*m_Matrices)[0].GetIA(), (*m_Matrices)[0].GetJA(), (*m_Matrices)[0].GetA(), 
    (*m_Vectors)[0], (*m_Solutions)[0], &(IWKSP[0]), &NW, &(WKSP[0]), &(m_IPARM[0]), &(m_RPARM[0]), &IERR);

  delete [] IWKSP;
  delete [] WKSP;

  /*
   * error flag for matrix solving
   *   IERR = 0   - no error
   *        = 1   - invalid order of system
   *        = 2   - m_WKSP no large enough - m_IPARM[7] is set to required space m_NW
   *        = 3   - failure to converge in m_IPARM[0] iterations.  m_RPARM[0] is reset to last stopping value
   *        = 4   - invalid order of the black subsystem ???
   *        = 101 - a diagnoal element is not positive
   *        = 102 - no diagonal element in a row
   *        = 201 - red-black indexing is not possible
   *        = 301 - no entry in a row of the original matrix
   *        = 302 - no entry in a row of the permuted matrix
   *        = 303 - sorting error in a row of the permuted matrix
   *        = 401 - a diagonal element is not positive
   *        = 402 - no diagonal element in a row
   *        = 501 - failure to converge in m_ITMAX function evaluations
   *        = 502 - function does not change sign at the endpointegers
   *        = 601 - successive iterates are not monotone increasing
   */

}


void ItpackLinearSystemWrapper::SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2)
{

  /* FIX ME: error checking */
  if (!m_Matrices || (matrixIndex1 >= m_NumberOfMatrices) || (matrixIndex2 >= m_NumberOfMatrices) ) throw;

  int n = (*m_Matrices)[matrixIndex2].GetOrder();
  int nz = (*m_Matrices)[matrixIndex2].GetMaxNonZeroValues();
  integer* ia = (*m_Matrices)[matrixIndex2].GetIA();
  integer *ja = (*m_Matrices)[matrixIndex2].GetJA();
  doublereal *a = (*m_Matrices)[matrixIndex2].GetA();

  (*m_Matrices)[matrixIndex2].SetOrder( (*m_Matrices)[matrixIndex1].GetOrder() );
  (*m_Matrices)[matrixIndex2].SetMaxNonZeroValues ( (*m_Matrices)[matrixIndex1].GetMaxNonZeroValues() );
  (*m_Matrices)[matrixIndex2].SetCompressedRow((*m_Matrices)[matrixIndex1].GetIA(), 
    (*m_Matrices)[matrixIndex1].GetJA(), (*m_Matrices)[matrixIndex1].GetA() );

  (*m_Matrices)[matrixIndex1].SetOrder( n );
  (*m_Matrices)[matrixIndex1].SetMaxNonZeroValues ( nz );
  (*m_Matrices)[matrixIndex1].SetCompressedRow(ia, ja, a);



}


void ItpackLinearSystemWrapper::SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2)
{

  if ( !m_Vectors || (vectorIndex1 >= m_NumberOfVectors) || (vectorIndex2 >= m_NumberOfVectors) ) throw;

  VectorRepresentation temp = (*m_Vectors)[vectorIndex1];

  (*m_Vectors)[vectorIndex1] = (*m_Vectors)[vectorIndex2];
  (*m_Vectors)[vectorIndex2] = temp;
}


void ItpackLinearSystemWrapper::SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2)
{

  if ( !m_Solutions || (solutionIndex1 >= m_NumberOfSolutions) || (solutionIndex2 >= m_NumberOfSolutions) ) throw;

  VectorRepresentation temp = (*m_Solutions)[solutionIndex1];

  (*m_Solutions)[solutionIndex1] = (*m_Solutions)[solutionIndex2];
  (*m_Solutions)[solutionIndex2] = temp;
}


void ItpackLinearSystemWrapper::CopySolution2Vector(unsigned solutionIndex, unsigned int vectorIndex)
{

  /* FIX ME: error checking */
  if (!m_Solutions || !m_Vectors || (solutionIndex >= m_NumberOfSolutions) || (vectorIndex >= m_NumberOfVectors) ) throw;
  if ( !(*m_Solutions)[solutionIndex] ) throw;

  this->InitializeVector(vectorIndex);

  /* copy values */
  for (unsigned int i=0; i<m_Order; i++)
  {
    (*m_Vectors)[vectorIndex][i] = (*m_Solutions)[solutionIndex][i];
  }
}


void ItpackLinearSystemWrapper::MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex, unsigned int rightMatrixIndex)
{

  if (!m_Matrices || (resultMatrixIndex >= m_NumberOfMatrices) || (leftMatrixIndex >= m_NumberOfMatrices) || (rightMatrixIndex >= m_NumberOfMatrices) ) throw;
  if ( !(*m_Matrices)[leftMatrixIndex].GetOrder() || !(*m_Matrices)[rightMatrixIndex].GetOrder() ) throw;

  (*m_Matrices)[leftMatrixIndex].mult( &((*m_Matrices)[rightMatrixIndex]), &((*m_Matrices)[resultMatrixIndex]) );

}


void ItpackLinearSystemWrapper::MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex)
{

  if (!m_Matrices || !m_Vectors || (resultVectorIndex >= m_NumberOfVectors) || (matrixIndex >= m_NumberOfMatrices) || (vectorIndex >= m_NumberOfVectors) ) throw;
  if ( !(*m_Vectors)[vectorIndex] ) throw;

  (*m_Matrices)[matrixIndex].mult( (*m_Vectors)[vectorIndex], (*m_Vectors)[resultVectorIndex] );

}


ItpackLinearSystemWrapper::~ItpackLinearSystemWrapper(void)
{
  delete m_Matrices;

  unsigned int i;
  if (m_Vectors != 0)
  {
    for (i=0; i<m_NumberOfVectors; i++)
    {
      if ( (*m_Vectors)[i] != 0 )
      {
        delete [] (*m_Vectors)[i];
      }
    }
    delete m_Vectors;
  }

  
  if (m_Solutions != 0)
  {
    for (i=0; i<m_NumberOfSolutions; i++)
    {
      if ( (*m_Solutions)[i] != 0 )
      {
        delete [] (*m_Solutions)[i];
      }
    }
    delete m_Solutions;
  }
  


}


}}  // end namespace itk::fem

