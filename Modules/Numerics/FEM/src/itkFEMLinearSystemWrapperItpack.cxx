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
#include "itkNumericTraits.h"
#include "itpack.h"
#include "itkFEMLinearSystemWrapperItpack.h"

namespace itk
{
namespace fem
{
/**
 * constructor
 */
LinearSystemWrapperItpack::LinearSystemWrapperItpack()
{
  /* fill m_Methods with pointers to solver functions */
  m_Methods[0] = jcg_;
  m_Methods[1] = jsi_;
  m_Methods[2] = sor_;
  m_Methods[3] = ssorcg_;
  m_Methods[4] = ssorsi_;
  m_Methods[5] = rscg_;
  m_Methods[6] = rssi_;
  m_Method = 0;                   /* set default method to jcg_ */

  /* Set solving parameters */
  dfault_( &( m_IPARM[0] ), &( m_RPARM[0] ) );

  // We don't want the solver routines to
  // overwrite the parameters.
  m_IPARM[2] = 1;

  /* m_IPARM[0] = 500; */  /* number of iterations */
  m_IPARM[1] = -1;   /* no error message output */
  m_IPARM[4] = 1;    /* non-symmetric matrix */

  /* itpack recommended (but not default) value */
#undef min
  m_RPARM[7] = 500.0 * NumericTraits<double>::min();

  m_MaximumNonZeroValues = 0;
  m_Matrices = ITK_NULLPTR;
  m_Vectors = ITK_NULLPTR;
  m_Solutions = ITK_NULLPTR;
}

void LinearSystemWrapperItpack::InitializeMatrix(unsigned int matrixIndex)
{
  /* error checking */
  if( !m_Order )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::InitializeMatrix",
                                   "System order not set");
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::InitializeMatrix",
                                         "m_Matrices",
                                         matrixIndex);
    }
  if( !m_MaximumNonZeroValues )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::InitializeMatrix",
                                   "Maximum number of non zeros not set");
    }

  // allocate if necessay
  if( m_Matrices == ITK_NULLPTR )
    {
    m_Matrices = new MatrixHolder(m_NumberOfMatrices);
    }

  /* Set required variables */
  ( *m_Matrices )[matrixIndex].Clear();
  ( *m_Matrices )[matrixIndex].SetOrder(m_Order);
  ( *m_Matrices )[matrixIndex].SetMaxNonZeroValues(m_MaximumNonZeroValues);
}

bool LinearSystemWrapperItpack::IsMatrixInitialized(unsigned int matrixIndex)
{
  if( !m_Matrices )
    {
    return false;
    }
  if( !( *m_Matrices )[matrixIndex].GetOrder() )
    {
    return false;
    }
  if( !( *m_Matrices )[matrixIndex].GetMaxNonZeroValues() )
    {
    return false;
    }

  return true;
}

void LinearSystemWrapperItpack::InitializeVector(unsigned int vectorIndex)
{
  /* error checking */
  if( !m_Order )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::InitializeVector",
                                   "System order not set");
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::InitializeVector",
                                         "m_Vectors",
                                         vectorIndex);
    }

  /* allocate if necessay */
  if( m_Vectors == ITK_NULLPTR )
    {
    m_Vectors = new VectorHolder(m_NumberOfVectors);
    }

  /* delete old vector */
  delete[] ( *m_Vectors )[vectorIndex];

  /* insert new vector */
  ( *m_Vectors )[vectorIndex] = new doublereal[m_Order];
  /* fill with zeros */
  for( unsigned int i = 0; i < m_Order; i++ )
    {
    ( *m_Vectors )[vectorIndex][i] = 0.0;
    }
}

bool LinearSystemWrapperItpack::IsVectorInitialized(unsigned int vectorIndex)
{
  if( !m_Vectors )
    {
    return false;
    }
  if( !( *m_Vectors )[vectorIndex] )
    {
    return false;
    }

  return true;
}

void LinearSystemWrapperItpack::InitializeSolution(unsigned int solutionIndex)
{
  /* FIX ME: exceptions */
  if( !m_Order )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::InitializeSolution",
                                   "System order not set");
    }
  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::InitializeSolution",
                                         "m_Solutions",
                                         solutionIndex);
    }

  // allocate if necessay
  if( m_Solutions == ITK_NULLPTR )
    {
    m_Solutions = new VectorHolder(m_NumberOfSolutions);
    }

  /* delete old vector */
  delete[] ( *m_Solutions )[solutionIndex];

  /* insert new vector */
  ( *m_Solutions )[solutionIndex] = new doublereal[m_Order];
  /* fill with zeros */
  for( unsigned int i = 0; i < m_Order; i++ )
    {
    ( *m_Solutions )[solutionIndex][i] = 0.0;
    }
}

bool LinearSystemWrapperItpack::IsSolutionInitialized(unsigned int solutionIndex)
{
  if( !m_Solutions )
    {
    return false;
    }
  if( !( *m_Solutions )[solutionIndex] )
    {
    return false;
    }

  return true;
}

void LinearSystemWrapperItpack::DestroyMatrix(unsigned int matrixIndex)
{
  /* FIX ME: exceptions */
  if( m_Matrices )
    {
    if( matrixIndex >= m_NumberOfMatrices )
      {
      throw FEMExceptionLinearSystemBounds(__FILE__,
                                          __LINE__,
                                          "LinearSystemWrapperItpack::DestroyMatrix",
                                          "m_Matrices",
                                          matrixIndex);
      }

    ( *m_Matrices )[matrixIndex].Clear();
    }
}

void LinearSystemWrapperItpack::DestroyVector(unsigned int vectorIndex)
{
  /* FIXME: exceptions */
  if( m_Vectors )
    {
    if( vectorIndex >= m_NumberOfVectors )
      {
      throw FEMExceptionLinearSystemBounds(__FILE__,
                                          __LINE__,
                                          "LinearSystemWrapperItpack::DestroyVector",
                                          "m_Vectors",
                                          vectorIndex);
      }

    /* delete vector */
    delete[] ( *m_Vectors )[vectorIndex];
    ( *m_Vectors )[vectorIndex] = ITK_NULLPTR;
    }
}

void LinearSystemWrapperItpack::DestroySolution(unsigned int solutionIndex)
{
  // FIXME: exceptions
  if( m_Solutions )
    {
    if( solutionIndex >= m_NumberOfSolutions )
      {
      throw FEMExceptionLinearSystemBounds(__FILE__,
                                          __LINE__,
                                          "LinearSystemWrapperItpack::DestroySolution",
                                          "m_Solutions",
                                          solutionIndex);
      }

    /* delete vector */
    delete[] ( *m_Solutions )[solutionIndex];
    ( *m_Solutions )[solutionIndex] = ITK_NULLPTR;
    }
}

LinearSystemWrapperItpack::Float LinearSystemWrapperItpack::GetMatrixValue(unsigned int i,
                                                                           unsigned int j,
                                                                           unsigned int matrixIndex) const
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::GetMatrixValue",
                                   "No matrices have been allocated");
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetMatrixValue",
                                         "m_Matrices",
                                         matrixIndex);
    }
  if( ( i >= m_Order ) || ( j >= m_Order ) )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetMatrixValue",
                                         "m_Matrices[]",
                                         i,
                                         j);
    }

  /* return value */
  return ( *m_Matrices )[matrixIndex].Get(i, j);
}

void LinearSystemWrapperItpack::SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SetMatrixValue",
                                   "No matrices have been allocated");
    }
  if( ( i >= m_Order ) || ( j >= m_Order ) )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetMatrixValue",
                                         "m_Matrices[]",
                                         i,
                                         j);
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetMatrixValue",
                                         "m_Matrices",
                                         matrixIndex);
    }

  /* set value */
  ( ( *m_Matrices )[matrixIndex] ).Set(i, j, value);
}

void LinearSystemWrapperItpack::AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  // FIXME: error checking
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddMatrixValue",
                                   "No matrices have been allocated");
    }
  if( ( i >= m_Order ) || ( j >= m_Order ) )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddMatrixValue",
                                         "m_Matrices[]",
                                         i,
                                         j);
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddMatrixValue",
                                         "m_Matrices",
                                         matrixIndex);
    }

  ( ( *m_Matrices )[matrixIndex] ).Add(i, j, value);
}

void LinearSystemWrapperItpack::GetColumnsOfNonZeroMatrixElementsInRow(unsigned int row,
                                                                       ColumnArray & cols,
                                                                       unsigned int matrixIndex)
{
  /* FIXME: error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::GetColumnsOfNonZeroMatrixElementsInRow",
                                   "No matrices have been allocated");
    }
  if( row >= this->m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetColumnsOfNonZeroMatrixElementsInRow",
                                         "m_Matrices[]",
                                         row);
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetColumnsOfNonZeroMatrixElementsInRow",
                                         "m_Matrices",
                                         matrixIndex);
    }

  cols.clear();

  ItpackSparseMatrix *mat = &( *m_Matrices )[matrixIndex];

  /* Check if matrix is in readable form */
  if( mat->m_MatrixFinalized )
    {
    /* get search bounds in appropriate row */
    unsigned int lower = mat->m_IA[row] - 1;
    unsigned int upper = mat->m_IA[row + 1] - 1;
    for( unsigned int j = lower; j < upper; j++ )
      {
      cols.push_back(mat->m_JA[j] - 1);
      }
    }
  else /* Scan the linked list to obtain the correct indices. */
    {
    int wrk = mat->m_IA[row] - 1;
    while( wrk > 0 )
      {
      cols.push_back(mat->m_JA[wrk] - 1);
      wrk = mat->m_IWORK[wrk] - 1;
      }
    }
}

void LinearSystemWrapperItpack::ScaleMatrix(Float scale, unsigned int matrixIndex)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::ScaleMatrix",
                                   "No matrices have been allocated");
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::ScaleMatrix",
                                         "m_Matrices",
                                         matrixIndex);
    }

  int         i;
  doublereal *values = ( *m_Matrices )[matrixIndex].GetA();
  for( i = 0; i < ( *m_Matrices )[matrixIndex].GetIA()[this->m_Order] - 1; i++ )
    {
    values[i] = values[i] * scale;
    }
}

LinearSystemWrapperItpack::Float LinearSystemWrapperItpack::GetVectorValue(unsigned int i,
                                                                           unsigned int vectorIndex) const
{
  /* error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::GetVectorValue",
                                   "No vectors have been allocated");
    }
  if( i >= m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetVectorValue",
                                         "m_Vectors[]",
                                         i);
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::GetVectorValue",
                                         "m_Vectors",
                                         vectorIndex);
    }
  if( !( *m_Vectors )[vectorIndex] )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::GetVectorValue",
                                   "Indexed vector not yet allocated");
    }

  /* return value */
  return ( *m_Vectors )[vectorIndex][i];
}

void LinearSystemWrapperItpack::SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  /* error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SetVectorValue",
                                   "No vectors have been allocated");
    }
  if( i >= m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetVectorValue",
                                         "m_Vectors[]",
                                         i);
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetVectorValue",
                                         "m_Vectors",
                                         vectorIndex);
    }
  if( !( *m_Vectors )[vectorIndex] )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SetVectorValue",
                                   "Indexed vector not yet allocated");
    }

  /* set value */
  ( *m_Vectors )[vectorIndex][i] = value;
}

void LinearSystemWrapperItpack::AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  /*( error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddVectorValue",
                                   "No vectors have been allocated");
    }
  if( i >= m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddVectorValue",
                                         "m_Vectors[]",
                                         i);
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddVectorValue",
                                         "m_Vectors",
                                         vectorIndex);
    }
  if( !( *m_Vectors )[vectorIndex] )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddVectorValue",
                                   "Indexed vector not yet allocated");
    }

  /* add value */
  ( *m_Vectors )[vectorIndex][i] += value;
}

LinearSystemWrapperItpack::Float LinearSystemWrapperItpack::GetSolutionValue(unsigned int i,
                                                                             unsigned int solutionIndex) const
{
  // FIXME: error checking
  if( ( i >= m_Order ) || !m_Solutions || ( solutionIndex >= m_NumberOfSolutions ) || !( *m_Solutions )[solutionIndex] )
    {
    return 0.0;
    }
  return ( *m_Solutions )[solutionIndex][i];
}

void LinearSystemWrapperItpack::SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  /* error checking */
  if( !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SetSolutionValue",
                                   "No solutions have been allocated");
    }
  if( i >= m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetSolutionValue",
                                         "m_Solutions[]",
                                         i);
    }
  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SetSolutionValue",
                                         "m_Solutions",
                                         solutionIndex);
    }
  if( !( *m_Solutions )[solutionIndex] )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SetSolutionValue",
                                   "Indexed solution not yet allocated");
    }

  /* set value */
  ( *m_Solutions )[solutionIndex][i] = value;
}

void LinearSystemWrapperItpack::AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  /* error checking */
  if( !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddSolutionValue",
                                   "No solutions have been allocated");
    }
  if( i >= m_Order )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddSolutionValue",
                                         "m_Solutions[]",
                                         i);
    }
  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::AddSolutionValue",
                                         "m_Solutions",
                                         solutionIndex);
    }
  if( !( *m_Solutions )[solutionIndex] )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddSolutionValue",
                                   "Indexed solution not yet allocated");
    }

  ( *m_Solutions )[solutionIndex][i] += value;
}

void LinearSystemWrapperItpack::Solve(void)
{
  /* error checking */
  if( !m_Order || !m_Matrices || !m_Vectors || !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::Solve",
                                   "Not all necessary data members have been allocated");
    }
  if( !( *m_Matrices )[0].GetOrder() )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::AddSolutionValue",
                                   "Primary matrix never filled");
    }

  /* itpack variables */
  integer     N;
  integer     NB;
  integer     NW;
  integer     NCG;
  integer *   IWKSP;
  doublereal *WKSP;
  integer     IERR = 0;

  /*********************************************************************
   * FIX ME: itpack does not allow for any non-zero diagonal elements
   * so "very small" numbers are inserted to allow for a solution
   *
   * int i;
   * doublereal fakeZero = 1.0e-16;

   * //insert "fake" zeros
   * for (i=0; i<static_cast<int>(m_Order); i++)
   *   {
   *   if( (*m_Matrices)[0].Get(i,i) == 0.0)
   *     {
   *     (*m_Matrices)[0].Set(i,i,fakeZero);
   *     }
   *   }
   *   // END FIXME
   *********************************************************************/

  /* Initialize solution values (set to zero) */
  if( !this->IsSolutionInitialized(0) )
    {
    this->InitializeSolution(0);
    }

  /* Set up itpack workspace variables
  *
  * N -> Order of system
  *
  * NCG -> temp var
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
  if( m_IPARM[4] == 1 )
    {
    NCG = 4 * m_IPARM[0];
    }
  else
    {
    NCG = 2 * m_IPARM[0];
    }
  NB = m_IPARM[8] + N; // upper bound of what can be computed in prbndx_

  switch( m_Method )
    {
    case 0:
      NW = 4 * N + NCG;
      break;
    case 1:
      NW = 2 * N;
      break;
    case 2:
      NW = N;
      break;
    case 3:
      NW = 6 * N + NCG;
      break;
    case 4:
      NW = 5 * N;
      break;
    case 5:
      NW = N + 3 * NB + NCG;
      break;
    case 6:
      NW = N + NB;
      break;
    default:
      std::ostringstream msg;
      msg << "m_Method is" << m_Method << " but must be >=0 and <= 6";
      throw FEMExceptionLinearSystem(__FILE__,
                                     __LINE__,
                                     "LinearSystemWrapperItpack::Solve",
                                     msg.str().c_str());
    }
  m_IPARM[7] = NW;
  IWKSP = new integer[3 * N];
  WKSP = new doublereal[NW + 2];

  integer i;
  for( i = 0; i < NW; i++ )
    {
    WKSP[i] = 0.0;
    }
  for( i = 0; i < ( 3 * N ); i++ )
    {
    IWKSP[i] = 0;
    }

  // Save maximum number of iteration, since it will
  // be overwritten.
  int max_num_iterations = m_IPARM[0];

  /* call to itpack solver routine */
  ( *m_Methods[m_Method] )(&N, ( *m_Matrices )[0].GetIA(), ( *m_Matrices )[0].GetJA(), ( *m_Matrices )[0].GetA(),
                           ( *m_Vectors )[0], ( *m_Solutions )[0], &( IWKSP[0] ), &NW, &( WKSP[0] ), &( m_IPARM[0] ),
                           &( m_RPARM[0] ), &IERR);

  m_IPARM[0] = max_num_iterations;

  /* remove exception throw on convergence failure */
  if( IERR < 100 )
    {
    if( ( IERR % 10 ) == 3 )
      {
      IERR = 0;
      }
    }

  /* clean up */
  delete[] IWKSP;
  delete[] WKSP;

  /* check for itpack error code */
  if( IERR > 0 )
    {
    throw FEMExceptionItpackSolver(__FILE__, __LINE__, "LinearSystemWrapperItpack::Solve", IERR);
    }
}

void LinearSystemWrapperItpack::SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SwapMatrices",
                                   "No matrices allocated");
    }
  if( matrixIndex1 >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapMatrices",
                                         "m_Matrices",
                                         matrixIndex1);
    }
  if( matrixIndex2 >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapMatrices",
                                         "m_Matrices",
                                         matrixIndex2);
    }

  int         n = ( *m_Matrices )[matrixIndex2].GetOrder();
  int         nz = ( *m_Matrices )[matrixIndex2].GetMaxNonZeroValues();
  integer *   ia = ( *m_Matrices )[matrixIndex2].GetIA();
  integer *   ja = ( *m_Matrices )[matrixIndex2].GetJA();
  doublereal *a = ( *m_Matrices )[matrixIndex2].GetA();

  ( *m_Matrices )[matrixIndex2].SetOrder( ( *m_Matrices )[matrixIndex1].GetOrder() );
  ( *m_Matrices )[matrixIndex2].SetMaxNonZeroValues( ( *m_Matrices )[matrixIndex1].GetMaxNonZeroValues() );
  ( *m_Matrices )[matrixIndex2].SetCompressedRow( ( *m_Matrices )[matrixIndex1].GetIA(),
                                                  ( *m_Matrices )[matrixIndex1].GetJA(),
                                                  ( *m_Matrices )[matrixIndex1].GetA() );

  ( *m_Matrices )[matrixIndex1].SetOrder(n);
  ( *m_Matrices )[matrixIndex1].SetMaxNonZeroValues(nz);
  ( *m_Matrices )[matrixIndex1].SetCompressedRow(ia, ja, a);
}

void LinearSystemWrapperItpack::SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2)
{
  /* error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__, __LINE__, "LinearSystemWrapperItpack::SwapVectors", "No vectors allocated");
    }
  if( vectorIndex1 >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapVectors",
                                         "m_Vectors",
                                         vectorIndex1);
    }
  if( vectorIndex2 >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapVectors",
                                         "m_Vectors",
                                         vectorIndex2);
    }

  VectorRepresentation temp = ( *m_Vectors )[vectorIndex1];

  ( *m_Vectors )[vectorIndex1] = ( *m_Vectors )[vectorIndex2];
  ( *m_Vectors )[vectorIndex2] = temp;
}

void LinearSystemWrapperItpack::SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2)
{
  /* error checking */
  if( !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::SwapSolutions",
                                   "No solutions allocated");
    }
  if( solutionIndex1 >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapSolutions",
                                         "m_Solutions",
                                         solutionIndex1);
    }
  if( solutionIndex2 >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::SwapSolutions",
                                         "m_Solutions",
                                         solutionIndex2);
    }

  VectorRepresentation temp = ( *m_Solutions )[solutionIndex1];

  ( *m_Solutions )[solutionIndex1] = ( *m_Solutions )[solutionIndex2];
  ( *m_Solutions )[solutionIndex2] = temp;
}

void LinearSystemWrapperItpack::CopySolution2Vector(unsigned solutionIndex,
                                                    unsigned int vectorIndex)
{
  /* error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::CopySolution2Vector",
                                   "No vectors allocated");
    }
  if( !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::CopySolution2Vector",
                                   "No solutions allocated");
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::CopySolution2Vector",
                                         "m_Vectors",
                                         vectorIndex);
    }
  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::CopySolution2Vector",
                                         "m_Solutions",
                                         solutionIndex);
    }

  this->InitializeVector(vectorIndex);
  /* copy values */
  for( unsigned int i = 0; i < m_Order; i++ )
    {
    ( *m_Vectors )[vectorIndex][i] = ( *m_Solutions )[solutionIndex][i];
    }
}

void LinearSystemWrapperItpack::CopyVector2Solution(unsigned vectorIndex, unsigned int solutionIndex)
{
  /* error checking */
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::CopySolution2Vector",
                                   "No vectors allocated");
    }
  if( !m_Solutions )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::CopySolution2Vector",
                                   "No solutions allocated");
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::CopySolution2Vector",
                                         "m_Vectors",
                                         vectorIndex);
    }
  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::CopySolution2Vector",
                                         "m_Solutions",
                                         solutionIndex);
    }

  this->InitializeSolution(solutionIndex);
  /* copy values */
  for( unsigned int i = 0; i < m_Order; i++ )
    {
    ( *m_Solutions )[solutionIndex][i] = ( *m_Vectors )[vectorIndex][i];
    }
}

void LinearSystemWrapperItpack::MultiplyMatrixMatrix(unsigned int resultMatrixIndex,
                                                     unsigned int leftMatrixIndex,
                                                     unsigned int rightMatrixIndex)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixMatrix",
                                   "No matrices allocated");
    }
  if( resultMatrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixMatrix",
                                         "m_Matrices",
                                         resultMatrixIndex);
    }
  if( leftMatrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixMatrix",
                                         "m_Matrices",
                                         leftMatrixIndex);
    }
  if( rightMatrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixMatrix",
                                         "m_Matrices",
                                         rightMatrixIndex);
    }

  ( *m_Matrices )[leftMatrixIndex].mult( &( ( *m_Matrices )[rightMatrixIndex] ), &( ( *m_Matrices )[resultMatrixIndex] ) );
}

void LinearSystemWrapperItpack::MultiplyMatrixVector(unsigned int resultVectorIndex,
                                                     unsigned int matrixIndex,
                                                     unsigned int vectorIndex)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                   "No matrices allocated");
    }
  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                   "No vectors allocated");
    }
  if( resultVectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Vectors",
                                         resultVectorIndex);
    }
  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Matrices",
                                         matrixIndex);
    }
  if( vectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Vectors",
                                         vectorIndex);
    }

  /* perform mult */
  ( *m_Matrices )[matrixIndex].mult( ( *m_Vectors )[vectorIndex], ( *m_Vectors )[resultVectorIndex] );
}


void LinearSystemWrapperItpack::MultiplyMatrixSolution(unsigned int resultVectorIndex,
                                                       unsigned int matrixIndex,
                                                       unsigned int solutionIndex)
{
  /* error checking */
  if( !m_Matrices )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                   "No matrices allocated");
    }

  if( !m_Vectors )
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                   "No vectors allocated");
    }

  if( !m_Solutions)
    {
    throw FEMExceptionLinearSystem(__FILE__,
                                   __LINE__,
                                   "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                   "No solutions allocated");
    }

  if( resultVectorIndex >= m_NumberOfVectors )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Vectors",
                                         resultVectorIndex);
    }

  if( matrixIndex >= m_NumberOfMatrices )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Matrices",
                                         matrixIndex);
    }

  if( solutionIndex >= m_NumberOfSolutions )
    {
    throw FEMExceptionLinearSystemBounds(__FILE__,
                                         __LINE__,
                                         "LinearSystemWrapperItpack::MultiplyMatrixVector",
                                         "m_Solutions",
                                         solutionIndex);
    }

  /* perform multiplication */
  ( *m_Matrices )[matrixIndex].mult( ( *m_Solutions )[solutionIndex], ( *m_Vectors )[resultVectorIndex] );
}


LinearSystemWrapperItpack::~LinearSystemWrapperItpack(void)
{
  delete m_Matrices;

  unsigned int i;
  if( m_Vectors )
    {
    for( i = 0; i < m_NumberOfVectors; i++ )
      {
      delete[] ( *m_Vectors )[i];
      }
    delete m_Vectors;
    }

  if( m_Solutions )
    {
    for( i = 0; i < m_NumberOfSolutions; i++ )
      {
      delete[] ( *m_Solutions )[i];
      }
    delete m_Solutions;
    }
}

FEMExceptionItpackSolver::FEMExceptionItpackSolver(const char *file, unsigned int lineNumber, std::string location,
                                                   integer errorCode) :
  FEMException(file, lineNumber)
{
  std::string solverError;

  if( errorCode < 100 )
    {
    errorCode = errorCode % 10;
    }

  switch( errorCode )
    {
    case 1:
      solverError = "Invalid order of system";
      break;
    case 2:
      solverError = "Workspace is not large enough";
      break;
    case 3:
      solverError = "Failure to converge before reaching maximum number of iterations";
      break;
    case 4:
      solverError = "Invalid order of black subsystem";
      break;
    case 101:
      solverError = "A diagonal element is not positive";
      break;
    case 102:
      solverError = "No diagonal element in a row";
      break;
    case 201:
      solverError = "Red-black indexing is not possible";
      break;
    case 301:
      solverError = "No entry in a row of the original matrix";
      break;
    case 302:
      solverError = "No entry in a row of the permuted matrix";
      break;
    case 303:
      solverError = "Sorting error in a row of the permuted matrix";
      break;
    case 401:
      solverError = "A diagonal element is not positive";
      break;
    case 402:
      solverError = "No diagonal element in a row";
      break;
    case 501:
      solverError = "Failure to converge before reaching maximum number of iterations";
      break;
    case 502:
      solverError = "Function does not change sign at endpoints";
      break;
    case 601:
      solverError = "Successive iterations are not monotone increasing";
      break;
    default:
      solverError = "Unknown error code returned";
    }

  std::ostringstream buf;
  buf << "Error: " << solverError;

  SetDescription( buf.str().c_str() );

  SetLocation(location);
}

}
}   // end namespace itk::fem
