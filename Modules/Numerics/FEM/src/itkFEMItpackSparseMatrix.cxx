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
#include "itkFEMItpackSparseMatrix.h"
#include "itpack.h"

namespace itk
{
namespace fem
{
ItpackSparseMatrix::ItpackSparseMatrix()
{
  m_MatrixFinalized = 0;
  m_MatrixInitialized = 0;
  m_NZ = 0;
  m_N = 0;
  /* m_IER = 0; */     /* initialize */
  m_MODE = 1;    /* add to existing entries when building matrix */
  m_LEVEL = -1;  /* no error messages */
  m_NOUT = 0;    /* output unit number */

  m_IA = ITK_NULLPTR;
  m_JA = ITK_NULLPTR;
  m_IWORK = ITK_NULLPTR;
  m_A = ITK_NULLPTR;
}

ItpackSparseMatrix::ItpackSparseMatrix(integer order)
{
  m_MatrixFinalized = 0;
  m_MatrixInitialized = 0;
  m_NZ = 0;
  m_N = order;
  /* m_IER = 0; */     /* initialize */
  m_MODE = 1;    /* add to existing entries when building matrix */
  m_LEVEL = -1;  /* no error messages */
  m_NOUT = 0;    /* output unit number */

  m_IA = ITK_NULLPTR;
  m_JA = ITK_NULLPTR;
  m_IWORK = ITK_NULLPTR;
  m_A = ITK_NULLPTR;
}

ItpackSparseMatrix::ItpackSparseMatrix(integer order, integer maxNonZeroValues)
{
  m_MatrixFinalized = 0;
  m_MatrixInitialized = 0;
  m_N = order;
  m_NZ = maxNonZeroValues;
  /* m_IER = 0; */      /* initialize */
  m_MODE = 1;     /* add to existing entries when building matrix */
  m_LEVEL = -1;   /* no error messages */
  m_NOUT = 0;     /* output unit number */

  m_IA = ITK_NULLPTR;
  m_JA = ITK_NULLPTR;
  m_IWORK = ITK_NULLPTR;
  m_A = ITK_NULLPTR;
}

void ItpackSparseMatrix::Initialize()
{
  /* is matrix ready for initialization */
  if( ( m_N <= 0 ) || ( m_NZ <= 0 ) )
    {
    /* FIX ME: error handling */
    throw FEMException(__FILE__, __LINE__, "ItpackSparseMatrix::Initialize");
    }

  /* initialize itpack variables */

  delete[] m_IA;
  delete[] m_JA;
  delete[] m_IWORK;
  delete[] m_A;
  m_IA =    new integer[m_N + 1];
  m_JA =    new integer[m_NZ];
  m_IWORK = new integer[m_NZ];
  m_A =     new doublereal[m_NZ];

  int i;
  for( i = 0; i < m_NZ; i++ )
    {
    m_JA[i] = 0;
    m_IWORK[i] = 0;
    m_A[i] = 0.0;
    }
  for( i = 0; i <= m_N; i++ )
    {
    m_IA[i] = 0;
    }

  /* initialize sparse matrix storage via itpack routine */
  sbini_(&m_N, &m_NZ, m_IA, m_JA, m_A, m_IWORK);

  /* set info flags */
  m_MatrixInitialized = 1;
  m_MatrixFinalized = 0;
  /* Do this to avoid itpack ignorance (unless it's somehow my ignorance) */
  for( i = 0; i < m_N; i++ )
    {
    this->Set(i, i, 0.0);
    }
}

void ItpackSparseMatrix::Clear()
{
  /* free variables */
  delete[] m_IA;
  delete[] m_JA;
  delete[] m_IWORK;
  delete[] m_A;

  m_MatrixFinalized = 0;
  m_MatrixInitialized = 0;
  m_N = 0;
  m_NZ = 0;
  /* m_IER = 0;   */
  m_MODE = 1;
  m_LEVEL = -1;
  m_NOUT = 0;

  m_IA = ITK_NULLPTR;
  m_JA = ITK_NULLPTR;
  m_IWORK = ITK_NULLPTR;
  m_A = ITK_NULLPTR;
}

void ItpackSparseMatrix::Finalize()
{
  /* check */
  if( ( m_MatrixFinalized != 0 ) || ( m_MatrixInitialized == 0 ) )
    {
    throw FEMException(__FILE__, __LINE__, "ItpackSparseMatrix::Finalize");
    }

  // std::cout << "sbend_ ... " << std::endl;
  // this->PrintCompressedRow();

  /* finalize */
  sbend_(&m_N, &m_NZ, m_IA, m_JA, m_A, m_IWORK);

  // this->PrintCompressedRow();
  // std::cout << "sbend_ " << m_IER << std::endl;

  /* set info flag */
  m_MatrixFinalized = 1;
}

void ItpackSparseMatrix::UnFinalize()
{
  /* check if this op makes sense*/
  if( ( m_MatrixFinalized == 0 ) || ( m_MatrixInitialized == 0 ) )
    {
    throw FEMException(__FILE__, __LINE__, "ItpackSparseMatrix::UnFinalize");
    }

  integer IER = 0;

  sbagn_(&m_N, &m_NZ, m_IA, m_JA, m_A, m_IWORK, &m_LEVEL, &m_NOUT, &IER);

  if( IER > 0 )
    {
    throw FEMExceptionItpackSparseMatrixSbagn(__FILE__, __LINE__, "ItpackSparseMatrix::UnFinalize", IER);
    }

  /* set info flag */
  m_MatrixFinalized = 0;
}

void ItpackSparseMatrix::Set(integer i, integer j, doublereal value)
{
  /* check for dynamic form */
  if( m_MatrixInitialized == 0 )
    {
    /* initialize if prepared */
    if( ( m_N <= 0 ) || ( m_NZ <= 0 ) )
      {
      throw FEMException(__FILE__, __LINE__, "ItpackSparseMatrix::Set");
      }
    else
      {
      this->Initialize();
      }
    }

  if( m_MatrixFinalized == 1 )
    {
    this->UnFinalize();
    }

  /* replace an existing value */
  m_MODE = 0;

  /* add entry (itpack expects 1-based indices */
  integer IER;
  integer fortranI = i + 1;
  integer fortranJ = j + 1;
  sbsij_(&m_N, &m_NZ, m_IA, m_JA, m_A, m_IWORK, &fortranI, &fortranJ, &value, &m_MODE, &m_LEVEL, &m_NOUT, &IER);

  if( IER > 700 )
    {
    throw FEMExceptionItpackSparseMatrixSbsij(__FILE__, __LINE__, "ItpackSparseMatrix::Set", IER);
    }
}

void ItpackSparseMatrix::Add(integer i, integer j, doublereal value)
{
  /* ignore add zero */
  if( value == 0.0 )
    {
    return;
    }

  /* check for dynamic form */
  if( m_MatrixInitialized == 0 )
    {
    /* initialize if prepared */
    if( ( m_N <= 0 ) || ( m_NZ <= 0 ) )
      {
      throw FEMException(__FILE__, __LINE__, "ItpackSparseMatrix::Add");
      }
    else
      {
      this->Initialize();
      }
    }
  if( m_MatrixFinalized != 0 )
    {
    this->UnFinalize();
    }

  /* add to an existing value */
  m_MODE = 1;

  /* add entry (itpack expects 1-based indices */
  integer IER;
  integer fortranI = i + 1;
  integer fortranJ = j + 1;
  sbsij_(&m_N, &m_NZ, m_IA, m_JA, m_A, m_IWORK, &fortranI, &fortranJ, &value, &m_MODE, &m_LEVEL, &m_NOUT, &IER);

  if( IER > 700 )
    {
    throw FEMExceptionItpackSparseMatrixSbsij(__FILE__, __LINE__, "ItpackSparseMatrix::Set", IER);
    }
}

ItpackSparseMatrix::doublereal ItpackSparseMatrix::Get(integer i, integer j)
{
  doublereal returnValue = 0.0; /* set to default return value */
  integer    fortranJ = j + 1;
  integer    lower;
  integer    upper;

  /* check for readiness */
  if( m_MatrixInitialized != 0 )
    {
    /* ensure matrix is in readable form */
    if( m_MatrixFinalized == 0 )
      {
      this->Finalize();
      }

    /* get search bounds in appropriate row */
    lower = m_IA[i] - 1;
    upper = m_IA[i + 1] - 1;
    /* Find value if it exists */
    for( int k = lower; k < upper; k++ )
      {
      if( m_JA[k] == fortranJ )
        {
        returnValue = m_A[k];
        }
      }
    }

  return returnValue;
}

ItpackSparseMatrix::doublereal * ItpackSparseMatrix::GetA()
{
  if( m_MatrixInitialized == 0 )
    {
    return ITK_NULLPTR;
    }
  if( m_MatrixFinalized == 0 )
    {
    Finalize();
    }

  return m_A;
}

ItpackSparseMatrix::integer * ItpackSparseMatrix::GetIA()
{
  if( m_MatrixInitialized == 0 )
    {
    return ITK_NULLPTR;
    }
  if( m_MatrixFinalized == 0 )
    {
    Finalize();
    }

  return m_IA;
}

ItpackSparseMatrix::integer * ItpackSparseMatrix::GetJA()
{
  if( m_MatrixInitialized == 0 )
    {
    return ITK_NULLPTR;
    }
  if( m_MatrixFinalized == 0 )
    {
    Finalize();
    }

  return m_JA;
}

void ItpackSparseMatrix::mult(doublereal *vector, doublereal *result)
{
  /* finalize matrix */
  if( m_MatrixFinalized == 0 )
    {
    this->Finalize();
    }

  /* loop and temp variables */
  int lower;
  int upper;
  int i;
  int j;
  /* prepare result vector */
  // delete[] result;
  // result = new doublereal [ m_N ];
  for( i = 0; i < m_N; i++ )
    {
    result[i] = 0.0;
    }
  /* perform the mult operation */
  for( i = 0; i < m_N; i++ )
    {
    lower = m_IA[i] - 1;
    upper = m_IA[i + 1] - 1;
    for( j = lower; j < upper; j++ )
      {
      result[i] += m_A[j] * vector[m_JA[j] - 1];
      }
    }
}

void ItpackSparseMatrix::mult(ItpackSparseMatrix *rightMatrix, ItpackSparseMatrix *resultMatrix)
{
  /* ensure appropriate matrix sizes */
  if( m_N != rightMatrix->GetOrder() )
    {
    return;
    }

  /* finalize matrix */
  if( m_MatrixFinalized == 0 )
    {
    this->Finalize();
    }

  /* loop and temp variables */
  int        lower;   /* lower bounds for column indices vector */
  int        upper;   /* upper bounds for column indices vector */
  int        i;       /* loop over rows */
  int        j;       /* loop over columns */
  int        k;       /* iterate through row */
  doublereal summed;  /* temp holder for row.column */
  /* perform the mult operation */
  for( i = 0; i < m_N; i++ )
    {
    for( j = 0; j < m_N; j++ )
      {
      /* bounds of values located in current row */
      lower = m_IA[i] - 1;
      upper = m_IA[i + 1] - 1;

      // sum up row*column elements
      summed = 0.0;
      for( k = lower; k < upper; k++ )
        {
        summed += m_A[k] * rightMatrix->Get(m_JA[k] - 1, j);
        }

      // insert sum to result matrix
      if( summed != 0.0 )
        {
        resultMatrix->Set(i, j, summed);
        }
      }
    }
}

void ItpackSparseMatrix::SetCompressedRow(integer *ia, integer *ja, doublereal *a)
{
  m_IA = ia;
  m_JA = ja;
  m_A = a;
  m_MatrixFinalized = 1;
  m_MatrixInitialized = 1;
}

ItpackSparseMatrix::~ItpackSparseMatrix()
{
  delete[] m_IA;
  delete[] m_JA;
  delete[] m_A;
  delete[] m_IWORK;
}

FEMExceptionItpackSparseMatrixSbagn::FEMExceptionItpackSparseMatrixSbagn(const char *file, unsigned int lineNumber,
                                                                         std::string location,
                                                                         integer errorCode) :
  FEMException(file, lineNumber)
{
  std::string solverError;

  if( errorCode == 703 )
    {
    solverError = "maximumNumberOfNonZeroValuesInMatrix is too small";
    }
  else
    {
    solverError = "Unknown error code returned";
    }

  std::ostringstream buf;
  buf << "Error: " << solverError;

  SetDescription( buf.str().c_str() );

  SetLocation(location);
}

FEMExceptionItpackSparseMatrixSbagn::~FEMExceptionItpackSparseMatrixSbagn() ITK_NOEXCEPT
{
}

FEMExceptionItpackSparseMatrixSbsij::FEMExceptionItpackSparseMatrixSbsij(const char *file, unsigned int lineNumber,
                                                                         std::string location,
                                                                         integer errorCode) :
  FEMException(file, lineNumber)
{
  std::string solverError;

  switch( errorCode )
    {
    case 701:
      solverError = "Improper index of matrix";
      break;
    case 702:
      solverError = "maximumNumberOfNonZeroValuesInMatrix is too small";
      break;
    default:
      solverError = "Unknown error code returned";
    }

  std::ostringstream buf;
  buf << "Error: " << solverError;

  SetDescription( buf.str().c_str() );

  SetLocation(location);
}

FEMExceptionItpackSparseMatrixSbsij::~FEMExceptionItpackSparseMatrixSbsij() ITK_NOEXCEPT
{
}

}
}   // end namespace itk::fem
