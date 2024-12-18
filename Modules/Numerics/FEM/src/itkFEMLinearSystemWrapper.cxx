/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFEMLinearSystemWrapper.h"

namespace itk
{
namespace fem
{

LinearSystemWrapper::~LinearSystemWrapper() = default;

void
LinearSystemWrapper::Clean()
{
  // FIXME: Does not work properly for all derived classes
  // clear all data
  for (unsigned int i = 0; i < m_NumberOfMatrices; ++i)
  {
    this->DestroyMatrix(i);
  }
  for (unsigned int i = 0; i < m_NumberOfVectors; ++i)
  {
    this->DestroyVector(i);
  }
  for (unsigned int i = 0; i < m_NumberOfSolutions; ++i)
  {
    this->DestroySolution(i);
  }

  this->SetSystemOrder(0);
}

void
LinearSystemWrapper::ScaleMatrix(Float scale, unsigned int matrixIndex)
{
  /* check for no scaling */
  if (scale == 1.0)
  {
    return;
  }

  for (unsigned int i = 0; i < m_Order; ++i)
  {
    for (unsigned int j = 0; j < m_Order; ++j)
    {
      this->SetMatrixValue(i, j, scale * GetMatrixValue(i, j, matrixIndex), matrixIndex);
    }
  }
}

void
LinearSystemWrapper::ScaleVector(Float scale, unsigned int vectorIndex)
{

  /* check for no scaling */
  if (scale == 1.0)
  {
    return;
  }
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    this->SetVectorValue(i, scale * GetVectorValue(i, vectorIndex), vectorIndex);
  }
}

void
LinearSystemWrapper::ScaleSolution(Float scale, unsigned int solutionIndex)
{
  /* check for no scaling */
  if (scale == 1.0)
  {
    return;
  }
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    this->SetSolutionValue(i, scale * GetSolutionValue(i, solutionIndex), solutionIndex);
  }
}

void
LinearSystemWrapper::AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex)
{
  this->SetVectorValue(i, value + this->GetVectorValue(i, vectorIndex), vectorIndex);
}

void
LinearSystemWrapper::AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex)
{
  this->SetMatrixValue(i, j, value + this->GetMatrixValue(i, j, matrixIndex), matrixIndex);
}

void
LinearSystemWrapper::AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex)
{
  this->SetSolutionValue(i, value + this->GetSolutionValue(i, solutionIndex), solutionIndex);
}

void
LinearSystemWrapper::MultiplyMatrixVector(unsigned int resultVector, unsigned int matrixIndex, unsigned int vectorIndex)
{
  this->InitializeVector(resultVector);
  /* perform multiply */
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    for (unsigned int j = 0; j < m_Order; ++j)
    {
      this->AddVectorValue(
        i, this->GetMatrixValue(i, j, matrixIndex) * this->GetVectorValue(j, vectorIndex), resultVector);
    }
  }
}


void
LinearSystemWrapper::MultiplyMatrixSolution(unsigned int resultVector,
                                            unsigned int matrixIndex,
                                            unsigned int solutionIndex)
{
  this->InitializeVector(resultVector);
  /* perform multiply */
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    for (unsigned int j = 0; j < m_Order; ++j)
    {
      this->AddVectorValue(
        i, this->GetMatrixValue(i, j, matrixIndex) * this->GetSolutionValue(j, solutionIndex), resultVector);
    }
  }
}

void
LinearSystemWrapper::GetColumnsOfNonZeroMatrixElementsInRow(unsigned int, ColumnArray & cols, unsigned int)
{
  // By default we assume full matrices and return indices of all columns
  cols = ColumnArray(m_Order);
  for (unsigned int i = 0; i < m_Order; ++i)
  {
    cols[i] = i;
  }
}

void
LinearSystemWrapper::OptimizeMatrixStorage(unsigned int matrixIndex, unsigned int tempMatrixIndex)
{
  /* put original matrix in temp space */
  this->SwapMatrices(matrixIndex, tempMatrixIndex);

  /* re-initialze storage */
  this->InitializeMatrix(matrixIndex);

  /* loop through old matrix and pull out non-zero values */
  for (unsigned int i = 0; i < this->m_Order; ++i)
  {
    ColumnArray currentRow;
    this->GetColumnsOfNonZeroMatrixElementsInRow(i, currentRow, tempMatrixIndex);
    for (unsigned int j = 0; j < currentRow.size(); ++j)
    {
      this->SetMatrixValue(i, currentRow[j], this->GetMatrixValue(i, currentRow[j], tempMatrixIndex), matrixIndex);
    }
  }

  /* destroy temp matrix space */
  this->DestroyMatrix(tempMatrixIndex);
}

void
LinearSystemWrapper::CopyMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2)
{
  this->InitializeMatrix(matrixIndex2);
  for (unsigned int r = 0; r < this->m_Order; ++r)
  {
    ColumnArray cols;
    this->GetColumnsOfNonZeroMatrixElementsInRow(r, cols, matrixIndex1);
    for (const auto & col : cols)
    {
      this->SetMatrixValue(r, col, this->GetMatrixValue(r, col, matrixIndex1), matrixIndex2);
    }
  }
}

void
LinearSystemWrapper::AddMatrixMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2)
{
  for (unsigned int r = 0; r < this->m_Order; ++r)
  {
    ColumnArray cols;
    this->GetColumnsOfNonZeroMatrixElementsInRow(r, cols, matrixIndex2);
    for (const auto & col : cols)
    {
      this->AddMatrixValue(r, col, this->GetMatrixValue(r, col, matrixIndex2), matrixIndex1);
    }
  }
}

void
LinearSystemWrapper::CopyVector(unsigned int vectorSource, unsigned int vectorDestination)
{
  for (unsigned int r = 0; r < this->m_Order; ++r)
  {
    this->SetVectorValue(r, this->GetVectorValue(r, vectorSource), vectorDestination);
  }
}

void
LinearSystemWrapper::AddVectorVector(unsigned int vectorIndex1, unsigned int vectorIndex2)
{
  for (unsigned int r = 0; r < this->m_Order; ++r)
  {
    this->AddVectorValue(r, this->GetVectorValue(r, vectorIndex2), vectorIndex1);
  }
}

/* FIXME - untested...do not use yet */
void
LinearSystemWrapper::ReverseCuthillMckeeOrdering(ColumnArray & newNumbering, unsigned int matrixIndex)
{
  /* find cuthill-mckee ordering */
  this->CuthillMckeeOrdering(newNumbering, -1, matrixIndex);
}

void
LinearSystemWrapper::CuthillMckeeOrdering(ColumnArray & newNumbering, int startingRow, unsigned int matrixIndex)
{
  /* temp storage for re-mapping
                               of rows */

  newNumbering = ColumnArray(this->m_Order);               /* new row numbering */
  ColumnArray reverseMapping = ColumnArray(this->m_Order); /* allocate temp storage */

  /* find degrees of each row in matrix & initialize newNumbering vector */
  ColumnArray currentRow;               /* column indices of nonzero in
                                          current row */
  ColumnArray rowDegree(this->m_Order); /* degrees in each row */
  /* initialize variables */
  for (unsigned int i = 0; i < this->m_Order; ++i)
  {
    this->GetColumnsOfNonZeroMatrixElementsInRow(i, currentRow, matrixIndex);
    rowDegree[i] = static_cast<unsigned int>(currentRow.size() - 1); /*
                                                                       assuming
                                                                       non-zero
                                                                       diagonal
                                                                       */
    reverseMapping[i] = this->m_Order;                               /* set
                                                                       to
                                                                       impossible
                                                                       value
                                                                       */
  }

  /* choose starting row if not given - chooses row of lowest degree */
  if (startingRow < 0)
  {
    unsigned int lowestDegree = rowDegree[0];
    startingRow = 0;
    for (unsigned int i = 1; i < this->m_Order; ++i)
    {
      if (rowDegree[i] < lowestDegree)
      {
        startingRow = i;
        lowestDegree = rowDegree[i];
      }
    }
  }

  /* set first row */
  unsigned int nextRowNumber = 0;
  reverseMapping[startingRow] = nextRowNumber++;

  /* follow connections and assign new row numbering */
  this->FollowConnectionsCuthillMckeeOrdering(startingRow, rowDegree, reverseMapping, nextRowNumber, matrixIndex);
  for (unsigned int i = 0; i < this->m_Order; ++i)
  {
    newNumbering[reverseMapping[i]] = i;
  }
}

void
LinearSystemWrapper::FollowConnectionsCuthillMckeeOrdering(unsigned int  rowNumber,
                                                           ColumnArray & rowDegree,
                                                           ColumnArray & reverseMapping,
                                                           unsigned int  nextRowNumber,
                                                           unsigned int  matrixIndex)
{
  if (reverseMapping[rowNumber] > (this->m_Order - 1))
  {
    return;
  }

  /* temp vector of next rows to examine */
  ColumnArray nextRows;
  this->GetColumnsOfNonZeroMatrixElementsInRow(rowNumber, nextRows, matrixIndex);
  /* remove diagonal element */
  for (ColumnArray::iterator nextRowsIt = nextRows.begin(); nextRowsIt != nextRows.end(); ++nextRowsIt)
  {
    if (*nextRowsIt == rowNumber)
    {
      nextRowsIt = nextRows.erase(nextRowsIt);
    }
  }

  /* order by degree */
  if (nextRows.size() > 1)
  {
    for (int i = 0; i < static_cast<int>(nextRows.size()) - 1; ++i)
    {
      for (int j = 0; j < static_cast<int>(nextRows.size()) - 1 - i; ++j)
      {
        if (rowDegree[nextRows[j + 1]] < rowDegree[nextRows[j]])
        {
          unsigned int temp = nextRows[j + 1];
          nextRows[j + 1] = nextRows[j];
          nextRows[j] = temp;
        }
      }
    }
  }

  ColumnArray bufferArray;
  /* while there are more rows to examine */
  while ((!nextRows.empty()) && (nextRowNumber < this->m_Order))
  {
    bufferArray.clear();
    for (int i = 0; i < static_cast<int>(nextRows.size()); ++i)
    {
      reverseMapping[nextRows[i]] = nextRowNumber++;
    }
    /* renumber rows in nextRows */
    for (int i = 0; i < static_cast<int>(nextRows.size()); ++i)
    {
      /* connections of current row */
      ColumnArray rowBuffer;
      this->GetColumnsOfNonZeroMatrixElementsInRow(nextRows[i], rowBuffer, matrixIndex);
      /* remove previously renumbered rows */
      for (ColumnArray::iterator rowBufferIt = rowBuffer.begin(); rowBufferIt != rowBuffer.end(); ++rowBufferIt)
      {
        if (reverseMapping[*rowBufferIt] < this->m_Order)
        {
          rowBufferIt = rowBuffer.erase(rowBufferIt);
        }
      }

      /* order by degree */
      if (rowBuffer.size() > 1)
      {
        for (int k = 0; k < static_cast<int>(rowBuffer.size()) - 1; ++k)
        {
          for (int j = 0; j < static_cast<int>(rowBuffer.size()) - 1 - k; ++j)
          {
            if (rowDegree[rowBuffer[j + 1]] < rowDegree[rowBuffer[j]])
            {
              unsigned int temp = rowBuffer[j + 1];
              rowBuffer[j + 1] = rowBuffer[j];
              rowBuffer[j] = temp;
            }
          }
        }
      }

      /* add rows in rowBuffer to bufferArray (don't add repeats) */
      for (int k = 0; k < static_cast<int>(rowBuffer.size()); ++k)
      {
        unsigned int repeatFlag = 0;
        for (int j = 0; j < static_cast<int>(bufferArray.size()); ++j)
        {
          if (bufferArray[j] == rowBuffer[k])
          {
            repeatFlag = 1;
          }
        }

        if (!repeatFlag)
        {
          bufferArray.push_back(rowBuffer[k]);
        }
      }
    }

    nextRows.clear();
    nextRows = bufferArray;
  }
}

FEMExceptionLinearSystem::~FEMExceptionLinearSystem() noexcept = default;

FEMExceptionLinearSystem::FEMExceptionLinearSystem(const char * file,
                                                   unsigned int lineNumber,
                                                   std::string  location,
                                                   std::string  moreDescription)
  : FEMException(file, lineNumber)
{
  SetDescription("Error in linear system: " + moreDescription);
  SetLocation(location);
}

FEMExceptionLinearSystemBounds::FEMExceptionLinearSystemBounds(const char * file,
                                                               unsigned int lineNumber,
                                                               std::string,
                                                               std::string  moreDescription,
                                                               unsigned int index1)
  : FEMException(file, lineNumber)
{
  std::ostringstream buf;

  buf << "Index of " << moreDescription << " out of bounds (" << index1 << ')';
  SetDescription(buf.str().c_str());
}

FEMExceptionLinearSystemBounds::FEMExceptionLinearSystemBounds(const char * file,
                                                               unsigned int lineNumber,
                                                               std::string,
                                                               std::string,
                                                               unsigned int index1,
                                                               unsigned int index2)
  : FEMException(file, lineNumber)
{
  std::ostringstream buf;

  buf << "Index out of bounds (" << index1 << ',' << index2 << ')';
  SetDescription(buf.str().c_str());
}

FEMExceptionLinearSystemBounds::~FEMExceptionLinearSystemBounds() noexcept = default;

} // end namespace fem
} // end namespace itk
