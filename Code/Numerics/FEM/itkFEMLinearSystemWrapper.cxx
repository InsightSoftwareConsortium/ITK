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
#include <vector>


namespace itk {
namespace fem {




void LinearSystemWrapper::Clean( void )
{
  unsigned int i;

  // FIXME: Does not work properly for all derived classes

  // clear all data
  for(i=0;i<m_NumberOfMatrices;i++)
  {
    this->DestroyMatrix(i);
  }
  for(i=0;i<m_NumberOfVectors;i++)
  {
    this->DestroyVector(i);
  }
  for(i=0;i<m_NumberOfSolutions;i++)
  {
    this->DestroySolution(i);
  }

  this->SetSystemOrder(0);

}


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


void LinearSystemWrapper::GetColumnsOfNonZeroMatrixElementsInRow( unsigned int, ColumnArray& cols, unsigned int )
{
  // By default we assume full matrices and return indices of all columns
  cols=ColumnArray(m_Order);
  for(unsigned int i=0;i<m_Order;i++)
  {
    cols[i]=i;
  }
}


void LinearSystemWrapper::OptimizeMatrixStorage(unsigned int matrixIndex, unsigned int tempMatrixIndex)
{

  /* put original matrix in temp space */
  this->SwapMatrices(matrixIndex, tempMatrixIndex);

  /* re-initialze storage */
  this->InitializeMatrix(matrixIndex);

  /* loop through old matrix and pull out non-zero values */
  ColumnArray currentRow;
  unsigned int i;
  unsigned int j;
  for (i=0; i<this->m_Order; i++)
  {
    this->GetColumnsOfNonZeroMatrixElementsInRow(i, currentRow, tempMatrixIndex);
    for (j=0; j<currentRow.size(); j++)
    {
      this->SetMatrixValue(i,currentRow[j],this->GetMatrixValue(i, currentRow[j], tempMatrixIndex), matrixIndex);
    }
  }
      
  /* destroy temp matrix space */
  this->DestroyMatrix(tempMatrixIndex);

}



void
LinearSystemWrapper
::CopyMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2)
{
  ColumnArray cols;
  unsigned int r;

  this->InitializeMatrix(matrixIndex2);

  for (r=0; r<this->m_Order; r++)
  {
    this->GetColumnsOfNonZeroMatrixElementsInRow(r, cols, matrixIndex1);
    for(LinearSystemWrapper::ColumnArray::iterator c=cols.begin(); c!=cols.end(); c++)
    {
      this->SetMatrixValue(r,*c,this->GetMatrixValue(r, *c, matrixIndex1), matrixIndex2);
    }
  }
}




void
LinearSystemWrapper
::AddMatrixMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2)
{
  ColumnArray cols;
  unsigned int r;

  for (r=0; r<this->m_Order; r++)
  {
    this->GetColumnsOfNonZeroMatrixElementsInRow(r, cols, matrixIndex2);
    for(LinearSystemWrapper::ColumnArray::iterator c=cols.begin(); c!=cols.end(); c++)
    {
      this->AddMatrixValue(r,*c,this->GetMatrixValue(r, *c, matrixIndex2), matrixIndex1);
    }
  }
}




void
LinearSystemWrapper
::CopyVector(unsigned int vectorSource, unsigned int vectorDestination)
{
  unsigned int r;
  for (r=0; r<this->m_Order; r++)
  {
    this->SetVectorValue(r,this->GetVectorValue(r, vectorSource), vectorDestination);
  }
}




void
LinearSystemWrapper
::AddVectorVector(unsigned int vectorIndex1, unsigned int vectorIndex2)
{
  unsigned int r;
  for (r=0; r<this->m_Order; r++)
  {
    this->AddVectorValue(r,this->GetVectorValue(r, vectorIndex2), vectorIndex1);
  }
}




/* FIXME - untested...do not use yet */
void LinearSystemWrapper::ReverseCuthillMckeeOrdering(ColumnArray& newNumbering, unsigned int matrixIndex)
{

  /* find cuthill-mckee ordering */
  this->CuthillMckeeOrdering(newNumbering, -1, matrixIndex);

}


void LinearSystemWrapper::CuthillMckeeOrdering(ColumnArray& newNumbering, int startingRow, unsigned int matrixIndex)
{


  ColumnArray reverseMapping;                   /* temp storage for re-mapping of rows */
  newNumbering = ColumnArray(this->m_Order);    /* new row numbering */
  reverseMapping = ColumnArray (this->m_Order); /* allocate temp storage */
  unsigned int i;                               /* loop counter */
  
  /* find degrees of each row in matrix & initialize newNumbering vector */
  ColumnArray currentRow;                 /* column indices of nonzero in current row */
  ColumnArray rowDegree(this->m_Order);   /* degrees in each row */
  
  /* initialize variables */
  for (i=0; i<this->m_Order; i++)
  {
    this->GetColumnsOfNonZeroMatrixElementsInRow(i, currentRow, matrixIndex);
    rowDegree[i] = currentRow.size() - 1;     /* assuming non-zero diagonal */
    reverseMapping[i] = this->m_Order;        /* set to impossible value */
  }

  /* choose starting row if not given - chooses row of lowest degree */
  if (startingRow < 0) 
  {
    unsigned int lowestDegree = rowDegree[0];
    startingRow = 0;
    for (i=1; i<this->m_Order; i++) 
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

  for (i=0; i<this->m_Order; i++)
  {
    newNumbering[ reverseMapping[i] ] = i;
  }

}

  
void LinearSystemWrapper::FollowConnectionsCuthillMckeeOrdering(unsigned int rowNumber, ColumnArray& rowDegree, ColumnArray& reverseMapping, unsigned int nextRowNumber, unsigned int matrixIndex)
{

  unsigned int i,j,k,temp;
  ColumnArray::iterator rowBufferIt;
  ColumnArray::iterator nextRowsIt;
  ColumnArray bufferArray;
  ColumnArray rowBuffer;

  if (reverseMapping[rowNumber] > (this->m_Order-1) ) return;

  /* temp vector of next rows to examine */
  ColumnArray nextRows;
  this->GetColumnsOfNonZeroMatrixElementsInRow(rowNumber, nextRows, matrixIndex);

  /* remove diagonal element */
  for (nextRowsIt = nextRows.begin(); nextRowsIt != nextRows.end(); ++nextRowsIt)
  {
    if ( *nextRowsIt == rowNumber )
    {
      nextRows.erase(nextRowsIt);
      --nextRowsIt;
    }
  }

  /* order by degree */
  if (nextRows.size() > 1) 
  {
    for (i=0; i<nextRows.size()-1; i++)
    {
      for (j=0; j<nextRows.size()-1-i; j++)
      {
        if ( rowDegree[nextRows[j+1]] < rowDegree[nextRows[j]] )
        {
          temp = nextRows[j+1];
          nextRows[j+1] = nextRows[j];
          nextRows[j] = temp;
        }
      }
    }
  }

  /* while there are more rows to examine */
  while ( (nextRows.size() != 0 ) && (nextRowNumber < this->m_Order) ) 
  {
    

    bufferArray.clear();

    for (i=0; i<nextRows.size(); i++) 
    {
      reverseMapping[ nextRows[i] ] = nextRowNumber++;
    }


    /* renumber rows in nextRows */
    for (i=0; i<nextRows.size(); i++) 
    {

      /* connections of current row */
      this->GetColumnsOfNonZeroMatrixElementsInRow( nextRows[i], rowBuffer, matrixIndex );

      /* remove previously renumbered rows */
      for (rowBufferIt = rowBuffer.begin(); rowBufferIt != rowBuffer.end(); ++rowBufferIt)
      {
        if (reverseMapping[*rowBufferIt] < this->m_Order )
        {
          rowBuffer.erase(rowBufferIt);
          --rowBufferIt;
        }
      }

      /* order by degree */
      if (rowBuffer.size() > 1)
      {
        for (k=0; k<rowBuffer.size()-1; k++)
        {
          for (j=0; j<rowBuffer.size()-1-k; j++)
          {
            if ( rowDegree[rowBuffer[j+1]] < rowDegree[rowBuffer[j]] )
            {
              temp = rowBuffer[j+1];
              rowBuffer[j+1] = rowBuffer[j];
              rowBuffer[j] = temp;
            }
          }
        }
      }

      /* add rows in rowBuffer to bufferArray (don't add repeats) */
      unsigned int repeatFlag = 0;
      for (k=0; k<rowBuffer.size(); k++)
      {
        repeatFlag = 0;
        for (j=0; j<bufferArray.size(); j++)
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

  return;


}


FEMExceptionLinearSystem::FEMExceptionLinearSystem(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription) :
  FEMException(file,lineNumber)
{
  SetDescription("Error in linear system: "+moreDescription);
  SetLocation(location);
}


FEMExceptionLinearSystemBounds::FEMExceptionLinearSystemBounds(const char *file, unsigned int lineNumber, std::string, std::string moreDescription, unsigned int index1) :
  FEMException(file,lineNumber)
{
  OStringStream buf;
  buf << "Index of " << moreDescription << " out of bounds (" << index1 << ")";
  SetDescription(buf.str().c_str());

}


FEMExceptionLinearSystemBounds::FEMExceptionLinearSystemBounds(const char *file, unsigned int lineNumber, std::string, std::string, unsigned int index1, unsigned int index2) :
  FEMException(file,lineNumber)
{
  OStringStream buf;
  buf << "Index out of bounds (" << index1 << "," << index2 << ")";
  SetDescription(buf.str().c_str());

}

}}
