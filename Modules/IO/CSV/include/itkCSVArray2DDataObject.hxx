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
#ifndef itkCSVArray2DDataObject_hxx
#define itkCSVArray2DDataObject_hxx

#include "itkCSVArray2DDataObject.h"
#include <fstream>
#include <vector>
#include <algorithm>

namespace itk
{
template <typename TData>
CSVArray2DDataObject<TData>
::CSVArray2DDataObject()
{
  this->m_HasRowHeaders = false;
  this->m_HasColumnHeaders = false;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::StringVectorType
CSVArray2DDataObject<TData>
 ::GetColumnHeaders() const
{
  return this->m_ColumnHeaders;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::StringVectorType
CSVArray2DDataObject<TData>
 ::GetRowHeaders() const
{
  return this->m_RowHeaders;
}

template <typename TData>
unsigned int
CSVArray2DDataObject<TData>
 ::GetRowIndexByName(const std::string & row_name) const
{
  if ( !this->m_HasRowHeaders )
    {
    itkExceptionMacro( << "The dataset does not contain any row headers!");
    }

  const typename StringVectorType::const_iterator it =
    std::find(this->m_RowHeaders.begin(),this->m_RowHeaders.end(),row_name);
  const unsigned int index = distance(this->m_RowHeaders.begin(), it);

  if ( it == this->m_RowHeaders.end() )
    {
    itkExceptionMacro( << "The row name " << row_name <<" does not exist. ");
    }
  return index;
}

template <typename TData>
unsigned int
CSVArray2DDataObject<TData>
 ::GetColumnIndexByName(const std::string & column_name) const
{
  if ( !this->m_HasColumnHeaders )
    {
    itkExceptionMacro( << "The dataset does not contain any column headers!");
    }

  const typename StringVectorType::const_iterator it =
    std::find(this->m_ColumnHeaders.begin(),this->m_ColumnHeaders.end(),column_name);
  const unsigned int index = distance(this->m_ColumnHeaders.begin(), it);

  if ( it == this->m_ColumnHeaders.end() )
    {
    itkExceptionMacro( << "The column name " << column_name << " does not exist." );
    }
  return index;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::NumericVectorType
CSVArray2DDataObject <TData>
 ::GetRow(const unsigned int & row_index) const
{
  NumericVectorType row;
  unsigned int max_rows = this->m_Matrix.rows() - 1;
  if ( row_index > max_rows )
    {
    itkExceptionMacro( <<" Row index: " << row_index
                       << " exceeds matrix dimension: " << max_rows);
    }
  unsigned int vector_size = this->m_Matrix.cols();
  for (unsigned int i = 0; i < vector_size; i++)
    {
    row.push_back(this->m_Matrix[row_index][i]);
    }
  return row;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::NumericVectorType
CSVArray2DDataObject<TData>
 ::GetRow(const std::string & row_name) const
{
  NumericVectorType row;
  unsigned int index = this->GetRowIndexByName(row_name);
  row = this->GetRow(index);
  return row;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::NumericVectorType
CSVArray2DDataObject <TData>
 ::GetColumn(const unsigned int & column_index) const
{
  NumericVectorType column;
  unsigned int max_columns = this->m_Matrix.columns() - 1;
  if ( column_index > max_columns )
    {
    itkExceptionMacro( << "Column index: " << column_index
                       << " exceeds matrix dimension: " << max_columns );
    }
  unsigned int vector_size = this->m_Matrix.rows();
  for (unsigned int i = 0; i < vector_size; i++)
    {
    column.push_back(this->m_Matrix[i][column_index]);
    }
  return column;
}

template <typename TData>
typename CSVArray2DDataObject<TData>::NumericVectorType
CSVArray2DDataObject <TData>
 ::GetColumn(const std::string & column_name) const
{
  NumericVectorType column;
  unsigned int index = this->GetColumnIndexByName(column_name);
  column = this->GetColumn(index);
  return column;
}


template <typename TData>
TData
CSVArray2DDataObject <TData>
 ::GetData(const unsigned int & row, const unsigned int & column) const
{
  if ( row > this->m_Matrix.rows() - 1)
    {
    itkExceptionMacro( <<" Row index: " << row
                       << " exceeds row dimension: " << this->m_Matrix.rows() - 1);
    }
  else if ( column > this->m_Matrix.cols()-1 )
    {
    itkExceptionMacro( <<" Column index: " << column
                       << " exceeds column dimension: " << this->m_Matrix.columns() - 1);
    }
  return this->m_Matrix[row][column];
}

template <typename TData>
TData
CSVArray2DDataObject <TData>
 ::GetData(const std::string & row_name, const std::string & column_name) const
{
  unsigned int row_index = this->GetRowIndexByName(row_name);
  unsigned int column_index = this->GetColumnIndexByName(column_name);
  return this->GetData(row_index, column_index);
}


template <typename TData>
TData
CSVArray2DDataObject <TData>
 ::GetRowData(const std::string & row_name, const unsigned int & column_index) const
{
  unsigned int row_index = this->GetRowIndexByName(row_name);
  return this->GetData(row_index, column_index);
}

template <typename TData>
TData
CSVArray2DDataObject <TData>
 ::GetColumnData(const std::string & column_name, const unsigned int & row_index) const
{
  unsigned int column_index = this->GetColumnIndexByName(column_name);
  return this->GetData(row_index, column_index);
}

template<typename TData>
TData
CSVArray2DDataObject <TData>
::operator()(const unsigned int & row_index, const unsigned int & column_index) const
{
  return this->GetData(row_index, column_index);
}

template<typename TData>
TData
CSVArray2DDataObject <TData>
::operator()(const std::string & row_name, const std::string & column_name) const
{
  return this->GetData(row_name, column_name);
}

template <typename TData>
void
CSVArray2DDataObject <TData>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "Number of rows in matrix: " << this->m_Matrix.rows() << std::endl;
  os << "Number of columns in matrix: " << this->m_Matrix.cols() << std::endl << std::endl;
  os << "Column Headers existence: " << this->m_HasColumnHeaders << std::endl;
  os << "Row Headers existence: " << this->m_HasRowHeaders << std::endl;
  os << "Number of Column Headers: " << this->m_ColumnHeaders.size() << std::endl;
  os << "Number of Row Headers: " << this->m_RowHeaders.size() << std::endl;

  os << "Below is the data: " << std::endl << std::endl;

  if ( this->m_HasColumnHeaders )
    {
    os << indent << indent;
    for (unsigned int i = 0; i < this->m_ColumnHeaders.size(); i++)
      {
      os << this->m_ColumnHeaders[i] << indent;
      }
    os << std::endl;
    }

  for (unsigned int i = 0; i < this->m_Matrix.rows(); i++)
    {
    if (this->m_HasRowHeaders)
      {
      os << this->m_RowHeaders[i] << indent;
      }
    for (unsigned int j = 0; j < this->m_Matrix.cols(); j++)
      {
      os << this->m_Matrix[i][j] << indent;
      }
    os << std::endl;
    }
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::SetMatrixSize(unsigned int rows, unsigned int columns)
{
  this->m_Matrix.SetSize(rows,columns);
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::SetMatrixData(unsigned int row_index, unsigned int column_index, const TData item)
{
  this->m_Matrix[row_index][column_index] = item;
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::RowHeadersPushBack(const std::string & header)
{
  this->m_RowHeaders.push_back(header);
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::ColumnHeadersPushBack(const std::string & header)
{
  this->m_ColumnHeaders.push_back(header);
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::EraseFirstColumnHeader()
{
  this->m_ColumnHeaders.erase(this->m_ColumnHeaders.begin() );
}

template <typename TData>
void
CSVArray2DDataObject<TData>
::FillMatrix(TData value)
{
  this->m_Matrix.Fill(value);
}


} //end namespace itk

#endif
