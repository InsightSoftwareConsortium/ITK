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
#ifndef itkCSVNumericObjectFileWriter_hxx
#define itkCSVNumericObjectFileWriter_hxx

#include "itkCSVNumericObjectFileWriter.h"
#include "itksys/SystemTools.hxx"

#include <fstream>
#include <iomanip>


namespace itk
{
template <typename TValue, unsigned int NRows, unsigned int NColumns>
CSVNumericObjectFileWriter<TValue,NRows, NColumns>
::CSVNumericObjectFileWriter()
{
  this->m_FieldDelimiterCharacter = ',';
  this->m_InputObject = ITK_NULLPTR;
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::SetInput(const vnlMatrixType* obj)
{
  this->m_InputObject = const_cast<TValue * >( obj->data_block() );
  this->m_Rows = obj->rows();
  this->m_Columns = obj->cols();
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::SetInput(const vnlFixedMatrixType* obj)
{
  this->m_InputObject = const_cast<TValue * >( obj->data_block() );
  this->m_Rows = obj->rows();
  this->m_Columns = obj->cols();
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::SetInput(const itkMatrixType* obj)
{
  this->m_InputObject = const_cast<TValue * >(
    obj->GetVnlMatrix().data_block() );
  this->m_Rows = obj->RowDimensions;
  this->m_Columns = obj->ColumnDimensions;
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::ColumnHeadersPushBack(const std::string & header)
{
  this->m_ColumnHeaders.push_back(header);
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::RowHeadersPushBack(const std::string & header)
{
  this->m_RowHeaders.push_back(header);
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::SetColumnHeaders(const StringVectorType & columnheaders)
{
  this->m_ColumnHeaders = columnheaders;
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::SetRowHeaders(const StringVectorType & rowheaders)
{
  this->m_RowHeaders = rowheaders;
}

template <typename TValue,unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::PrepareForWriting()
{
  // throw an exception if no filename is provided
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro( << "A filename for writing was not specified!" );
    }

  // throw an exception if no input object is provided
  if ( this->m_InputObject == ITK_NULLPTR )
    {
    itkExceptionMacro( << "An input object was not specified!" );
    }

  // output a warning if the number of row headers and number of rows in the
  // object are not the same
  if ( !this->m_RowHeaders.empty()
       && (this->m_RowHeaders.size() != this->m_Rows) )
    {
    itkWarningMacro(<< "Warning: The number of row headers and the number of rows in"
              << " the input object is not consistent.");
    }

  // output a warning if the number of column headers and number of columns in
  // the object are not the same
  if( !this->m_ColumnHeaders.empty() )
    {
    if ( !this->m_RowHeaders.empty()
         && this->m_ColumnHeaders.size() != (this->m_Columns+1) )
      {
      itkWarningMacro(<< "Warning: The number of column headers and the number of"
                      << " columns in the input object is not consistent.");
      }
    if ( this->m_RowHeaders.empty()
         && this->m_ColumnHeaders.size() != this->m_Columns )
      {
      itkWarningMacro(<< "Warning: The number of column headers and the number of"
                      << " columns in the input object is not consistent.");
      }
    }
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::Write()
{
  this->PrepareForWriting();

  std::ofstream outputStream(this->m_FileName.c_str());
  if ( outputStream.fail() )
    {
      itkExceptionMacro(
        "The file " << this->m_FileName <<" cannot be opened for writing!"
        << std::endl
        << "Reason: "
        << itksys::SystemTools::GetLastSystemError() );
    }

  try
    {
    if ( !this->m_ColumnHeaders.empty() )
      {
      for (unsigned int i = 0; i < this->m_ColumnHeaders.size(); i++)
        {
        outputStream << this->m_ColumnHeaders[i];
        if ( i < this->m_ColumnHeaders.size() - 1 )
          {
          outputStream << this->m_FieldDelimiterCharacter;
          }
        }
      outputStream << std::endl;
      }
    for (unsigned int i = 0; i < this->m_Rows; i++)
      {
      if ( !this->m_RowHeaders.empty() )
        {
        if ( i < this->m_RowHeaders.size() )
          {
          outputStream << this->m_RowHeaders[i]
                       << this->m_FieldDelimiterCharacter;
          }
        }

      for (unsigned int j = 0; j < this->m_Columns; j++)
        {
        outputStream << std::setprecision(std::numeric_limits
                                          <TValue>::digits10)
                     << *(this->m_InputObject++);

        if (j < this->m_Columns - 1)
          {
          outputStream << this->m_FieldDelimiterCharacter;
          }
        }
      outputStream << std::endl;
      }
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught! " << std::endl;
    std::cerr << exp << std::endl;
    outputStream.close();
    throw exp;
    }
  catch (...)
    {
    outputStream.close();
    throw;
    }

  outputStream.close();
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::Update()
{
  this->Write();
}

template <typename TValue, unsigned int NRows, unsigned int NColumns>
void
CSVNumericObjectFileWriter<TValue,NRows,NColumns>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "File name: " << this->m_FileName << std::endl;
  os << indent << "Field Delimiter Character: "
     << this->m_FieldDelimiterCharacter << std::endl;
}


} //end namespace itk

#endif
