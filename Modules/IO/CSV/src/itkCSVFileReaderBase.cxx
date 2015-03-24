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
#include "itkCSVFileReaderBase.h"

#include <fstream>

namespace itk
{

CSVFileReaderBase::CSVFileReaderBase()
{
  this->m_FileName = "";
  this->m_FieldDelimiterCharacter = ',';
  this->m_StringDelimiterCharacter = '"';
  this->m_HasColumnHeaders = true;
  this->m_HasRowHeaders = true;
  this->m_UseStringDelimiterCharacter = false;
  this->m_Line = "";
  this->m_EndOfColumnHeadersLine = 0;
}

void
CSVFileReaderBase
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "File Name: "         << this->m_FileName << std::endl;
  os << indent << "FieldDelimiterCharacter: "
     << this->m_FieldDelimiterCharacter << std::endl;
  os << indent << "StringDelimier Character: "
     << this->m_StringDelimiterCharacter << std::endl;
  os << indent << "Has Row Headers : "  << this->m_HasRowHeaders << std::endl;
  os << indent << "Has Column Headers: "
     << this->m_HasColumnHeaders << std::endl;
  os << indent << "Use of String Delimiter character: "
     << this->m_UseStringDelimiterCharacter << std::endl;
  os << indent << "Position of end of column headers: "
     << this->m_EndOfColumnHeadersLine << std::endl;
}

void
CSVFileReaderBase
::PrepareForParsing()
{
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro( << "There is no file name provided!"
                       << "Please provide a filename." );
    }

  if ( this->m_UseStringDelimiterCharacter
       && !(this->m_HasRowHeaders || this->m_HasColumnHeaders) )
    {
    itkWarningMacro( << " Use string delimiter has been set to on"
                     << "but row and/or column headers indicators are off!" );
    }

  if ( this->m_UseStringDelimiterCharacter
       && this->m_FieldDelimiterCharacter == this->m_StringDelimiterCharacter )
    {
    itkExceptionMacro( << "The same character has been set for the string"
                       << "delimiter and the field delimiter character!" );
    }
}

void
CSVFileReaderBase
::GetDataDimension(SizeValueType & rows, SizeValueType & cols)
{

  this->m_InputStream.seekg(0);
  rows = 0;
  cols = 0;
  std::string line;
  std::string cell;

  unsigned int prev_cols = 0;
  unsigned int current_cols = 0;
  unsigned int max_cols = 0;
  bool isSame = true;

  // If column headers exist,
  if ( this->m_HasColumnHeaders )
    {
    std::getline(this->m_InputStream,line);
    this->m_EndOfColumnHeadersLine = this->m_InputStream.tellg();

    // Count the number of headers
    std::stringstream linestream(line);

    if ( this->m_UseStringDelimiterCharacter )
      {
      int cellnum = 0;
      while(std::getline(linestream,cell,this->m_StringDelimiterCharacter))
        {
        if ( cellnum%2 != 0)
          {
          prev_cols++;
          }
        cellnum++;
        }
      }

    // if not using StringDelimiter, just count using the field delimiter
    // character
    else
      {
      while(std::getline(linestream,cell,this->m_FieldDelimiterCharacter))
        {
        prev_cols++;
        }
      }
    // previous columns now holds the number of headers minus the first header,
    // if there are row headers
    if ( this->m_HasRowHeaders )
      {
      prev_cols -= 1;
      }
    max_cols = prev_cols;
    }

  // Count the number of entries in each of the following lines
  while(std::getline(this->m_InputStream,line))
    {
    cols = 0;
    std::stringstream linestream(line);

    // If row headers exist, move past (but do not count) the row header in
    // each line.
    if ( this->m_HasRowHeaders )
      {
      if( this->m_UseStringDelimiterCharacter )
        {
        for (int k = 0; k < 2; k++)
          {
          std::getline(linestream,cell,this->m_StringDelimiterCharacter);
          }
        std::getline(linestream,cell, this->m_FieldDelimiterCharacter);
        }
      else
        {
        std::getline(linestream,cell, this->m_FieldDelimiterCharacter);
        }
      }

    // Count the entries
    while(std::getline(linestream,cell, this->m_FieldDelimiterCharacter))
      {
      cols++;
      }
    rows++;

    // Determine the max #columns and #rows
    current_cols = cols;
    if ( !this->m_HasColumnHeaders && rows == 1 )
      {
      prev_cols = cols;
      max_cols = cols;
      }
    if ( current_cols != prev_cols )
      {
      isSame = false;

      if ( current_cols > max_cols )
        {
        max_cols = current_cols;
        }

      prev_cols = current_cols;
      }
    }

  // If the number of entries is not consistent across each row, display a
  // warning to the user.
  if ( !isSame )
    {
    itkWarningMacro( << "Warning: Data appears to contain missing data! "
                     << "These will be set to NaN." );
    }

  cols = max_cols;
  this->m_InputStream.clear();
  this->m_InputStream.seekg(0);
}

/** Function to get the next entry from the file. */
void
CSVFileReaderBase
::GetNextField(std::string & str)
{
  /** The process below is as follows: we check if this->m_Line is empty. If it is
   * then we have to get a new line. If not, we have to get the fields that
   * this->m_Line contains. We check if the line we're on contains column headers.
   * If so, we get a column header and then delete it from this->m_Line.
   * If we're not on the column header line, we're either getting a row header
   * or a data field. If we're on a new line and row headers exist, the
   *  assumption is the first field we get from the line is the row header.
   * After that, we get data fields. Once we get a field, delete it from
   * this->m_Line. This way the next field that we need to get is always at the
   *  beginning of this->m_Line. */

  std::string empty;
  // Check that we are not at the end of the file
  itkDebugMacro( << "m_Line: " << m_Line );
  if ( !this->m_InputStream.eof() )
    {
    bool OnANewLine = false;
    bool OnColumnHeadersLine = false;
    if ( this->m_Line.empty() )
      {
      std::getline(this->m_InputStream,this->m_Line);
      OnANewLine = true;
      }

    if ( this->m_InputStream.tellg() <= this->m_EndOfColumnHeadersLine )
      {
      OnColumnHeadersLine = true;
      }

    if ( OnColumnHeadersLine )
      {
      std::istringstream linestream(this->m_Line);

      // if the string delimiter character is being used, delimit using it
      if ( this->m_UseStringDelimiterCharacter )
        {
        // move past the first string delimiter
        std::getline(linestream,empty,this->m_StringDelimiterCharacter);

        // get the string within the string delimiters
        std::getline(linestream,str,this->m_StringDelimiterCharacter);

        // move past the field delimiter
        std::getline(linestream,empty,this->m_FieldDelimiterCharacter);

        // erase this entry from this->m_Line
        this->m_Line.erase(0,str.size()+3);
        }

      // if string delimiter not being used, just delimit using the field
      // delimiter
      else
        {
        std::getline(linestream,str,this->m_FieldDelimiterCharacter);
        this->m_Line.erase(0,str.size()+1);
        }
      }

    else
      {
      /** if we're not on the column headers line, but we're on a new line, and
       * row headers exist and the string delimiter is being used. */
      if (this->m_HasRowHeaders &&
          OnANewLine &&
          this->m_UseStringDelimiterCharacter)
        {
        std::istringstream linestream(this->m_Line);

        // move past the first string delimiter
        std::getline(linestream,empty,this->m_StringDelimiterCharacter);

        // get the string within the string delimiters
        std::getline(linestream,str,this->m_StringDelimiterCharacter);

        // move past the field delimiter
        std::getline(linestream,empty,this->m_FieldDelimiterCharacter);

        // erase this entry from this->m_Line
        this->m_Line.erase(0,str.size()+3);
        OnANewLine = false;
        }

      // for any other entry, just get the next entry using the field delimiter
      else
        {
        std::istringstream linestream(this->m_Line);
        std::getline(linestream,str,this->m_FieldDelimiterCharacter);
        this->m_Line.erase(0,str.size()+1);
        }
      }
    }

  // Alert the user if end of file is reached
  else
    {
    itkExceptionMacro( << "End of file reached. No more entries" );
    }
}

} // end namespace itk
