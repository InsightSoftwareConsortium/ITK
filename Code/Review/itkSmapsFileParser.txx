/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmapsFileParser.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSmapsFileParser_txx
#define __itkSmapsFileParser_txx

#include "itkSmapsFileParser.h"


#include <fstream>  // std::ifstream
#include <numeric>  // std::accumulate
#include <algorithm> // std::find

namespace itk
{

/** Binary functor to accumulate memory usage in kB
*/
template<class TFirstType, class TSmapRecordType>
struct SmapsRecordPlusor{
  SmapsRecordPlusor<TFirstType,TSmapRecordType>(const char * token = "Size")
    :m_Token(token)
    {
    }

  TFirstType operator()(TFirstType first, const TSmapRecordType & second)
    { 
    std::map<std::string,unsigned int>::const_iterator it = second.m_Tokens.find(m_Token);
    return first + ((it!=second.m_Tokens.end())?it->second:0); 
    }
  const char * m_Token;
};

/** Binary functor to accumulate memory usage in kB
 *  The record must match the filter to be taken into account
 */
template<class TFirstType, class TSmapRecordType>
struct SmapsRecordConditionalPlusor{  
  SmapsRecordConditionalPlusor<TFirstType,TSmapRecordType>(const char * filter, const char * token = "Size")
    :m_Filter(filter), m_Token(token)
  {
  }
  TFirstType operator()(TFirstType first, const TSmapRecordType & second)
    {
    if (second.m_HeaderName.find(m_Filter) != std::string::npos)
      {
      return SmapsRecordPlusor<TFirstType,TSmapRecordType>(m_Token)(first,second);
      }
    return first;
    }
  const char * m_Filter;
  const char * m_Token;
};



template<class TSmapsRecordType> ITK_EXPORT
std::istream& 
operator>>( std::istream & smapsStream, SmapsData_2_6<TSmapsRecordType> & data)
{
  // reset the records from a previous parsing
  data.Reset();
  try
    {
    TSmapsRecordType record;
    //SmapsRecord record;
    // parse each line of the Smaps file and fill the record vector.
    while( smapsStream >> record  )
      {
      data.m_Records.push_back( record );
      }
    }
  catch( ExceptionObject excp )
    {
    // erase the added records.
    data.Reset();
    // propagate the exception
    itkGenericExceptionMacro( << "The Smaps stream contains errors, can't read the memory records." );
    }
  return smapsStream;
}

template<class TSmapsRecordType>
bool 
SmapsData_2_6<TSmapsRecordType>
::Empty()
{
  return m_Records.empty();
}

template<class TSmapsRecordType>
typename SmapsData_2_6<TSmapsRecordType>::MemoryLoadType 
SmapsData_2_6<TSmapsRecordType>
::GetHeapUsage()
{
  MemoryLoadType heapUsage = this->GetMemoryUsage("heap");
  // in some machines, there is no [heap] record; 
  if ( heapUsage == 0 )
    {
    //use the unnamed segments instead
    heapUsage = this->GetMemoryUsage("");
    }
  return heapUsage;
}

template<class TSmapsRecordType>
typename SmapsData_2_6<TSmapsRecordType>::MemoryLoadType 
SmapsData_2_6<TSmapsRecordType>
::GetStackUsage()
{
  return this->GetMemoryUsage("stack");
}

template<class TSmapsRecordType>
typename SmapsData_2_6<TSmapsRecordType>::MemoryLoadType 
SmapsData_2_6<TSmapsRecordType>
::GetMemoryUsage( const char * filter , const char * token)
{
  return std::accumulate(m_Records.begin(), m_Records.end(), 0, SmapsRecordConditionalPlusor<MemoryLoadType,TSmapsRecordType>(filter,token));
}

template<class TSmapsRecordType>
typename SmapsData_2_6<TSmapsRecordType>::MemoryLoadType 
SmapsData_2_6<TSmapsRecordType>
::GetTotalMemoryUsage( )
{
  return std::accumulate(m_Records.begin(), m_Records.end(), 0, SmapsRecordPlusor<MemoryLoadType,TSmapsRecordType>());
}

template<class TSmapsRecordType>
void SmapsData_2_6<TSmapsRecordType>::Reset()
{
  m_Records.clear();
}

/* SmapsFileParser implementation */
template<class TSmapsDataType>
void SmapsFileParser<TSmapsDataType>::ReadFile( const std::string &smapsFileLocation)
{
   std::stringstream filename;
   filename << smapsFileLocation;

  // if location is empty (default parameter), use the regular linux smaps file.
  if ( filename.str().empty() )
    {
    int pid = getpid();
    filename << "/proc/" << pid << "/smaps";
    }

  // open the Smaps file
  std::ifstream inputFile( filename.str().c_str() );

  // can't find or open the Smaps file
  if ( inputFile.is_open() == false )
    {
    itkGenericExceptionMacro( << "The smaps file " << filename << " could not be open" );
    return;
    }

  try
    {
    //load the file
    inputFile >> m_SmapsData;
    }
  catch( ExceptionObject excp )
    {
    // propagate the exception
    itkGenericExceptionMacro( << "The smaps file " << filename << " is an invalid file or contains errors" );
    }
  m_SmapsFilePath = filename.str();
}

template<class TSmapsDataType>
bool SmapsFileParser<TSmapsDataType>::Update()
{
  if (m_SmapsFilePath.empty())
    {
    return false;
    }
  this->ReadFile(m_SmapsFilePath);
  return true;
}

template<class TSmapsDataType>
typename SmapsFileParser<TSmapsDataType>::MemoryLoadType 
SmapsFileParser<TSmapsDataType>
::GetHeapUsage()
{
  if ( m_SmapsData.Empty() )
    {
    itkGenericExceptionMacro( << "Read a smaps file before quering memory usage" );
    }
  return m_SmapsData.GetHeapUsage();
}

template<class TSmapsDataType>
typename SmapsFileParser<TSmapsDataType>::MemoryLoadType 
SmapsFileParser<TSmapsDataType>
::GetStackUsage()
{
  if ( m_SmapsData.Empty() )
    {
    itkGenericExceptionMacro( << "Read a smaps file before quering memory usage" );
    }
  return m_SmapsData.GetStackUsage();
}

template<class TSmapsDataType>
typename SmapsFileParser<TSmapsDataType>::MemoryLoadType 
SmapsFileParser<TSmapsDataType>
::GetMemoryUsage( const char * filter, const char * token )
{
  if ( m_SmapsData.Empty() )
    {
    itkGenericExceptionMacro( << "Read a smaps file before quering memory usage" );
    }
  return m_SmapsData.GetMemoryUsage(filter,token);
}

template<class TSmapsDataType>
typename SmapsFileParser<TSmapsDataType>::MemoryLoadType 
SmapsFileParser<TSmapsDataType>
::GetTotalMemoryUsage( )
{ 
  if ( m_SmapsData.Empty() )
    {
    itkGenericExceptionMacro( << "Read a smaps file before quering memory usage" );
    }
  return m_SmapsData.GetTotalMemoryUsage();
}

} //end namespace itk

#endif //__itkSmapsFileParser_txx
