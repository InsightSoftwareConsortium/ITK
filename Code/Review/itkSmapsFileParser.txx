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

#if defined(_WIN32) && !defined(__CYGWIN__)
# include <io.h>
# include <process.h>
#else
# include <unistd.h>
#endif

#include <fstream>  // std::ifstream
#include <numeric>  // std::accumulate
#include <algorithm> // std::find

namespace itk
{  
  bool ITKCommon_EXPORT ci_equal(char a, char b); // located in itkSmapsFileParser.cxx

/** Binary functor to accumulate memory usage in kB
*/
template<class TFirstType, class TMapRecordType>
struct MapRecordPlusor{
  MapRecordPlusor<TFirstType,TMapRecordType>(const char * token = "Size")
    :m_Token(token)
    {
    }

  TFirstType operator()(TFirstType first, const TMapRecordType & second)
    { 
    std::map<std::string,unsigned int>::const_iterator it = second.m_Tokens.find(m_Token);
    return first + ((it!=second.m_Tokens.end())?it->second:0); 
    }
  const char * m_Token;
};

/** Binary functor to accumulate memory usage in kB
 *  The record must match (insensitively) the filter in order to be taken into account
 */
template<class TFirstType, class TMapRecordType>
struct MapRecordConditionalPlusor{  
  MapRecordConditionalPlusor<TFirstType,TMapRecordType>(const char * filter, const char * token = "Size")
    :m_Filter(filter), m_Token(token)
  {
  }
  TFirstType operator()(TFirstType first, const TMapRecordType & second)
    {
      if ( std::search(second.m_RecordName.begin(),second.m_RecordName.end(),
        m_Filter.begin(),m_Filter.end(),ci_equal) != second.m_RecordName.end())
      {
      return MapRecordPlusor<TFirstType,TMapRecordType>(m_Token)(first,second);
      }
    return first;
    }
  std::string m_Filter;
  const char * m_Token;
};

/**              ---             MapData               ---              **/

/** MadData destructor */
template <class TMapRecord>
MapData<TMapRecord>::~MapData()
{
}

/** is the data empty ? */
template <class TMapRecord>
bool MapData<TMapRecord>::Empty()
{
  return m_Records.empty();
}
template <class TMapRecord>
void MapData<TMapRecord>::Reset()
{
  m_Records.clear();
}

/**              ---            SmapsData               ---              **/

template<class TMapRecord>
SmapsData_2_6<TMapRecord>
::~SmapsData_2_6()
{
}

template<class TSmapsRecordType> ITK_EXPORT
std::istream& 
operator>>( std::istream & smapsStream, SmapsData_2_6<TSmapsRecordType> & data)
{
  // reset the records from a previous parsing
  data.Reset();
  try
    {
    TSmapsRecordType record;
    // parse each line of the Smaps file and fill the record vector.
    while( smapsStream >> record )
      {
      data.m_Records.push_back( record );
      }
    }
  catch( ExceptionObject excp )
    {
    // in case of error, erase the records.
    data.Reset();
    // propagate the exception
    itkGenericExceptionMacro( << "The Smaps stream contains errors, can't read the memory records." );
    }
  return smapsStream;
}

template<class TMapRecord>
typename SmapsData_2_6<TMapRecord>::MemoryLoadType 
SmapsData_2_6<TMapRecord>
::GetHeapUsage()
{
  MemoryLoadType heapUsage = this->GetMemoryUsage("heap","Size");
  // in some machines, there is no [heap] record; 
  if ( heapUsage == 0 )
    {
    //use the unnamed segments instead
    heapUsage = this->GetMemoryUsage("","Size");
    }
  return heapUsage;
}

template<class TMapRecord>
typename SmapsData_2_6<TMapRecord>::MemoryLoadType 
SmapsData_2_6<TMapRecord>
::GetStackUsage()
{
  return this->GetMemoryUsage("stack","Size");
}

template<class TMapRecord>
typename SmapsData_2_6<TMapRecord>::MemoryLoadType 
SmapsData_2_6<TMapRecord>
::GetTotalMemoryUsage( )
{
  return std::accumulate(this->m_Records.begin(), this->m_Records.end(), 0, MapRecordPlusor<MemoryLoadType,TMapRecord>());
}

template<class TMapRecord>
typename SmapsData_2_6<TMapRecord>::MemoryLoadType 
SmapsData_2_6<TMapRecord>
::GetMemoryUsage( const char * filter , const char * token)
{
  return std::accumulate(this->m_Records.begin(), this->m_Records.end(), 0, MapRecordConditionalPlusor<MemoryLoadType,TMapRecord>(filter,token));
}

/**              ---            VMMapData               ---              **/

template<class TVMMapRecord>
VMMapData_10_2<TVMMapRecord>
::~VMMapData_10_2()
{
}

template<class TVMMapRecordType> ITK_EXPORT
std::istream& 
operator>>( std::istream & stream, VMMapData_10_2<TVMMapRecordType> & data)
{
  // reset the records from a previous parsing
  data.Reset();
  try
    {
    std::string line;
    // get to the Summary subsection
    while( std::getline(stream,line).good() )
      {
      if ( line.find("==== Summary for process",0) != std::string::npos )
        break;
      }
    // get to the first record
    while( std::getline(stream,line).good() )
      {
      if ( line.find("REGION TYPE",0) != std::string::npos )
        break;
      }
    // burn the line "===========             [ =======]"
    if ( !std::getline(stream,line).good() )
      itkGenericExceptionMacro( << "vmmap stream is corrupted." );
    // fill the records
    TVMMapRecordType record;
    // parse each line of the Smaps file and fill the record vector.
    while( stream >> record  )
      {
      data.m_Records.push_back( record );
      }
    }
  catch( ExceptionObject excp )
    {
    // in case of error, erase the records.
    data.Reset();
    // propagate the exception
    itkGenericExceptionMacro( << "The Smaps stream contains errors, can't read the memory records." );
    }
  return stream;
}

template<class TVMMapRecord>
typename VMMapData_10_2<TVMMapRecord>::MemoryLoadType 
VMMapData_10_2<TVMMapRecord>
::GetHeapUsage()
{
  return this->GetMemoryUsage("malloc","Size");
}

template<class TVMMapRecord>
typename VMMapData_10_2<TVMMapRecord>::MemoryLoadType 
VMMapData_10_2<TVMMapRecord>
::GetStackUsage()
{
  return this->GetMemoryUsage("stack","Size");
}

template<class TVMMapRecord>
typename VMMapData_10_2<TVMMapRecord>::MemoryLoadType 
VMMapData_10_2<TVMMapRecord>
::GetTotalMemoryUsage( )
{
  return std::accumulate(this->m_Records.begin(), this->m_Records.end(), 0, MapRecordPlusor<MemoryLoadType,TVMMapRecord>("Size"));
}

template<class TVMMapRecord>
typename VMMapData_10_2<TVMMapRecord>::MemoryLoadType 
VMMapData_10_2<TVMMapRecord>
::GetMemoryUsage( const char * filter , const char * token)
{
  return std::accumulate(this->m_Records.begin(), this->m_Records.end(), 0, MapRecordConditionalPlusor<MemoryLoadType,TVMMapRecord>(filter,token));
}

/**              ---            MapFileParser               ---              **/

template<class TMapDataType>
MapFileParser<TMapDataType>
::~MapFileParser()
{
}

template<class TMapDataType>
bool 
MapFileParser<TMapDataType>::Update()
{
  this->ReadFile(m_MapFilePath);
  return m_MapFilePath.empty();
}

template<class TMapDataType>
typename MapFileParser<TMapDataType>::MemoryLoadType 
MapFileParser<TMapDataType>
::GetHeapUsage()
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage";
    }
  return m_MapData.GetHeapUsage();
}

template<class TMapDataType>
typename MapFileParser<TMapDataType>::MemoryLoadType 
MapFileParser<TMapDataType>
::GetStackUsage()
{
  if ( m_MapData.Empty() )
    {
    std::cerr<< "Read a map file before quering memory usage" ;
    }
  return m_MapData.GetStackUsage();
}


template<class TMapDataType>
typename MapFileParser<TMapDataType>::MemoryLoadType 
MapFileParser<TMapDataType>
::GetTotalMemoryUsage( )
{ 
  if ( m_MapData.Empty() )
    {
    std::cerr<< "Read a map file before quering memory usage" ;
    }
  return m_MapData.GetTotalMemoryUsage();
}

template<class TMapDataType>
typename MapFileParser<TMapDataType>::MemoryLoadType 
MapFileParser<TMapDataType>
::GetMemoryUsage( const char * filter, const char * token )
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage" ;
    }
  return m_MapData.GetMemoryUsage(filter,token);
}

/**              ---            SmapsFileParser               ---              **/

template<class TSmapsDataType>
SmapsFileParser<TSmapsDataType>
::~SmapsFileParser()
{
}

/* SmapsFileParser implementation */
template<class TSmapsDataType>
void SmapsFileParser<TSmapsDataType>::ReadFile( const std::string &mapFileLocation)
{
   std::stringstream filename;
   filename << mapFileLocation;

  // if location is empty (default parameter), use the regular linux smaps file.
  if ( filename.str().empty() )
    {
#if defined(WIN32) || defined (_WIN32)
    itkGenericExceptionMacro( << "smaps files don't exist on this machine" );
#else
    int pid = getpid();
    filename << "/proc/" << pid << "/smaps";
#endif
    }

  // open the Smaps file
  std::ifstream inputFile( filename.str().c_str() );

  // can't find or open the Smaps file
  if ( inputFile.is_open() == false )
    {
    std::cerr<< "The smaps file " << filename.str() << " could not be open";
    return;
    }

  try
    {
    //load the file
    inputFile >> this->m_MapData;
    }
  catch( ExceptionObject excp )
    {
    // propagate the exception
    itkGenericExceptionMacro( << "The smaps file " << filename.str() << " is an invalid file or contains errors" );
    }
  this->m_MapFilePath = filename.str();
}



template<class TVMMapDataType>
VMMapFileParser<TVMMapDataType>
::~VMMapFileParser()
{
}

/* VMapFileParser implementation */
template<class TVMMapDataType>
void VMMapFileParser<TVMMapDataType>::ReadFile( const std::string &mapFileLocation )
{
  try
    {
    if ( !mapFileLocation.empty() )
      {
      // open the VMap file
      std::ifstream inputFile( mapFileLocation.c_str() );
      // can't find or open the VMap file
      if ( inputFile.is_open() == false )
        {
        itkGenericExceptionMacro( << "The VMap file " << mapFileLocation << " could not be open" );
        return;
        }
      //load the file
      inputFile >> this->m_MapData;
      this->m_MapFilePath = mapFileLocation;
      }
    else
      {
#if defined(WIN32) || defined (_WIN32)
      itkGenericExceptionMacro( << "VMMap files don't exist on this machine" );
#else
      std::stringstream vmmapCommand;
      vmmapCommand << "vmmap " << getpid();

      FILE * vmmapCommandOutput = NULL;
      if ( (vmmapCommandOutput = popen(vmmapCommand.str().c_str(), "r")) == NULL)
        {
        itkGenericExceptionMacro( << "Error using pmap. Can execute pmap command" );
        }

      // Not optimal solution: copy the output into an std::istream object. 
      std::stringstream vmmapFile;
      char buf[256];
      while ( !feof(vmmapCommandOutput) )
        {
        fgets(buf,256,vmmapCommandOutput);
        vmmapFile << buf;
        }

      fclose(vmmapCommandOutput);
      //fill the data
      vmmapFile >> this->m_MapData;
      this->m_MapFilePath = "";
#endif
      }
    }
  catch( ExceptionObject excp )
    {
    // propagate the exception
    itkGenericExceptionMacro( << "The vmmap file is an invalid file or contains errors" );
    }
}



} //end namespace itk

#endif //__itkSmapsFileParser_txx
