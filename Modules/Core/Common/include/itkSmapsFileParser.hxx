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
#ifndef itkSmapsFileParser_hxx
#define itkSmapsFileParser_hxx

#include "itkSmapsFileParser.h"

#if defined( _WIN32 )
#include <io.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#include <fstream>   // std::ifstream
#include <numeric>   // std::accumulate
#include <algorithm> // std::find

namespace itk
{
template< typename TMapDataType >
MapFileParser< TMapDataType >
::~MapFileParser()
{}

template< typename TMapDataType >
bool
MapFileParser< TMapDataType >::Update()
{
  this->ReadFile(m_MapFilePath);
  return m_MapFilePath.empty();
}

template< typename TMapDataType >
typename MapFileParser< TMapDataType >::MemoryLoadType
MapFileParser< TMapDataType >
::GetHeapUsage()
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage";
    }
  return m_MapData.GetHeapUsage();
}

template< typename TMapDataType >
typename MapFileParser< TMapDataType >::MemoryLoadType
MapFileParser< TMapDataType >
::GetStackUsage()
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage";
    }
  return m_MapData.GetStackUsage();
}

template< typename TMapDataType >
typename MapFileParser< TMapDataType >::MemoryLoadType
MapFileParser< TMapDataType >
::GetTotalMemoryUsage()
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage";
    }
  return m_MapData.GetTotalMemoryUsage();
}

template< typename TMapDataType >
typename MapFileParser< TMapDataType >::MemoryLoadType
MapFileParser< TMapDataType >
::GetMemoryUsage(const char *filter, const char *token)
{
  if ( m_MapData.Empty() )
    {
    std::cerr << "Read a map file before quering memory usage";
    }
  return m_MapData.GetMemoryUsage(filter, token);
}

//---------------------------------
//      SmapsFileParser
//---------------------------------

template< typename TSmapsDataType >
SmapsFileParser< TSmapsDataType >
::~SmapsFileParser()
{}

/* SmapsFileParser implementation */
template< typename TSmapsDataType >
void SmapsFileParser< TSmapsDataType >::ReadFile(const std::string & mapFileLocation)
{
  std::stringstream filename;

  filename << mapFileLocation;

  // if location is empty (default parameter), use the regular linux smaps file.
  if ( filename.str().empty() )
    {
#if defined( WIN32 ) || defined ( _WIN32 )
    itkGenericExceptionMacro(<< "smaps files don't exist on Windows");
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
    std::cerr << "The smaps file " << filename.str() << " could not be open";
    return;
    }

  try
    {
    //load the file
    inputFile >> this->m_MapData;
    }
  catch ( ExceptionObject excp )
    {
    // propagate the exception
    itkGenericExceptionMacro(<< "The smaps file " << filename.str() << " is an invalid file or contains errors");
    }
  this->m_MapFilePath = filename.str();
}

template< typename TVMMapDataType >
VMMapFileParser< TVMMapDataType >
::~VMMapFileParser()
{}

/* VMapFileParser implementation */
template< typename TVMMapDataType >
void VMMapFileParser< TVMMapDataType >::ReadFile(const std::string & mapFileLocation)
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
        itkGenericExceptionMacro(<< "The VMap file " << mapFileLocation << " could not be open");
        return;
        }
      //load the file
      inputFile >> this->m_MapData;
      this->m_MapFilePath = mapFileLocation;
      }
    else
      {
#if defined( WIN32 ) || defined ( _WIN32 )
      itkGenericExceptionMacro(<< "VMMap files don't exist on Windows");
#else
      std::stringstream vmmapCommand;
      vmmapCommand << "vmmap " << getpid();

      FILE *vmmapCommandOutput = ITK_NULLPTR;
      if ( ( vmmapCommandOutput = popen(vmmapCommand.str().c_str(), "r") ) == ITK_NULLPTR )
        {
        itkGenericExceptionMacro(<< "Error using pmap. Can execute pmap command");
        }

      // Not optimal solution: copy the output into an std::istream object.
      std::stringstream vmmapFile;
      char              buf[256];
      while ( !feof(vmmapCommandOutput) )
        {
        fgets(buf, 256, vmmapCommandOutput);
        vmmapFile << buf;
        }

      fclose(vmmapCommandOutput);
      //fill the data
      vmmapFile >> this->m_MapData;
      this->m_MapFilePath = "";
#endif
      }
    }
  catch ( ExceptionObject excp )
    {
    // propagate the exception
    itkGenericExceptionMacro(<< "The vmmap file is an invalid file or contains errors");
    }
}
} //end namespace itk

#endif //itkSmapsFileParser_hxx
