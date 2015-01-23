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

#ifndef itkFileTools_h
#define itkFileTools_h

namespace itk
{

/**
 * \class FileTools
 * \brief This is a helper class to provide file/directory manipulations such as file creation, directory creation, etc.
 * \ingroup ITKIOXML
 */
class FileTools
{
public:
  /** Helper function to create the directory, if it does not exist. */
  static void CreateDirectory( const char* fn );

  /** Function to create a file, if it does not exist. */
  static void CreateFile( const char* fn );
};

} // namespace itk

// here comes the implementation

#include <string>
#include "itksys/SystemTools.hxx"
#include "itkMacro.h"

namespace itk
{

/** Helper function to create the directory, if it does not exist. */
inline void
FileTools::CreateDirectory( const char* dir )
{
  if ( dir == ITK_NULLPTR || itksys::SystemTools::FileExists(dir,true) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "directory cannot be created" );
    throw eo;
    }

  // do nothing if it already exists
  std::string s(dir);
  if ( s == "" || s == "." || itksys::SystemTools::FileIsDirectory(dir) )
    {
    return;
    }

  // create it
  itksys::SystemTools::MakeDirectory( dir );

  // check successful or not
  if ( !itksys::SystemTools::FileIsDirectory(dir) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "directory cannot be created" );
    throw eo;
    }
}

/** Helper function to create a file, if it does not exist. */
inline void
FileTools::CreateFile( const char* fn )
{
  if ( fn == ITK_NULLPTR || *fn == 0 || itksys::SystemTools::FileIsDirectory(fn) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "file cannot be created" );
    throw eo;
    }

  // do nothing if it already exists
  if ( itksys::SystemTools::FileExists(fn,true) )
    {
    return;
    }

  // make sure the directory exists
  std::string dir = itksys::SystemTools::GetFilenamePath( fn );
  FileTools::CreateDirectory( dir.c_str() );

  // create the file
  itksys::SystemTools::Touch( fn, true );

  // check successful or not
  if ( !itksys::SystemTools::FileExists(fn,true) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "file cannot be created" );
    throw eo;
    }
}

} // namespace itk

#endif // itkFileTools_h
