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

#include <string>

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
  static void CreateDirectory( const std::string &fn );

  /** Function to create a file, if it does not exist. */
  static void CreateFile( const std::string &fn );

};

} // namespace itk

// here comes the implementation

#include "itksys/SystemTools.hxx"
#include "itkMacro.h"

namespace itk
{

/** Helper function to create the directory, if it does not exist. */
inline void
FileTools::CreateDirectory( const std::string &dir )
{
  if ( dir.empty() || itksys::SystemTools::FileExists(dir.c_str(),true) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "directory cannot be created" );
    throw eo;
    }

  // do nothing if it already exists
  if ( "" == dir || "." == dir || itksys::SystemTools::FileIsDirectory(dir.c_str()) )
    {
    return;
    }

  // create it
  itksys::SystemTools::MakeDirectory( dir.c_str() );

  // check successful or not
  if ( !itksys::SystemTools::FileIsDirectory( dir.c_str() ) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "directory cannot be created" );
    throw eo;
    }
}

/** Helper function to create a file, if it does not exist. */
inline void
FileTools::CreateFile( const std::string &fn )
{
  if ( fn.empty() || itksys::SystemTools::FileIsDirectory( fn.c_str() ) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "file cannot be created" );
    throw eo;
    }

  // do nothing if it already exists
  if ( itksys::SystemTools::FileExists(fn.c_str() ,true) )
    {
    return;
    }

  // make sure the directory exists
  std::string dir = itksys::SystemTools::GetFilenamePath( fn.c_str() );
  FileTools::CreateDirectory( dir );

  // create the file
  itksys::SystemTools::Touch( fn.c_str(), true );

  // check successful or not
  if ( !itksys::SystemTools::FileExists( fn.c_str(), true) )
    {
    ExceptionObject eo( __FILE__, __LINE__, "file cannot be created" );
    throw eo;
    }
}

} // namespace itk

#endif // itkFileTools_h
