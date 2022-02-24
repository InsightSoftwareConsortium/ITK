/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmGlobal.h"
#include "gdcmDicts.h"
#include "gdcmDefs.h"
#include "gdcmFilename.h"

#include <limits.h> // PATH_MAX
#include <string.h> // strcpy
#ifdef _WIN32
#include <windows.h> // MAX_PATH
#endif

namespace gdcm
{

// Must NOT be initialized.  Default initialization to zero is
// necessary.
unsigned int GlobalCount;

class GlobalInternal
{
public:
  GlobalInternal():GlobalDicts(),GlobalDefs() {}
  Dicts GlobalDicts; // Part 6 + Part 4 elements
// TODO need H table for TransferSyntax / MediaStorage / Part 3 ...
  Defs GlobalDefs;

  // Resource paths:
  // By default only construct two paths:
  // - The official install dir (need to keep in sync with cmakelist variable
  // - a dynamic one, so that gdcm is somewhat rellocatable
  // - on some system where it make sense the path where the Resource should be located
  void LoadDefaultPaths()
    {
    assert( ResourcePaths.empty() );
    const char filename2[] = GDCM_CMAKE_INSTALL_PREFIX "/" GDCM_INSTALL_DATA_DIR "/XML/";
    ResourcePaths.emplace_back(filename2 );
    const char filename3[] = GDCM_CMAKE_INSTALL_PREFIX " " GDCM_API_VERSION "/" GDCM_INSTALL_DATA_DIR "/XML/";
    ResourcePaths.emplace_back(filename3 );
    const char *curprocfn = System::GetCurrentProcessFileName();
    if( curprocfn )
      {
      Filename fn( curprocfn );
      std::string str = fn.GetPath();
      str += "/../" GDCM_INSTALL_DATA_DIR "/XML/";
      ResourcePaths.push_back( str );
      }
    const char *respath = System::GetCurrentResourcesDirectory();
    if( respath )
      {
      ResourcePaths.emplace_back(respath );
      }
#ifdef GDCM_BUILD_TESTING
    // Needed for backward compat and dashboard
    const char src_path[] = GDCM_SOURCE_DIR "/Source/InformationObjectDefinition/";
    ResourcePaths.emplace_back(src_path );
    std::rotate(ResourcePaths.rbegin(), ResourcePaths.rbegin() + 1, ResourcePaths.rend());
#endif
    }
  std::vector<std::string> ResourcePaths;
};

Global::Global()
{
  if(++GlobalCount == 1)
    {
    assert( Internals == nullptr ); // paranoid
    Internals = new GlobalInternal;
    assert( Internals->GlobalDicts.IsEmpty() );
    // Fill in with default values now !
    // at startup time is safer as later call might be from different thread
    // thus initialization of std::map would be all skrew up
    Internals->GlobalDicts.LoadDefaults();
    assert( Internals->GlobalDefs.IsEmpty() );
    // Same goes for GlobalDefs:
    //Internals->GlobalDefs.LoadDefaults();
    Internals->LoadDefaultPaths();
    }
}

Global::~Global()
{
  if(--GlobalCount == 0)
    {
    //Internals->GlobalDicts.Unload();
    delete Internals;
    Internals = nullptr; // paranoid
    }
}

bool Global::LoadResourcesFiles()
{
  assert( Internals != nullptr ); // paranoid
  const char *filename = Locate( "Part3.xml" );
  if( filename )
    {
    if( Internals->GlobalDefs.IsEmpty() )
      Internals->GlobalDefs.LoadFromFile(filename);
    return true;
    }
  // resource manager was not set properly
  return false;
}

bool Global::Append(const char *path)
{
  if( !System::FileIsDirectory(path) )
    {
    return false;
    }
  Internals->ResourcePaths.emplace_back(path );
  return true;
}

bool Global::Prepend(const char *path)
{
  if( !System::FileIsDirectory(path) )
    {
    return false;
    }
  Internals->ResourcePaths.insert( Internals->ResourcePaths.begin(), path );
  return true;
}

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

const char *Global::Locate(const char *resfile) const
{
#ifdef _WIN32
  static char path[MAX_PATH];
#else
  static char path[PATH_MAX];
#endif

  std::vector<std::string>::const_iterator it = Internals->ResourcePaths.begin();
  for( ; it != Internals->ResourcePaths.end(); ++it)
    {
    const std::string &p = *it;
    gdcmDebugMacro( "Trying to locate in: " << p );
    std::string fullpath = p + "/" + resfile;
    if( System::FileExists(fullpath.c_str()) )
      {
      // we found a match
      // check no invalid write access possible:
      if( fullpath.size() >= sizeof(path) )
        {
        gdcmDebugMacro( "Impossible happen: path is too long" );
        return nullptr;
        }
      strcpy(path, fullpath.c_str() );
      return path;
      }
    }
  // no match sorry  :(
  return nullptr;
}

Dicts const &Global::GetDicts() const
{
  assert( !Internals->GlobalDicts.IsEmpty() );
  return Internals->GlobalDicts;
}

Dicts &Global::GetDicts()
{
  assert( !Internals->GlobalDicts.IsEmpty() );
  return Internals->GlobalDicts;
}

Defs const &Global::GetDefs() const
{
  assert( !Internals->GlobalDefs.IsEmpty() );
  return Internals->GlobalDefs;
}

Global& Global::GetInstance()
{
  return GlobalInstance;
}

// Purposely not initialized.  ClassInitialize will handle it.
GlobalInternal * Global::Internals;


} // end namespace gdcm
