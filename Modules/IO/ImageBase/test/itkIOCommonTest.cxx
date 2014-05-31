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

#include "itkIOCommon.h"
#include <cstring>
#include "itksys/SystemTools.hxx"

bool CheckFileNameParsing(const std::string & fileName,
                          const std::string & correctNameOnly,
                          const std::string & correctExtension,
                          const std::string & correctPath)
{
  // the current kwsys way...
  std::cout << "(kwsys) Extracting...file name...";
  char* nameOnly;
  {
    std::string fileNameString =
      itksys::SystemTools::GetFilenameWithoutLastExtension(
        itksys::SystemTools::GetFilenameName(fileName));
    nameOnly = new char[fileNameString.size() + 1];
    std::strncpy(nameOnly, fileNameString.c_str(), fileNameString.size() + 1);
  }

  std::cout << "extension...";
  char* extension;
  {
    std::string extensionString =
      itksys::SystemTools::GetFilenameLastExtension(fileName);
    // NB: remove the period (kwsys leaves it on, ITK precedent was to
    // remove it)
    extension = new char[extensionString.size()+1];
    if ( !extensionString.empty() )
      {
      std::strncpy(extension, extensionString.c_str()+1, extensionString.size()+1);
      }
    else
      {
      extension[0] = '\0';
      }
  }

  std::cout << "path...";
  char* path;
  {
    std::string pathString = itksys::SystemTools::GetFilenamePath(fileName);
#ifdef _WIN32
    for (size_t i = 0; i < pathString.size(); i++)
      {
      if (pathString[i] == '/')
        {
        pathString[i] = '\\';
        }
      }
#endif
    // NB: add trailing slash iff the result is non-empty (kwsys always
    // removes it, ITK precedent was to keep it)
    if (pathString.size() > 1)
    {
  #if defined(_WIN32)
      pathString = pathString + "\\";
  #else
      pathString = pathString + "/";
  #endif
    }
    path = new char[pathString.size() + 1];
    std::strncpy(path, pathString.c_str(),pathString.size() + 1);
  }

  std::cout << "DONE" << std::endl;

  std::cout << "Comparing...file name...";
  bool nameMatches;
  if ( strlen(nameOnly) == 0 )
    {
    nameMatches = correctNameOnly.size() == 0;
    }
  else
    {
    nameMatches = correctNameOnly.compare(nameOnly) == 0;
    }

  std::cout << "extension...";
  bool extensionMatches;
  if ( strlen(extension) == 0 )
    {
    extensionMatches = correctExtension.size() == 0;
    }
  else
    {
    extensionMatches = correctExtension.compare(extension) == 0;
    }

  std::cout << "path...";
  bool pathMatches;
  if ( strlen(path) == 0 )
    {
    pathMatches = correctPath.size() == 0;
    }
  else
    {
    pathMatches = correctPath.compare(path) == 0;
    }
  std::cout << "DONE" << std::endl;

  std::cout << "FullFileName: \"" << fileName << "\"" << std::endl;
  std::cout << "FileName: (expected) \"" << correctNameOnly
            << "\" (actual) \"" << static_cast<const char *>(nameOnly)
            << "\""
            << " (correct) " << nameMatches << std::endl;
  std::cout << "Extension: (expected) \"" << correctExtension
            << "\" (actual) \"" << static_cast<const char *>(extension)
            << "\""
            << " (correct) " << extensionMatches << std::endl;
  std::cout << "Path: (expected) \"" << correctPath
            << "\" (actual) \"" << static_cast<const char *>(path)
            << "\""
            << " (correct) " << pathMatches << std::endl;

  bool correctParse = nameMatches && extensionMatches && pathMatches;
  std::cout << "Parsing is " << (correctParse ? "correct" : "incorrect")
            << std::endl;

  // clean up
  std::cout << "Cleaning up...";
  delete[] nameOnly;
  delete[] extension;
  delete[] path;
  std::cout << "DONE" << std::endl;

  return correctParse;
}

int itkIOCommonTest(int , char* [])
{
  bool success = true;

  //
  // reasonable cases
  //

#if defined(_WIN32)
  success = success &&
    CheckFileNameParsing("c:\\dir1\\dir2\\myfile.tar.gz",
                         "myfile.tar",
                         "gz",
                         "c:\\dir1\\dir2\\");
  success = success &&
    CheckFileNameParsing("\\\\sambaserver\\dir1\\dir2\\myfile.tar.gz",
                         "myfile.tar",
                         "gz",
                         "\\\\sambaserver\\dir1\\dir2\\");
#else
  success = success &&
    CheckFileNameParsing("/dir1/dir2/myfile.tar.gz",
                         "myfile.tar",
                         "gz",
                         "/dir1/dir2/");
#endif

  //
  // less reasonable cases
  //
  success = success &&
    CheckFileNameParsing(".", "", "", "");

#if defined(_WIN32)
  success = success &&
    CheckFileNameParsing("\\", "", "", "\\");
  success = success &&
    CheckFileNameParsing("\\.tar.gz", ".tar", "gz", "\\");
#else
  success = success &&
    CheckFileNameParsing("/", "", "", "/");
  success = success &&
    CheckFileNameParsing("/.tar.gz", ".tar", "gz", "/");
#endif

#if defined(_WIN32)
  success = success &&
    CheckFileNameParsing("\\.tar.gz", ".tar", "gz", "\\");
#endif

  success = success &&
    CheckFileNameParsing(".tar.gz", ".tar", "gz", "");

  success = success &&
    CheckFileNameParsing("myfile", "myfile", "", "");

#if defined(_WIN32)
  success = success &&
    CheckFileNameParsing("\\myfile", "myfile", "", "\\");
#else
  success = success &&
    CheckFileNameParsing("/myfile", "myfile", "", "/");
#endif

  success = success &&
    CheckFileNameParsing("", "", "", "");

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
