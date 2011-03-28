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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include "itkTestDriverIncludeRequiredIOFactories.h"

// define some itksys* things to make ShareForward.h happy
#define itksys_SHARED_FORWARD_DIR_BUILD ""
#define itksys_SHARED_FORWARD_PATH_BUILD ""
#define itksys_SHARED_FORWARD_PATH_INSTALL ""
#define itksys_SHARED_FORWARD_EXE_BUILD ""
#define itksys_SHARED_FORWARD_EXE_INSTALL ""

// include SharedForward to avoid duplicating the code which find the
// library path variable name and the path separator
#include "itksys/SharedForward.h"
#include "itksys/Process.h"


int main(int ac, char *av[])
{
  RegisterRequiredFactories();

  // parse the command line
  int  i = 1;
  bool skip = false;
  while ( i < ac )
    {
    if ( !skip && strcmp(av[i], "--add-before-libpath") == 0 )
      {
      if ( i + 1 >= ac )
        {
        usage();
        return 1;
        }
      std::string libpath = KWSYS_SHARED_FORWARD_LDPATH;
      libpath += "=";
      libpath += av[i + 1];
      char *oldenv = getenv(KWSYS_SHARED_FORWARD_LDPATH);
      if ( oldenv )
        {
        libpath += KWSYS_SHARED_FORWARD_PATH_SEP;
        libpath += oldenv;
        }
      itksys::SystemTools::PutEnv( libpath.c_str() );
      // on some 64 bit systems, LD_LIBRARY_PATH_64 is used before
      // LD_LIBRARY_PATH if it is set. It can lead the test to load
      // the system library instead of the expected one, so this
      // var must also be set
      if ( std::string(KWSYS_SHARED_FORWARD_LDPATH) == "LD_LIBRARY_PATH" )
        {
        std::string libpath64 = "LD_LIBRARY_PATH_64";
        libpath64 += "=";
        libpath64 += av[i + 1];
        char *oldenv64 = getenv("LD_LIBRARY_PATH_64");
        if ( oldenv64 )
          {
          libpath64 += KWSYS_SHARED_FORWARD_PATH_SEP;
          libpath64 += oldenv64;
          }
        itksys::SystemTools::PutEnv( libpath64.c_str() );
        }
      av += 2;
      ac -= 2;
      }
    else if ( !skip && strcmp(av[i], "--add-before-env") == 0 )
      {
      if ( i + 2 >= ac )
        {
        usage();
        return 1;
        }
      std::string env = av[i + 1];
      env += "=";
      env += av[i + 2];
      char *oldenv = getenv(av[i + 1]);
      if ( oldenv )
        {
        env += KWSYS_SHARED_FORWARD_PATH_SEP;
        env += oldenv;
        }
      itksys::SystemTools::PutEnv( env.c_str() );
      av += 3;
      ac -= 3;
      }
    else if ( !skip && strcmp(av[i], "--add-before-env-with-sep") == 0 )
      {
      if ( i + 3 >= ac )
        {
        usage();
        return 1;
        }
      std::string env = av[i + 1];
      env += "=";
      env += av[i + 2];
      char *oldenv = getenv(av[i + 1]);
      if ( oldenv )
        {
        env += av[i + 3];
        env += oldenv;
        }
      itksys::SystemTools::PutEnv( env.c_str() );
      av += 4;
      ac -= 4;
      }
    else
      {
      i += 1;
      }
    }

 int result = ProcessArguments(&ac, &av);

 if (result == 0)
   {
   #include "itkTestDriverBeforeTest.inc"
   #include "itkTestDriverAfterTest.inc"
   }


  // We call this function here just to silence a warning.
  // It should not have any effect on the execution of the test driver.
  itksys_shared_forward_to_real(0, NULL);

  return result;
}
