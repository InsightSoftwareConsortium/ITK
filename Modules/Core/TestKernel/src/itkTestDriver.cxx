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


/* Select the environment variable holding the shared library runtime
   search path for this platform.  */

/* Linux */
#if defined(__linux) || defined(__FreeBSD__) || defined(__OpenBSD__)
# define ITK_TEST_DRIVER_LDPATH "LD_LIBRARY_PATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH

/* OSX */
#elif defined(__APPLE__)
# define ITK_TEST_DRIVER_LDPATH "DYLD_LIBRARY_PATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH

/* AIX */
#elif defined(_AIX)
# define ITK_TEST_DRIVER_LDPATH "LIBPATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH

/* SUN */
#elif defined(__sun)
#  define ITK_TEST_DRIVER_LDPATH "LD_LIBRARY_PATH"
#  define ITK_TEST_DRIVER_LDPATH64 "LD_LIBRARY_PATH_64"

/* HP-UX */
#elif defined(__hpux)
#  define ITK_TEST_DRIVER_LDPATH "SHLIB_PATH"
#  define ITK_TEST_DRIVER_LDPATH64 "LD_LIBRARY_PATH"

/* SGI MIPS */
#elif defined(__sgi) && defined(_MIPS_SIM)
# if _MIPS_SIM == _ABIO32
#  define ITK_TEST_DRIVER_LDPATH "LD_LIBRARY_PATH"
# elif _MIPS_SIM == _ABIN32
#  define ITK_TEST_DRIVER_LDPATH "LD_LIBRARYN32_PATH"
# endif
#  define ITK_TEST_DRIVER_LDPATH64 "LD_LIBRARY64_PATH"

/* Cygwin */
#elif defined(__CYGWIN__)
# define ITK_TEST_DRIVER_LDPATH "PATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH

/* Windows */
#elif defined(_WIN32)
# define ITK_TEST_DRIVER_LDPATH "PATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH

/* Guess on this unknown system.  */
#else
# define ITK_TEST_DRIVER_LDPATH "LD_LIBRARY_PATH"
# define ITK_TEST_DRIVER_LDPATH64 ITK_TEST_DRIVER_LDPATH
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
# define ITK_TEST_DRIVER_PATH_SEP ';'
# define ITK_TEST_DRIVER_PATH_SLASH '\\'
#else
# define ITK_TEST_DRIVER_PATH_SEP ':'
# define ITK_TEST_DRIVER_PATH_SLASH '/'
#endif


void AddEntriesBeforeLibraryPath( const ArgumentsList & args )
{
  unsigned int i = 0;

  while( i < args.size() )
    {
    std::string libpath = ITK_TEST_DRIVER_LDPATH;
    libpath += "=";
    libpath += args[i];
    char *oldenv = getenv(ITK_TEST_DRIVER_LDPATH);
    if ( oldenv )
      {
      libpath += ITK_TEST_DRIVER_PATH_SEP;
      libpath += oldenv;
      }
    itksys::SystemTools::PutEnv( libpath.c_str() );
    // on some 64 bit systems, LD_LIBRARY_PATH_64 is used before
    // LD_LIBRARY_PATH if it is set. It can lead the test to load
    // the system library instead of the expected one, so this
    // var must also be set
    if ( std::string(ITK_TEST_DRIVER_LDPATH) != ITK_TEST_DRIVER_LDPATH64 )
      {
      std::string libpath64 = ITK_TEST_DRIVER_LDPATH64;
      libpath64 += "=";
      libpath64 += args[i];
      char *oldenv64 = getenv(ITK_TEST_DRIVER_LDPATH64);
      if ( oldenv64 )
        {
        libpath64 += ITK_TEST_DRIVER_PATH_SEP;
        libpath64 += oldenv64;
        }
      itksys::SystemTools::PutEnv( libpath64.c_str() );
      }

    i++;
    }
}


void AddEntriesBeforeEnvironment( const ArgumentsList & args )
{
  unsigned int i = 0;

  while( i < args.size() )
    {
    std::string env = args[i];
    env += "=";
    env += args[i+1];
    char *oldenv = getenv( args[i] );
    if ( oldenv )
      {
      env += ITK_TEST_DRIVER_PATH_SEP;
      env += oldenv;
      }
    itksys::SystemTools::PutEnv( env.c_str() );

    i += 2;
    }
}


void AddEntriesBeforeEnvironmentWithSeparator( const ArgumentsList & args )
{
  unsigned int i = 0;

  while( i < args.size() )
    {
    std::string env = args[i];
    env += "=";
    env += args[i+1];
    char *oldenv = getenv( args[i] );
    if ( oldenv )
      {
      env += args[i+2];
      env += oldenv;
      }
    itksys::SystemTools::PutEnv( env.c_str() );

    i += 3;
    }
}


int TestDriverInvokeProcess( const ArgumentsList & args )
{
  // a ITK_NULLPTR is required at the end of the table
  char ** argv = new char *[args.size() + 1];
  for ( unsigned int i = 0; i < args.size(); i++ )
    {
    argv[i] = args[i];
    }
  argv[args.size()] = ITK_NULLPTR;

  itksysProcess *process = itksysProcess_New();
  itksysProcess_SetCommand(process, argv);
  itksysProcess_SetPipeShared(process, itksysProcess_Pipe_STDOUT, true);
  itksysProcess_SetPipeShared(process, itksysProcess_Pipe_STDERR, true);
  itksysProcess_Execute(process);
  itksysProcess_WaitForExit(process, ITK_NULLPTR);

  delete[] argv;

  int state = itksysProcess_GetState(process);
  switch( state )
    {
    case itksysProcess_State_Error:
      {
      std::cerr << "itkTestDriver: Process error: " << itksysProcess_GetErrorString(process) << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    case itksysProcess_State_Exception:
      {
      std::cerr << "itkTestDriver: Process exception: " << itksysProcess_GetExceptionString(process) << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    case itksysProcess_State_Executing:
      {
      // this is not a possible state after itksysProcess_WaitForExit
      std::cerr << "itkTestDriver: Internal error: process can't be in Executing State." << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    case itksysProcess_State_Exited:
      {
      // this is the normal case - it is treated later
      break;
      }
    case itksysProcess_State_Expired:
      {
      // this is not a possible state after itksysProcess_WaitForExit
      std::cerr << "itkTestDriver: Internal error: process can't be in Expired State." << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    case itksysProcess_State_Killed:
      {
      std::cerr << "itkTestDriver: The process has been killed." << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    case itksysProcess_State_Disowned:
      {
      std::cerr << "itkTestDriver: Process disowned." << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    default:
      {
      // this is not a possible state after itksysProcess_WaitForExit
      std::cerr << "itkTestDriver: Internal error: unknown State." << std::endl;
      itksysProcess_Delete(process);
      return 1;
      }
    }

  int retCode = itksysProcess_GetExitValue(process);
  if ( retCode != 0 )
    {
    std::cerr << "itkTestDriver: Process exited with return value: " << retCode << std::endl;
    }
  itksysProcess_Delete(process);
return retCode;
}


int main(int ac, char *av[])
{
  try
    {
    RegisterRequiredFactories();
    }
  catch( itk::ExceptionObject & err )
    {
    err.Print(std::cerr);
    return EXIT_FAILURE;
    }


  ProcessedOutputType po;

  int result = ProcessArguments(&ac, &av, &po);

  if( result )
    {
    // There was a problem parsing the arguments, so usage has already
    // been printed, just return
    return 1;
    }

  if ( po.externalProcessMustBeCalled && po.args.empty() )
    {
    usage();
    return 1;
    }

  if ( !po.externalProcessMustBeCalled && !po.args.empty() )
    {
    usage();
    return 1;
    }


  if ( !result && po.externalProcessMustBeCalled )
    {
    AddEntriesBeforeLibraryPath( po.add_before_libpath );

    AddEntriesBeforeEnvironment( po.add_before_env );

    AddEntriesBeforeEnvironmentWithSeparator( po.add_before_env_with_sep );

    result = TestDriverInvokeProcess( po.args );
    }

  if (result == 0)
    {
    #include "itkTestDriverBeforeTest.inc"
    #include "itkTestDriverAfterTest.inc"
    }

  return result;
}
