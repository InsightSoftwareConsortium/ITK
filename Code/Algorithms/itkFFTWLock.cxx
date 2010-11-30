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
#include "itkFFTWLock.h"
#include "itksys/SystemTools.hxx"
#ifdef _WIN32
        #include <Windows.h>
        #include <sys/locking.h>
        #include <io.h>
        #include <fcntl.h>
        #include <sys/types.h>
        #include <sys/stat.h>
        #include <share.h>
#else
        #include <sys/file.h>
#endif

#if defined(USE_FFTWF) || defined(USE_FFTWD)

namespace itk
{
SimpleFastMutexLock    FFTWLock::m_Lock;
FFTWLock               FFTWLock::m_Singleton;
bool                   FFTWLock::m_NewWisdomAvailable;
int                    FFTWLock::m_GlobalOptimizationLevel;

FFTWLock
::FFTWLock()
{
  // std::cout << "======== init fftw stuff =========" << std::endl;
  // initialize static stuff
  m_NewWisdomAvailable = false;

  SetGlobalOptimizationLevel(ESTIMATE); // the default value
  std::string opt;
  if( itksys::SystemTools::GetEnv("ITK_FFTW_OPTIMIZATION_LEVEL", opt) )
    {
    if( opt == "PATIENT" )
      {
      SetGlobalOptimizationLevel(PATIENT);
      }
    else if( opt == "EXHAUSTIVE" )
      {
      SetGlobalOptimizationLevel(EXHAUSTIVE);
      }
    else
      {
      std::cerr << "Warning: Unkown FFTW optimization level: " << opt << std::endl;
      }
    }

#if defined(USE_FFTWF)
  if( !fftwf_init_threads() )
    {
    std::cerr << "Warning: Unable to initialize the FFTWF thread support" << std::endl;
    }
#endif
#if defined(USE_FFTWD)
  if( !fftw_init_threads() )
    {
    std::cerr << "Warning: Unable to initialize the FFTWD thread support" << std::endl;
    }
#endif

  std::string auto_import_env;
  if( !itksys::SystemTools::GetEnv("ITK_FFTW_WISDOM_AUTO_IMPORT", auto_import_env) ||
      ( auto_import_env != "no" &&
      auto_import_env != "NO" &&
      auto_import_env != "off" &&
      auto_import_env != "OFF" &&
      auto_import_env != "0" ) )
    {
#if defined(USE_FFTWF)
      fftwf_import_system_wisdom();
      ImportWisdomFileFloat();
#endif
#if defined(USE_FFTWD)
      fftw_import_system_wisdom();
      ImportWisdomFileDouble();
#endif
    }
}

FFTWLock
::~FFTWLock()
{
  // std::cout << "======== cleanup fftw stuff =========" << std::endl;
  std::string auto_export_env;
  if( m_NewWisdomAvailable && ( !itksys::SystemTools::GetEnv("ITK_FFTW_WISDOM_AUTO_IMPORT", auto_export_env) ||
      ( auto_export_env != "no" &&
      auto_export_env != "NO" &&
      auto_export_env != "off" &&
      auto_export_env != "OFF" &&
      auto_export_env != "0" ) ) )
    {
    // import the wisdom files again to be sure to not erase the wisdom saved in another process
    ImportWisdomFileFloat();
    ExportWisdomFileFloat();

    ImportWisdomFileDouble();
    ExportWisdomFileDouble();
    }
#if defined(USE_FFTWF)
  fftwf_cleanup_threads();
  fftwf_cleanup();
#endif
#if defined(USE_FFTWD)
  fftw_cleanup_threads();
  fftw_cleanup();
#endif
}

std::string
FFTWLock
::GetWisdomFileDefaultBaseName()
{
  std::string name;
  if( !itksys::SystemTools::GetEnv("ITK_FFTW_WISDOM_FILE_BASE_NAME", name) )
    {
#ifdef _WIN32
                name = std::string(itksys::SystemTools::GetEnv("HOMEPATH")) + "\\.itkwisdom";
#else
                name = std::string(itksys::SystemTools::GetEnv("HOME")) + "/.itkwisdom";
#endif
    }
  return name;
}

bool
FFTWLock
::ImportWisdomFileFloat()
{
  return ImportWisdomFileFloat( GetWisdomFileDefaultBaseName() + "f" );
}

bool
FFTWLock
::ImportWisdomFileDouble()
{
  return ImportWisdomFileDouble( GetWisdomFileDefaultBaseName() );
}

bool
FFTWLock
::ExportWisdomFileFloat()
{
  return ExportWisdomFileFloat( GetWisdomFileDefaultBaseName() + "f" );
}

bool
FFTWLock
::ExportWisdomFileDouble()
{
  return ExportWisdomFileDouble( GetWisdomFileDefaultBaseName() );
}

bool
FFTWLock
::ImportWisdomFileFloat( std::string path )
{
  bool ret = false;
#if defined(USE_FFTWF)
#ifdef _WIN32
        FILE *f;
        int  fd;
        if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD)){
                if ( (f = _fdopen(fd, "r")) != NULL ) {// strange but seems ok under VC++ not so friendly with checking the return values of affectations
                        ret = fftwf_import_wisdom_from_file( f );
                }
        _close(fd);
        }
#else
  FILE * f = fopen( path.c_str(), "r" );
  if( f )
    {
    flock( fileno(f), LOCK_SH );
    ret = fftwf_import_wisdom_from_file( f );
    flock( fileno(f), LOCK_UN );
    fclose( f );
    }
#endif
#endif
  return ret;
}

bool
FFTWLock
::ImportWisdomFileDouble( std::string path )
{
  bool ret = false;
#if defined(USE_FFTWD)
  #ifdef _WIN32
        FILE *f;
        int  fd;
        if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD)){
                if ( (f = _fdopen(fd, "r")) != NULL ) {// strange but seems ok under VC++
                        ret = fftwf_import_wisdom_from_file( f );
                }
        _close(fd);
        }
#else
  FILE * f = fopen( path.c_str(), "r" );
  if( f )
    {
    flock( fileno(f), LOCK_SH );
    ret = fftw_import_wisdom_from_file( f );
    flock( fileno(f), LOCK_UN );
    fclose( f );
    }
#endif
#endif
  return ret;
}

bool
FFTWLock
::ExportWisdomFileFloat( std::string path )
{
  bool ret = false;
#if defined(USE_FFTWF)
  #ifdef _WIN32
        FILE *f;
        int  fd;
        if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD)){
                if ( (f = _fdopen(fd, "r")) != NULL ) {// strange but seems ok under VC++
                        ret = fftwf_import_wisdom_from_file( f );
                }
        _close(fd);
        }
#else
  FILE * f = fopen( path.c_str(), "w" );
  if( f )
    {
    flock( fileno(f), LOCK_EX );
    fftwf_export_wisdom_to_file( f );
    flock( fileno(f), LOCK_UN );
    ret = fclose( f );
    }
#endif
#endif
  return ret;
}

bool
FFTWLock
::ExportWisdomFileDouble( std::string path )
{
  bool ret = false;
#if defined(USE_FFTWD)
  #ifdef _WIN32
        FILE *f;
        int  fd;
        if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD)){
                if ( (f = _fdopen(fd, "r")) != NULL ) {// strange but seems ok under VC++
                        ret = fftwf_import_wisdom_from_file( f );
                }
        _close(fd);
        }
#else
  FILE * f = fopen( path.c_str(), "w" );
  if( f )
    {
    flock( fileno(f), LOCK_EX );
    fftw_export_wisdom_to_file( f );
    flock( fileno(f), LOCK_UN );
    ret = fclose( f );
    }
#endif
#endif
  return ret;
}


void
FFTWLock
::Lock()
{
  FFTWLock::m_Lock.Lock();
}

void
FFTWLock
::Unlock()
{
  FFTWLock::m_Lock.Unlock();
}

void
FFTWLock
::NewWisdomAvailable()
{
  FFTWLock::m_NewWisdomAvailable = true;
}

int
FFTWLock
::GetGlobalOptimizationLevel()
{
  return m_GlobalOptimizationLevel;
}

void
FFTWLock
::SetGlobalOptimizationLevel(int opt)
{
  m_GlobalOptimizationLevel = opt;
}


}//end namespace itk

#endif
