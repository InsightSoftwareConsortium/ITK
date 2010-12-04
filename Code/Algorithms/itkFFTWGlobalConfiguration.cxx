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
#include "itkFFTWGlobalConfiguration.h"
#if defined(USE_FFTWF) || defined(USE_FFTWD)
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

# include "itkObjectFactory.h"

namespace itk
{

static bool isAffirmativeString(std::string response)
{
  std::for_each(response.begin(),response.end(),::toupper);
  if(response == "YES" || response == "ON" || response=="1")
    {
    return true;
    }
  return false;
}

static bool isDeclineString(std::string response)
{
  std::for_each(response.begin(),response.end(),::toupper);
  if(response == "NO" || response == "OFF" || response=="0")
    {
    return true;
    }
  return false;
}

itk::SimpleFastMutexLock              itk::FFTWGlobalConfiguration::m_CreationLock;
itk::FFTWGlobalConfiguration::Pointer itk::FFTWGlobalConfiguration::m_Instance=NULL;

FFTWGlobalConfiguration::Pointer
FFTWGlobalConfiguration
::GetInstance()
{
  if( ! FFTWGlobalConfiguration::m_Instance )
    {
    FFTWGlobalConfiguration::m_CreationLock.Lock();
    //Need to make sure that during gaining access
    //to the lock that some other thread did not
    //initialize the singleton.
    if( ! FFTWGlobalConfiguration::m_Instance )
      {
      FFTWGlobalConfiguration::m_Instance= Self::New();
      if ( ! FFTWGlobalConfiguration::m_Instance )
        {
        std::ostringstream message;
        message << "itk::ERROR: " << "FFTWGlobalConfiguration"
          << " Valid FFTWGlobalConfiguration instance not created";
        ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
        throw e_; /* Explicit naming to work around Intel compiler bug.  */
        }
      }
    FFTWGlobalConfiguration::m_CreationLock.Unlock();
    }
  return FFTWGlobalConfiguration::m_Instance;
}

FFTWGlobalConfiguration
::FFTWGlobalConfiguration():m_NewWisdomAvailable(false),
  m_PlanRigor(0),
  m_WriteWisdomCache(false),
  m_ReadWisdomCache(true),
  m_WisdomCacheBase("")
{
    {//Configure default method for creating WISDOM_CACHE files
    std::string manualCacheFilename="";
    if( itksys::SystemTools::GetEnv("ITK_FFTW_WISDOM_CACHE_FILE", manualCacheFilename))
      {
      ManualWisdomFilenameGenerator * DefaultFilenameGenerator = new ManualWisdomFilenameGenerator(manualCacheFilename);
      this->m_WisdomFilenameGenerator = DefaultFilenameGenerator;
      this->m_WisdomFilenameGenerator->GenerateWisdomFilename(this->m_WisdomCacheBase);
      }
      else
      {
      //In the abscence of explicit specification, point to
      //the DefaultFilenameGenerator for creating the name
      HardwareWisdomFilenameGenerator * DefaultFilenameGenerator = new HardwareWisdomFilenameGenerator;
      this->m_WisdomFilenameGenerator = DefaultFilenameGenerator;
      }
    }
  // std::cout << "======== init fftw stuff =========" << std::endl;
  // initialize static stuff
  if( this->m_PlanRigor == 0 ) //Only interrogate system if not done before.
    {
    this->m_PlanRigor=FFTW_ESTIMATE; // the default value
    std::string fftwEnvOptimiztionString;
    if( itksys::SystemTools::GetEnv("ITK_FFTW_PLAN_RIGOR", fftwEnvOptimiztionString) )
      {
      if( fftwEnvOptimiztionString == "FFTW_MEASURE" )
        {
        this->m_PlanRigor = FFTW_MEASURE;
        }
      if( fftwEnvOptimiztionString == "FFTW_PATIENT" )
        {
        this->m_PlanRigor = FFTW_PATIENT;
        }
      else if( fftwEnvOptimiztionString == "FFTW_EXHAUSTIVE" )
        {
        this->m_PlanRigor = FFTW_EXHAUSTIVE;
        }
      else
        {
        itkWarningMacro( "Warning: Unkown FFTW PLAN RIGOR type: " << fftwEnvOptimiztionString );
        }
      }
    }

#if defined(USE_FFTWF)
  //TODO:  Investigate if this is really a warnable situation.
  //       fftw should work just fine without threads
  if( !fftwf_init_threads() )
    {
    itkWarningMacro( "Warning: Unable to initialize the FFTWF thread support" );
    }
#endif
#if defined(USE_FFTWD)
  if( !fftw_init_threads() )
    {
    itkWarningMacro( "Warning: Unable to initialize the FFTWD thread support" );
    }
#endif

    {
    //Default library behavior should be to NOT write
    //cache files in the default home account
    std::string auto_import_env;
    const bool envITK_FFTW_WRITE_WISDOM_CACHEfound=
      itksys::SystemTools::GetEnv("ITK_FFTW_WRITE_WISDOM_CACHE", auto_import_env);
    const bool envAffirmativeResponse=isAffirmativeString(auto_import_env);
    if( envITK_FFTW_WRITE_WISDOM_CACHEfound && isAffirmativeString(auto_import_env) )
      {
      this->m_WriteWisdomCache=true;
      }
    else
      {
      this->m_WriteWisdomCache=false;
      }
    }
    {
    //Default library behavior should be to NOT write
    //cache files in the default home account
    std::string auto_import_env;
    const bool envITK_FFTW_READ_WISDOM_CACHEfound=
      itksys::SystemTools::GetEnv("ITK_FFTW_READ_WISDOM_CACHE", auto_import_env);
    const bool envAffirmativeResponse=isAffirmativeString(auto_import_env);
    if( envITK_FFTW_READ_WISDOM_CACHEfound && isDeclineString(auto_import_env) )
      {
      this->m_ReadWisdomCache=false;
      }
    else
      {
      this->m_ReadWisdomCache=true;
      }
    }

  if( this->m_ReadWisdomCache )
    {
#if defined(USE_FFTWF)
    fftwf_import_system_wisdom();
    ImportDefaultWisdomFileFloat();
#endif
#if defined(USE_FFTWD)
    fftw_import_system_wisdom();
    ImportDefaultWisdomFileDouble();
#endif
    }

    {
    //If the environmental variable is set, then use it, else
    //use the requested directory
    std::string envSetPath;
    const bool WisdomCacheBaseEnvSet=itksys::SystemTools::GetEnv("ITK_FFTW_WISDOM_CACHE_BASE", envSetPath);
    if( WisdomCacheBaseEnvSet ) //The environment variable overrides application settings.
      {
      this->m_WisdomCacheBase=envSetPath;
      }
    else if( this->m_WisdomCacheBase.size() < 1 ) //Use home account if nothing specified
      {
#ifdef _WIN32
      this->m_WisdomCacheBase = std::string(itksys::SystemTools::GetEnv("HOMEPATH"));
#else
      this->m_WisdomCacheBase = std::string(itksys::SystemTools::GetEnv("HOME"));
#endif
      }
    }
}

FFTWGlobalConfiguration
::~FFTWGlobalConfiguration()
{
  //std::cout << "======== cleanup fftw stuff =========" << std::endl;
  //std::cout << " ==== " << this->m_WriteWisdomCache << std::endl;
  if( this->m_WriteWisdomCache )
    {
#if defined(USE_FFTWF)
      {
      // import the wisdom files again to be sure to not erase the wisdom saved in another process
      char * localWisdomf=fftwf_export_wisdom_to_string();
      const bool readWisdomSuccessfullyf=ImportDefaultWisdomFileFloat();//TODO:  Determine if this prevents updating of wisdom files.
      //       i.e. if wisdom is initially created as "FFTW_ESTIMATE"
      //       will subsequent "FFTW_EXHAUSTIVE" wisdom files
      //       ever overwrite the original?
      //       My reading of http://www.fftw.org/fftw3_doc/Wisdom-Import.html
      //       indicates that importing will replace the currently accumulated
      //       wisdom, and will, therefore, simply write out the
      //       same wisdom file that was just read from disk.
      //TODO:  I don't know how to test for the rigor level from the different options.
      if(!readWisdomSuccessfullyf /* || wisdom_rigor(fromDisk)  < widsom_rigor(localWisdomf) */ )
        {
        //Reset local wisdom to what was created for this run.
        const bool import_status=fftwf_import_wisdom_from_string(localWisdomf);
        }
      ExportDefaultWisdomFileFloat();
      }
#endif
#if defined(USE_FFTWD)
      {
      // import the wisdom files again to be sure to not erase the wisdom saved in another process
      char * localWisdom=fftw_export_wisdom_to_string();
      const bool readWisdomSuccessfully=ImportDefaultWisdomFileDouble();
      //TODO:  I don't know how to test for the rigor level from the different options.
      if(!readWisdomSuccessfully /* || wisdom_rigor(fromDisk)  < widsom_rigor(localWisdom) */ )
        {
        //Reset local wisdom to what was created for this run.
        const bool import_status=fftw_import_wisdom_from_string(localWisdom);
        }
      ExportDefaultWisdomFileDouble();
      }
#endif
    }
#if defined(USE_FFTWF)
  fftwf_cleanup_threads();
  fftwf_cleanup();
#endif
#if defined(USE_FFTWD)
  fftw_cleanup_threads();
  fftw_cleanup();
#endif
  if(this->m_WisdomFilenameGenerator)
    {
    delete this->m_WisdomFilenameGenerator;
    }
}

// void
// FFTWGlobalConfiguration
// ::SetWisdomFileDefaultBaseName(std::string s)
// {
//   m_WisdomFileDefaultBaseName = s;
// }


void
FFTWGlobalConfiguration
::SetWisdomFilenameGenerator( WisdomFilenameGeneratorBase * wfg)
{
  this->m_WisdomFilenameGenerator=wfg;
}

std::string
FFTWGlobalConfiguration
::GetWisdomFileDefaultBaseName()
{
  return this->m_WisdomFilenameGenerator->GenerateWisdomFilename(this->m_WisdomCacheBase);
}

bool
FFTWGlobalConfiguration
::ImportDefaultWisdomFileFloat()
{
  return ImportWisdomFileFloat( GetWisdomFileDefaultBaseName() + "f" );
}

bool
FFTWGlobalConfiguration
::ImportDefaultWisdomFileDouble()
{
  return ImportWisdomFileDouble( GetWisdomFileDefaultBaseName() );
}

bool
FFTWGlobalConfiguration
::ExportDefaultWisdomFileFloat()
{
  return ExportWisdomFileFloat( GetWisdomFileDefaultBaseName() + "f" );
}

bool
FFTWGlobalConfiguration
::ExportDefaultWisdomFileDouble()
{
  return ExportWisdomFileDouble( GetWisdomFileDefaultBaseName() );
}

bool
FFTWGlobalConfiguration
::ImportWisdomFileFloat( const std::string & path )
{
  bool ret = false;
#if defined(USE_FFTWF)
#ifdef _WIN32
  FILE *f;
  int  fd;
  if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD))
    {
    if ( (f = _fdopen(fd, "r")) != NULL )
      {// strange but seems ok under VC++ not so friendly with checking the return values of affectations
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
FFTWGlobalConfiguration
::ImportWisdomFileDouble( const std::string & path )
{
  bool ret = false;
#if defined(USE_FFTWD)
#ifdef _WIN32
  FILE *f;
  int  fd;
  if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD))
    {
    if ( (f = _fdopen(fd, "r")) != NULL )
      {// strange but seems ok under VC++
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
FFTWGlobalConfiguration
::ExportWisdomFileFloat( const std::string & path )
{
  bool ret = false;
    {
    //If necessary, make a directory for writing the file.
    const std::string directoryName=itksys::SystemTools::GetParentDirectory(path.c_str());
    itksys::SystemTools::MakeDirectory(directoryName.c_str());
    }

#if defined(USE_FFTWF)
#ifdef _WIN32
  int  fd;
  if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD))
    {
    FILE *f;
    if ( (f = _fdopen(fd, "r")) != NULL )
      {// strange but seems ok under VC++
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
FFTWGlobalConfiguration
::ExportWisdomFileDouble( const std::string & path )
{
  bool ret = false;
#if defined(USE_FFTWD)
#ifdef _WIN32
  FILE *f;
  int  fd;
  if ( !_sopen_s( &fd, path.c_str(), _O_RDONLY, _SH_DENYNO, _S_IREAD))
    {
    if ( (f = _fdopen(fd, "r")) != NULL )
      {// strange but seems ok under VC++
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
FFTWGlobalConfiguration
::Lock()
{
  this->m_Lock.Lock();
}

void
FFTWGlobalConfiguration
::Unlock()
{
  this->m_Lock.Unlock();
}

}//end namespace itk

#endif
