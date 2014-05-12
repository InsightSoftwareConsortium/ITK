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
#ifndef __itkFFTWGlobalConfiguration_h
#define __itkFFTWGlobalConfiguration_h

#include "itkObject.h"
//NOTE:  Need to have at least one itk include before
//       the next defines in order to have ITK_USE_FFTWF,ITK_USE_FFTWD defined
#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#include "itkSimpleFastMutexLock.h"

#include "itksys/SystemTools.hxx"
#include "itksys/SystemInformation.hxx"
#include "fftw3.h"
#include <algorithm>
#include <cctype>

//* The fftw utilities help control the various strategies
//available for controlling optimizations for the FFTW library.
//
//Environmental variables:
//ITK_FFTW_PLAN_RIGOR   - Defines how aggressive the generation of
//                             wisdom should be.
//ITK_FFTW_READ_WISDOM_CACHE  - Defines if a wisdom file cache should
//                              be read if found.  (it is "On" by default)
//ITK_FFTW_WRITE_WISDOM_CACHE  - Defines if generated wisdom file cache
//                               should be written (it is "Off" by default)
//ITK_FFTW_WISDOM_CACHE_BASE - Defines the base directory where the
//                             fftw wisdom cache will be placed,
//                             this is intended to be used with auto-
//                             generated cache file names
//ITK_FFTW_WISDOM_CACHE_FILE - Defines the full name of the cache
//                             file to be generated.  If this is
//                             set, then ITK_FFTW_WISDOM_CACHE_BASE
//                             is ignored.
//
// The above behaviors can also be controlled by the application.
//

namespace itk
{
/**
 * A set of functions for defining wisdom filename
 * generation strategies.
 */
#ifdef _WIN32
#define FFTWPathSep "\\"
#else
#define FFTWPathSep "/"
#endif

class WisdomFilenameGeneratorBase
{
  public:
    //The baseCacheDirectory from which to build the cache hierarchy
    virtual std::string GenerateWisdomFilename(const std::string baseCacheDirectory) const = 0;
    WisdomFilenameGeneratorBase() {};
    virtual ~WisdomFilenameGeneratorBase() {};
  private:
};

class ManualWisdomFilenameGenerator: public WisdomFilenameGeneratorBase
{
  public:
    ManualWisdomFilenameGenerator(const std::string wfn): m_WisdomFilename(wfn) { };
    void SetWisdomFilename(const std::string wfn)
      {
      this->m_WisdomFilename=wfn;
      }
    virtual std::string GenerateWisdomFilename(const std::string itkNotUsed(baseCacheDirectory) ) const ITK_OVERRIDE
      {
       return this->m_WisdomFilename;
      }
  private:
    std::string m_WisdomFilename;
};

class SimpleWisdomFilenameGenerator: public WisdomFilenameGeneratorBase
{
  public:
    virtual std::string GenerateWisdomFilename(const std::string baseCacheDirectory) const ITK_OVERRIDE
      {
       return baseCacheDirectory+FFTWPathSep+".itksimple.wisdom";
      }
};

class HostnameWisdomFilenameGenerator: public WisdomFilenameGeneratorBase
{
  public:
    virtual std::string GenerateWisdomFilename(const std::string baseCacheDirectory) const ITK_OVERRIDE
      {

      itksys::SystemInformation hostInfo;
      hostInfo.RunOSCheck();
      return baseCacheDirectory+FFTWPathSep + ".itkwisdomfftw"+FFTWPathSep+".itk_"+hostInfo.GetHostname()+".wisdom";
      }
};

class HardwareWisdomFilenameGenerator: public WisdomFilenameGeneratorBase
{
public:
    HardwareWisdomFilenameGenerator():
      m_UseOSName(true),
      m_UseOSRelease(false),
      m_UseOSVersion(false),
      m_UseOSPlatform(true),
      m_UseOSBitSize(true),
      m_UseNumberOfProcessors(true),
      m_UseVendorString(true),
      m_UseVendorID(false),
      m_UseTypeID(true),
      m_UseFamilyID(true),
      m_UseModelID(true),
      m_UseSteppingCode(true)
    {}

    virtual std::string GenerateWisdomFilename(const std::string baseCacheDirectory) const ITK_OVERRIDE
      {
      //Now build the hardware string by system interogation
      itksys::SystemInformation hardwareInfo;
      hardwareInfo.RunCPUCheck();
      hardwareInfo.RunOSCheck();
      hardwareInfo.RunMemoryCheck();
      std::stringstream OSD("");

      if( this->m_UseOSName )
        {
        OSD << hardwareInfo.GetOSName() << "_";
        }
      if( this->m_UseOSRelease )
        {
        OSD << hardwareInfo.GetOSRelease() << "_";
        }
      if( this->m_UseOSVersion )
        {
        OSD << hardwareInfo.GetOSVersion() << "_";
        }
      if( this->m_UseOSPlatform )
        {
        OSD << hardwareInfo.GetOSPlatform() << "_";
        }
      if( this->m_UseOSBitSize )
        {
        const char * const bitsizeString=( hardwareInfo.Is64Bits() ) ? "64_":"32_";
        OSD << bitsizeString;
        }
      if( this->m_UseNumberOfProcessors )
        {
        OSD << hardwareInfo.GetNumberOfLogicalCPU() <<"x"<< hardwareInfo.GetNumberOfPhysicalCPU() << "_";
        }
      if( this->m_UseVendorString )
        {
        OSD << hardwareInfo.GetVendorString() << "_";
        }
      if( this->m_UseVendorID )
        {
        OSD << hardwareInfo.GetVendorID() << "_";
        }
      if( this->m_UseTypeID )
        {
        OSD << hardwareInfo.GetTypeID() << "_";
        }
      if( this->m_UseFamilyID )
        {
        OSD << hardwareInfo.GetFamilyID() << "_";
        }
      if( this->m_UseModelID )
        {
        OSD << hardwareInfo.GetModelID() << "_";
        }
      if( this->m_UseSteppingCode )
        {
        OSD << hardwareInfo.GetSteppingCode();
        }
      OSD << ".wisdom";
      std::string noSpaceStr=OSD.str();
      //Now remove spaces
      noSpaceStr.erase(std::remove_if(noSpaceStr.begin(), noSpaceStr.end(),::isspace), noSpaceStr.end());
      return baseCacheDirectory + FFTWPathSep + ".itkwisdomfftw" + FFTWPathSep + noSpaceStr;
      }
    void SetUseOSName(const bool flag) { this->m_UseOSName=flag; }
    void SetUseOSRelease(const bool flag) { this->m_UseOSRelease=flag; }
    void SetUseOSVersion(const bool flag) { this->m_UseOSVersion=flag; }
    void SetUseOSPlatform(const bool flag) { this->m_UseOSPlatform=flag; }
    void SetUseOSBitSize(const bool flag) { this->m_UseOSBitSize=flag; }
    void SetUseNumberOfProcessors(const bool flag) { this->m_UseNumberOfProcessors=flag; }
    void SetUseVendorString(const bool flag) { this->m_UseVendorString=flag; }
    void SetUseTypeID(const bool flag) { this->m_UseTypeID=flag; }
    void SetUseFamilyID(const bool flag) { this->m_UseFamilyID=flag; }
    void SetUseModelID(const bool flag) { this->m_UseModelID=flag; }
    void SetUseSteppingCode(const bool flag) { this->m_UseSteppingCode=flag; }

    bool GetUseOSName() const { return this->m_UseOSName; }
    bool GetUseOSRelease() const { return this->m_UseOSRelease; }
    bool GetUseOSVersion() const { return this->m_UseOSVersion; }
    bool GetUseOSPlatform() const { return this->m_UseOSPlatform; }
    bool GetUseOSBitSize() const { return this->m_UseOSBitSize; }
    bool GetUseNumberOfProcessors() const { return this->m_UseNumberOfProcessors; }
    bool GetUseVendorString() const { return this->m_UseVendorString; }
    bool GetUseTypeID() const { return this->m_UseTypeID; }
    bool GetUseFamilyID() const { return this->m_UseFamilyID; }
    bool GetUseModelID() const { return this->m_UseModelID; }
    bool GetUseSteppingCode() const { return this->m_UseSteppingCode; }

private:
    bool m_UseOSName;
    bool m_UseOSRelease;
    bool m_UseOSVersion;
    bool m_UseOSPlatform;
    bool m_UseOSBitSize;
    bool m_UseNumberOfProcessors;
    bool m_UseVendorString;
    bool m_UseVendorID;
    bool m_UseTypeID;
    bool m_UseFamilyID;
    bool m_UseModelID;
    bool m_UseSteppingCode;
};

/**
 * \class FFTWGlobalConfiguration
 * A class to contain all the global configuration options for
 * FFTW.
 *
 * A simple global lock is included that must be called
 * before calling FFTW unsafe functions. It also handle
 * cleanly the initialization and cleanup of FFTW.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/10380/3154
 * or http://insight-journal.com/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Hans Johnson, The University of Iowa
 *
 * \ingroup ITKFFT
 */
class FFTWGlobalConfiguration: public Object
{
public:
  /** Standard class typedefs. */
  typedef FFTWGlobalConfiguration    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef SimpleFastMutexLock        MutexType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWGlobalConfiguration, Object);

  /** Get the mutex that protects calls to FFTW functions. */
  static SimpleFastMutexLock & GetLockMutex()
  {
    return GetInstance()->m_Lock;
  }

  /** Set/Get wether a new wisdom is available compared to the
   * initial state. If a new wisdom is available, the wisdoms
   * may be written to the cache file
   */
  static void SetNewWisdomAvailable( const bool & v )
  {
    GetInstance()->m_NewWisdomAvailable = v;
  }
  static bool GetNewWisdomAvailable()
  {
    return GetInstance()->m_NewWisdomAvailable;
  }

  /**
   * \brief Set the behavior of wisdom plan creation
   *
   * If the environmental variable "ITK_FFTW_PLAN_RIGOR", is set,
   * then the environmental setting overides default settings.
   * \param v One of the FFTW planner rigor flags FFTW_ESTIMATE,
   * FFTW_MEASURE, FFTW_PATIENT, FFTW_EXHAUSTIVE
   */
  static void SetPlanRigor( const int & v )
  {
    // use that method to check the value
    GetPlanRigorName( v );
    GetInstance()->m_PlanRigor = v;
  }

  static int GetPlanRigor()
  {
    return GetInstance()->m_PlanRigor;
  }
  static void SetPlanRigor( const std::string & name )
  {
    SetPlanRigor( GetPlanRigorValue( name ) );
  }

  /** Translate plan rigor name to value. An exception is sent if the name is not valid. */
  static int GetPlanRigorValue( const std::string & name );

  /** Translate plan rigor value to name. An exception is sent if the value is not valid. */
  static std::string GetPlanRigorName( const int & value );

  /**
   * \brief Set the behavior of wisdom file caching
   *
   * If the environmental variable "ITK_FFTW_WRITE_WISDOM_CACHE", is set,
   * then the environmental setting overides default settings.
   * \param v true will create a wisdom file in the location
   */
  static void SetReadWisdomCache( const bool & v )
  {
    GetInstance()->m_ReadWisdomCache = v;
  }

  static bool GetReadWisdomCache()
  {
    return GetInstance()->m_ReadWisdomCache;
  }

  /**
   * \brief Set the behavior of wisdom file caching
   *
   * If the environmental variable "ITK_FFTW_WRITE_WISDOM_CACHE", is set,
   * then the environmental setting overides default settings.
   * \param v true will create a wisdom file in the location
   */
  static void SetWriteWisdomCache( const bool & v )
  {
    GetInstance()->m_WriteWisdomCache = v;
  }

  static bool GetWriteWisdomCache()
  {
    return GetInstance()->m_WriteWisdomCache;
  }

  /**
   * \brief Define the directory where
   * the wisdom cache will be placed.
   * The environmental variable ITK_FFTW_WISDOM_CACHE_BASE
   * will override the default behavior.
   * \param v the path to the base directory name
   */
  static void SetWisdomCacheBase( const std::string & v )
  {
    GetInstance()->m_WisdomCacheBase = v;
  }

  static std::string GetWisdomCacheBase()
  {
    return GetInstance()->m_WisdomCacheBase;
  }

  /**
   * \brief allows application developers
   * to create arbitrary rules for auto-generating
   * cache file names.  A default cache strategy is set
   * to generate separate cache files for each unique
   * operating system and hardware permutation.  Alternate
   * respresentative strategies are available to meet common
   * use cases.
   * \sa HardwareWisdomFilenameGenerator
   * \sa SimpleWisdomFilenameGenerator
   * \sa HostnameWisdomFilenameGenerator
   */
  static void SetWisdomFilenameGenerator( WisdomFilenameGeneratorBase *wfg);

  /**
   * \brief
   * \return Returns the full path for the file to be written
   * if WriteWisdomCache is set to true.
   * The file name is based on the naming strategy set
   * in SetWisdomFilenameGenerator (defaults to HardwareWisdomFilenameGenerator).
   * The file generated by GetWisdomFileDefaultBaseName() (with an "f" suffix for single
   * precision wisdom files).
   * The environmental variable ITK_FFTW_WISDOM_CACHE_BASE can be used to set
   * the base directory name to place the file name specified
   * by the GetWisdomFileDefaultBaseName(). The default location is the users
   * home account directory.
   */
  static std::string GetWisdomFileDefaultBaseName();

  /** Import or export some wisdom for the type double to/from a file */
  static bool ImportWisdomFileDouble( const std::string &fname );
  static bool ExportWisdomFileDouble( const std::string &fname );

  /** Import or export some wisdom for the type float to/from a file */
  static bool ImportWisdomFileFloat( const std::string &fname );
  static bool ExportWisdomFileFloat( const std::string &fname );

  /** Import or export some wisdom for the type double to/from the default file */
  static bool ImportDefaultWisdomFileDouble();
  static bool ExportDefaultWisdomFileDouble();

  /** Import or export some wisdom for the type float to/from the default file */
  static bool ImportDefaultWisdomFileFloat();
  static bool ExportDefaultWisdomFileFloat();

private:
  FFTWGlobalConfiguration(); //This will process env variables
  ~FFTWGlobalConfiguration(); //This will write cache file if requested.

  /** Return the singleton instance with no reference counting. */
  static Pointer GetInstance();

  /** This is a singleton pattern New.  There will only be ONE
   * reference to a FFTWGlobalConfiguration object per process.
   * The single instance will be unreferenced when
   * the program exits. */
  itkFactorylessNewMacro(Self);

  FFTWGlobalConfiguration(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  static Pointer                m_Instance;
  static SimpleFastMutexLock    m_CreationLock;

  SimpleFastMutexLock           m_Lock;
  bool                          m_NewWisdomAvailable;
  int                           m_PlanRigor;
  bool                          m_WriteWisdomCache;
  bool                          m_ReadWisdomCache;
  std::string                   m_WisdomCacheBase;
  //m_WriteWisdomCache Controls the behavior of default
  //wisdom file creation policies.
  WisdomFilenameGeneratorBase * m_WisdomFilenameGenerator;
};
}
#endif
#endif
