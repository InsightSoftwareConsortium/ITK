/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkFFTWGlobalConfiguration_h
#define itkFFTWGlobalConfiguration_h

#include "itkObject.h"
// NOTE:  Need to have at least one itk include before
//       the next defines in order to have ITK_USE_FFTWF,ITK_USE_FFTWD defined
#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#  include "ITKFFTExport.h"
#  include <mutex>
#  include "itkSingletonMacro.h"
#  include "itksys/SystemTools.hxx"
#  include "itksys/SystemInformation.hxx"
#  if defined(ITK_USE_CUFFTW)
#    include "cufftw.h"
#  else
#    include "fftw3.h"
#  endif
#  include <algorithm>
#  include <cctype>

struct FFTWGlobalConfigurationGlobals;

//* The fftw utilities help control the various strategies
// available for controlling optimizations for the FFTW library.
//
// Environmental variables:
// ITK_FFTW_PLAN_RIGOR   - Defines how aggressive the generation of
//                             wisdom should be.
// ITK_FFTW_READ_WISDOM_CACHE  - Defines if a wisdom file cache should
//                              be read if found.  (it is "On" by default)
// ITK_FFTW_WRITE_WISDOM_CACHE  - Defines if generated wisdom file cache
//                               should be written (it is "Off" by default)
// ITK_FFTW_WISDOM_CACHE_BASE - Defines the base directory where the
//                             fftw wisdom cache will be placed,
//                             this is intended to be used with auto-
//                             generated cache file names
// ITK_FFTW_WISDOM_CACHE_FILE - Defines the full name of the cache
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

struct FFTWGlobalConfigurationGlobals;

#  ifdef _WIN32
#    define FFTWPathSep "\\"
#  else
#    define FFTWPathSep "/"
#  endif

class ITKFFT_EXPORT WisdomFilenameGeneratorBase
{
public:
  // The baseCacheDirectory from which to build the cache hierarchy
  virtual std::string
  GenerateWisdomFilename(const std::string & baseCacheDirectory) const = 0;
  WisdomFilenameGeneratorBase();
  virtual ~WisdomFilenameGeneratorBase();

private:
};

class ITKFFT_EXPORT ManualWisdomFilenameGenerator : public WisdomFilenameGeneratorBase
{
public:
  ManualWisdomFilenameGenerator(std::string wfn);
  void
  SetWisdomFilename(const std::string & wfn);
  std::string
  GenerateWisdomFilename(const std::string & baseCacheDirectory) const override;

private:
  std::string m_WisdomFilename;
};

class ITKFFT_EXPORT SimpleWisdomFilenameGenerator : public WisdomFilenameGeneratorBase
{
public:
  std::string
  GenerateWisdomFilename(const std::string & baseCacheDirectory) const override;
};

class ITKFFT_EXPORT HostnameWisdomFilenameGenerator : public WisdomFilenameGeneratorBase
{
public:
  std::string
  GenerateWisdomFilename(const std::string & baseCacheDirectory) const override;
};

class ITKFFT_EXPORT HardwareWisdomFilenameGenerator : public WisdomFilenameGeneratorBase
{
public:
  HardwareWisdomFilenameGenerator();

  std::string
  GenerateWisdomFilename(const std::string & baseCacheDirectory) const override;

  void
  SetUseOSName(const bool flag);
  void
  SetUseOSRelease(const bool flag);
  void
  SetUseOSVersion(const bool flag);
  void
  SetUseOSPlatform(const bool flag);
  void
  SetUseOSBitSize(const bool flag);
  void
  SetUseNumberOfProcessors(const bool flag);
  void
  SetUseVendorString(const bool flag);
  void
  SetUseTypeID(const bool flag);
  void
  SetUseFamilyID(const bool flag);
  void
  SetUseModelID(const bool flag);
  void
  SetUseSteppingCode(const bool flag);

  bool
  GetUseOSName() const;
  bool
  GetUseOSRelease() const;
  bool
  GetUseOSVersion() const;
  bool
  GetUseOSPlatform() const;
  bool
  GetUseOSBitSize() const;
  bool
  GetUseNumberOfProcessors() const;
  bool
  GetUseVendorString() const;
  bool
  GetUseTypeID() const;
  bool
  GetUseFamilyID() const;
  bool
  GetUseModelID() const;
  bool
  GetUseSteppingCode() const;

private:
  bool m_UseOSName{ true };
  bool m_UseOSRelease{ false };
  bool m_UseOSVersion{ false };
  bool m_UseOSPlatform{ true };
  bool m_UseOSBitSize{ true };
  bool m_UseNumberOfProcessors{ true };
  bool m_UseVendorString{ true };
  bool m_UseVendorID{ false };
  bool m_UseTypeID{ true };
  bool m_UseFamilyID{ true };
  bool m_UseModelID{ true };
  bool m_UseSteppingCode{ true };
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
 * https://www.insight-journal.org/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Hans Johnson, The University of Iowa
 *
 * \ingroup ITKFFT
 */
class ITKFFT_EXPORT FFTWGlobalConfiguration : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWGlobalConfiguration);

  /** Standard class type aliases. */
  using Self = FFTWGlobalConfiguration;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using MutexType = std::mutex;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWGlobalConfiguration, Object);

  /** Get the mutex that protects calls to FFTW functions. */
  static std::mutex &
  GetLockMutex();

  /** Set/Get whether a new wisdom is available compared to the
   * initial state. If a new wisdom is available, the wisdoms
   * may be written to the cache file
   */
  static void
  SetNewWisdomAvailable(const bool & v);
  static bool
  GetNewWisdomAvailable();

  /**
   * \brief Set the behavior of wisdom plan creation
   *
   * If the environmental variable "ITK_FFTW_PLAN_RIGOR", is set,
   * then the environmental setting overrides default settings.
   * \param v One of the FFTW planner rigor flags FFTW_ESTIMATE,
   * FFTW_MEASURE, FFTW_PATIENT, FFTW_EXHAUSTIVE
   */
  static void
  SetPlanRigor(const int & v);

  static int
  GetPlanRigor();
  static void
  SetPlanRigor(const std::string & name);

  /** Translate plan rigor name to value. An exception is sent if the name is not valid. */
  static int
  GetPlanRigorValue(const std::string & name);

  /** Translate plan rigor value to name. An exception is sent if the value is not valid. */
  static std::string
  GetPlanRigorName(const int & value);

  /**
   * \brief Set/Get the behavior of wisdom file caching
   *
   * If the environmental variable "ITK_FFTW_WRITE_WISDOM_CACHE", is set,
   * then the environmental setting overrides default settings.
   * If true, will create a wisdom file in the location
   */
  static void
  SetReadWisdomCache(const bool & v);
  static bool
  GetReadWisdomCache();

  /**
   * \brief Set/Get the behavior of wisdom file caching
   *
   * If the environmental variable "ITK_FFTW_WRITE_WISDOM_CACHE", is set,
   * then the environmental setting overrides default settings.
   * If true, will create a wisdom file in the location
   */
  static void
  SetWriteWisdomCache(const bool & v);
  static bool
  GetWriteWisdomCache();

  /**
   * \brief Define the directory where
   * the wisdom cache will be placed.
   * The environmental variable ITK_FFTW_WISDOM_CACHE_BASE
   * will override the default behavior.
   */
  static void
  SetWisdomCacheBase(const std::string & v);
  static std::string
  GetWisdomCacheBase();

  /**
   * \brief allows application developers
   * to create arbitrary rules for auto-generating
   * cache file names.  A default cache strategy is set
   * to generate separate cache files for each unique
   * operating system and hardware permutation.  Alternate
   * representative strategies are available to meet common
   * use cases.
   * \sa HardwareWisdomFilenameGenerator
   * \sa SimpleWisdomFilenameGenerator
   * \sa HostnameWisdomFilenameGenerator
   */
  static void
  SetWisdomFilenameGenerator(WisdomFilenameGeneratorBase * wfg);

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
  static std::string
  GetWisdomFileDefaultBaseName();

  /** Import or export some wisdom for the type double to/from a file */
  static bool
  ImportWisdomFileDouble(const std::string & fname);
  static bool
  ExportWisdomFileDouble(const std::string & fname);

  /** Import or export some wisdom for the type float to/from a file */
  static bool
  ImportWisdomFileFloat(const std::string & fname);
  static bool
  ExportWisdomFileFloat(const std::string & fname);

  /** Import or export some wisdom for the type double to/from the default file */
  static bool
  ImportDefaultWisdomFileDouble();
  static bool
  ExportDefaultWisdomFileDouble();

  /** Import or export some wisdom for the type float to/from the default file */
  static bool
  ImportDefaultWisdomFileFloat();
  static bool
  ExportDefaultWisdomFileFloat();

  /** Convenience functions to Import/Export both double and float default wisdom files */
  static bool
  ImportDefaultWisdomFile();
  static bool
  ExportDefaultWisdomFile();

private:
  FFTWGlobalConfiguration();           // This will process env variables
  ~FFTWGlobalConfiguration() override; // This will write cache file if requested.

  /** Return the singleton instance with no reference counting. */
  static Pointer
  GetInstance();

  itkGetGlobalDeclarationMacro(FFTWGlobalConfigurationGlobals, PimplGlobals);


  /** This is a singleton pattern New.  There will only be ONE
   * reference to a FFTWGlobalConfiguration object per process.
   * The single instance will be unreferenced when
   * the program exits. */
  itkFactorylessNewMacro(Self);

  static FFTWGlobalConfigurationGlobals * m_PimplGlobals;

  std::mutex  m_Lock;
  bool        m_NewWisdomAvailable{ false };
  int         m_PlanRigor{ 0 };
  bool        m_WriteWisdomCache{ false };
  bool        m_ReadWisdomCache{ true };
  std::string m_WisdomCacheBase;
  // m_WriteWisdomCache Controls the behavior of default
  // wisdom file creation policies.
  WisdomFilenameGeneratorBase * m_WisdomFilenameGenerator;
};
} // namespace itk
#endif
#endif
