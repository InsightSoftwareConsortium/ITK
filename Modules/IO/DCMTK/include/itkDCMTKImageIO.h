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
#ifndef itkDCMTKImageIO_h
#define itkDCMTKImageIO_h
#include "ITKIODCMTKExport.h"


#include <fstream>
#include <cstdio>
#include "itkImageIOBase.h"

class DicomImage;

namespace itk
{
/**\class DCMTKImageIOEnums
 * \brief Enums used by the DCMTKImageIO class
 * \ingroup IOFilters
 * \ingroup ITKIODCMTK
 */
class DCMTKImageIOEnums
{
public:
  /**
   *\class LogLevel
   * \ingroup IOFilters
   * \ingroup ITKIODCMTK
   * enum for DCMTK log level.  These are defined here without
   *  reference to DCMTK library enumerations, to avoid including
   * dcmtk headers in this header.
   */
  enum class LogLevel : uint8_t
  {
    TRACE_LOG_LEVEL = 0,
    DEBUG_LOG_LEVEL,
    INFO_LOG_LEVEL,
    WARN_LOG_LEVEL,
    ERROR_LOG_LEVEL,
    FATAL_LOG_LEVEL,
    OFF_LOG_LEVEL,
  };
};
// Define how to print enumeration
extern ITKIODCMTK_EXPORT std::ostream &
                         operator<<(std::ostream & out, const DCMTKImageIOEnums::LogLevel value);

/**
 *\class DCMTKImageIO
 *
 *  \brief Read DICOM image file format.
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIODCMTK
 */
class ITKIODCMTK_EXPORT DCMTKImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DCMTKImageIO);

  /** Standard class type aliases. */
  using Self = DCMTKImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DCMTKImageIO, ImageIOBase);

  using LogLevelEnum = DCMTKImageIOEnums::LogLevel;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr LogLevelEnum TRACE_LOG_LEVEL = LogLevelEnum::TRACE_LOG_LEVEL;
  static constexpr LogLevelEnum DEBUG_LOG_LEVEL = LogLevelEnum::DEBUG_LOG_LEVEL;
  static constexpr LogLevelEnum INFO_LOG_LEVEL = LogLevelEnum::INFO_LOG_LEVEL;
  static constexpr LogLevelEnum WARN_LOG_LEVEL = LogLevelEnum::WARN_LOG_LEVEL;
  static constexpr LogLevelEnum ERROR_LOG_LEVEL = LogLevelEnum::ERROR_LOG_LEVEL;
  static constexpr LogLevelEnum FATAL_LOG_LEVEL = LogLevelEnum::FATAL_LOG_LEVEL;
  static constexpr LogLevelEnum OFF_LOG_LEVEL = LogLevelEnum::OFF_LOG_LEVEL;
#endif

  /** */
  void
  SetDicomImagePointer(DicomImage * UserProvided)
  {
    m_DImage = UserProvided;
    m_DicomImageSetByUser = true;
  }

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * buffer) override;

  /** Set the DCMTK Message Logging Level */
  void
  SetLogLevel(LogLevelEnum level);
  /** Get the DCMTK Message Logging Level */
  LogLevelEnum
  GetLogLevel() const;

  DCMTKImageIO();
  ~DCMTKImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  OpenDicomImage();

  /** Finds the correct type to call the templated function ReorderRGBValues(...) */
  void
  ReorderRGBValues(void * buffer, const void * data, size_t count, unsigned int voxel_size);
  /** Reorders RGB values in an image from color-by-plane (R1R2R3...G1G2G3...B1B2B3...)
   * to color-by-pixel (R1G1B1...R2G2B2...R3G3B3...). `voxel_size` specifies the pixel size: It should
   * be 3 for RGB images, and 4 for RGBA images.
   * The code in this function is based on code available in DCMTK dcmimage/include/dcmtk/dcmimage/dicoopxt.h
   * in the Convert(...) function.*/
  template <typename T>
  void
  ReorderRGBValues(void * buffer, const void * data, size_t count, unsigned int voxel_size)
  {
    auto *        output_buffer = static_cast<T *>(buffer);
    const auto ** input_buffer = static_cast<const T **>(const_cast<void *>(data));
    for (size_t pos = 0; pos < count; ++pos)
    {
      for (unsigned int color = 0; color < voxel_size; ++color)
      {
        *(output_buffer++) = input_buffer[color][pos];
      }
    }
  }

  /*----- internal helpers --------------------------------------------*/
  bool m_UseJPEGCodec;
  bool m_UseJPLSCodec;
  bool m_UseRLECodec;

  DicomImage * m_DImage;

  bool m_DicomImageSetByUser;

  double      m_RescaleSlope;
  double      m_RescaleIntercept;
  std::string m_LastFileName;
};

// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, DCMTKImageIO::LogLevelEnum value);

} // end namespace itk

#endif // itkDCMTKImageIO_h
