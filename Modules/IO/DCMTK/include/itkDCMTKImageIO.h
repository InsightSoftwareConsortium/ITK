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
#ifndef itkDCMTKImageIO_h
#define itkDCMTKImageIO_h
#include "ITKIODCMTKExport.h"


#include <fstream>
#include <stdio.h>
#include "itkImageIOBase.h"

class DicomImage;

namespace itk
{
/** \class DCMTKImageIO
 *
 *  \brief Read DICOM image file format.
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIODCMTK
 */
class ITKIODCMTK_EXPORT DCMTKImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef DCMTKImageIO              Self;
  typedef ImageIOBase               Superclass;
  typedef SmartPointer< Self >      Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DCMTKImageIO, ImageIOBase);

  /** enum for DCMTK log level.  These are defined here without
   *  reference to DCMTK library enumerations, to avoid including
   * dcmtk headers in this header.
   */
  enum LogLevel
  {
    TRACE_LOG_LEVEL = 0,
    DEBUG_LOG_LEVEL,
    INFO_LOG_LEVEL ,
    WARN_LOG_LEVEL ,
    ERROR_LOG_LEVEL,
    FATAL_LOG_LEVEL,
    OFF_LOG_LEVEL,
  };

  /** */
  void SetDicomImagePointer( DicomImage* UserProvided)
    {
    m_DImage = UserProvided;
    m_DicomImageSetByUser = true;
    }

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Set the DCMTK Message Logging Level */
  void SetLogLevel(LogLevel level);
  /** Get the DCMTK Message Logging Level */
  LogLevel GetLogLevel() const;

  DCMTKImageIO();
  ~DCMTKImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DCMTKImageIO);

  void OpenDicomImage();

  /** Finds the correct type to call the templated function ReorderRGBValues(...) */
  void ReorderRGBValues(void *buffer, const void* data, unsigned long count, unsigned int voxel_size);
  /** Reorders RGB values in an image from color-by-plane (R1R2R3...G1G2G3...B1B2B3...)
   * to color-by-pixel (R1G1B1...R2G2B2...R3G3B3...). `voxel_size` specifies the pixel size: It should
   * be 3 for RGB images, and 4 for RGBA images.
   * The code in this function is based on code available in DCMTK dcmimage/include/dcmtk/dcmimage/dicoopxt.h
   * in the Convert(...) function.*/
  template<typename T>
  void
  ReorderRGBValues(void *buffer, const void* data, unsigned long count, unsigned int voxel_size)
  {
    T* output_buffer = static_cast<T*>(buffer);
    const T** input_buffer = static_cast<const T**>(const_cast<void *>(data));
    for (unsigned long pos = 0; pos < count; ++pos)
      {
      for (unsigned int color = 0; color < voxel_size; ++color)
        {
        *(output_buffer++)=input_buffer[color][pos];
        }
      }
  }

  /*----- internal helpers --------------------------------------------*/
  bool m_UseJPEGCodec;
  bool m_UseJPLSCodec;
  bool m_UseRLECodec;

  DicomImage* m_DImage;

  bool m_DicomImageSetByUser;

  double      m_RescaleSlope;
  double      m_RescaleIntercept;
  std::string m_LastFileName;
};
} // end namespace itk

#endif // itkDCMTKImageIO_h
