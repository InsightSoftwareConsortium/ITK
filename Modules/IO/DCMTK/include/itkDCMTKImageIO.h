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
#ifndef __itkDCMTKImageIO_h
#define __itkDCMTKImageIO_h
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
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

  /** Set the DCMTK Message Logging Level */
  void SetLogLevel(LogLevel level);
  /** Get the DCMTK Message Logging Level */
  LogLevel GetLogLevel() const;

  DCMTKImageIO();
  ~DCMTKImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  DCMTKImageIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void OpenDicomImage();

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

#endif // __itkDCMTKImageIO_h
