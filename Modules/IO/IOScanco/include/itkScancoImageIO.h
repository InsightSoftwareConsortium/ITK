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
/*=========================================================================

  Program: DICOM for VTK

  Copyright (c) 2015 David Gobbi
  All rights reserved.
  See Copyright.txt or http://dgobbi.github.io/bsd3.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
/**
 * David's notes from vtkScancoCTReader
 * Read SCANCO ISQ and AIM medical image files
 *
 * This class reads ISQ and AIM files, which are used for high-resolution
 * computed tomography.  The information that it provides uses different
 * units as compared to the original files: all distances are given in
 * millimeters (instead of micrometers), times are given in milliseconds
 * (instead of microseconds), voltage and current given in kV and mA
 * (instead of volts and microamps).  If the scanner was calibrated, then
 * the data values can be converted to calibrated units.  To convert
 * to linear attenuation coefficients [cm^-1], simply divide the data
 * values by the MuScaling.  To convert to density values, multiply
 * the data values by the m_RescaleSlope and add the m_RescaleIntercept.
 * To convert to Hounsfield units, multiply by 1000/(MuScaling*m_MuWater)
 * and subtract 1000.
 *
 * Created at the Calgary Image Processing and Analysis Centre (CIPAC).
 */
#ifndef itkScancoImageIO_h
#define itkScancoImageIO_h
#include "IOScancoExport.h"


#include <fstream>
#include "itkImageIOBase.h"

namespace itk
{
/** \class ScancoImageIO
 *
 * \brief Read Scanco image file formats.
 *
 * Many methods are based off vtkScancoCTReader in vtk-dicom by David Gobbi
 *
 * \ingroup IOFilters
 * \ingroup IOScanco
 */
class IOScanco_EXPORT ScancoImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScancoImageIO);

  /** Standard class typedefs. */
  using Self = ScancoImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScancoImageIO, ImageIOBase);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  bool
  SupportsDimension(unsigned long dimension) override
  {
    if (dimension == 3)
    {
      return true;
    }
    return false;
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


  bool
  CanStreamRead() override
  {
    return false;
  }

  bool
  CanStreamWrite() override
  {
    return false;
  }

  /** Get a string that states the version of the file header.
   * Max size: 16 characters. */
  const char *
  GetVersion() const
  {
    return this->m_Version;
  }
  void
  SetVersion(const char * version)
  {
    strcpy(this->m_Version, version);
    this->Modified();
  }

  itkGetConstMacro(PatientIndex, int);
  itkSetMacro(PatientIndex, int);

  itkGetConstMacro(ScannerID, int);
  itkSetMacro(ScannerID, int);

  itkGetConstMacro(SliceThickness, double);
  itkSetMacro(SliceThickness, double);

  itkGetConstMacro(SliceIncrement, double);
  itkSetMacro(SliceIncrement, double);

  itkGetConstMacro(StartPosition, double);
  itkSetMacro(StartPosition, double);

  /** Set / Get the minimum and maximum values */
  const double *
  GetDataRange() const
  {
    return this->m_DataRange;
  }
  void
  SetDataRange(const double * dataRange)
  {
    this->m_DataRange[0] = dataRange[0];
    this->m_DataRange[1] = dataRange[1];
  }

  itkGetConstMacro(MuScaling, double);
  itkSetMacro(MuScaling, double);

  itkGetConstMacro(NumberOfSamples, int);
  itkSetMacro(NumberOfSamples, int);

  itkGetConstMacro(NumberOfProjections, int);
  itkSetMacro(NumberOfProjections, int);

  itkGetConstMacro(ScanDistance, double);
  itkSetMacro(ScanDistance, double);

  itkGetConstMacro(ScannerType, int);
  itkSetMacro(ScannerType, int);

  itkGetConstMacro(SampleTime, double);
  itkSetMacro(SampleTime, double);

  itkGetConstMacro(MeasurementIndex, int);
  itkSetMacro(MeasurementIndex, int);

  itkGetConstMacro(Site, int);
  itkSetMacro(Site, int);

  itkGetConstMacro(ReferenceLine, int);
  itkSetMacro(ReferenceLine, int);

  itkGetConstMacro(ReconstructionAlg, int);
  itkSetMacro(ReconstructionAlg, int);

  /** Get a string that states patient name.
   * Max size: 40 characters. */
  const char *
  GetPatientName() const
  {
    return this->m_PatientName;
  }
  void
  SetPatientName(const char * version)
  {
    strcpy(this->m_PatientName, version);
    this->Modified();
  }

  const char *
  GetCreationDate() const
  {
    return this->m_CreationDate;
  }
  void
  SetCreationDate(const char * version)
  {
    strcpy(this->m_CreationDate, version);
    this->Modified();
  }

  const char *
  GetModificationDate() const
  {
    return this->m_ModificationDate;
  }
  void
  SetModificationDate(const char * version)
  {
    strcpy(this->m_ModificationDate, version);
    this->Modified();
  }

  itkGetConstMacro(Energy, double);
  itkSetMacro(Energy, double);

  itkGetConstMacro(Intensity, double);
  itkSetMacro(Intensity, double);

protected:
  ScancoImageIO();
  ~ScancoImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Check the file header to see what type of file it is.
   *
   *  Return values are: 0 if unrecognized, 1 if ISQ/RAD,
   *  2 if AIM 020, 3 if AIM 030.
   */
  int
  CheckVersion(const char header[16]);

  /** Convert char data to 32-bit int (little-endian). */
  static int
  DecodeInt(const void * data);
  /** Convert 32-bit int (little-endian) to char data. */
  static void
  EncodeInt(int data, void * target);

  /** Convert char data to float (single precision). */
  static float
  DecodeFloat(const void * data);

  /** Convert char data to float (double precision). */
  static double
  DecodeDouble(const void * data);

  //! Convert a VMS timestamp to a calendar date.
  void
  DecodeDate(const void * data,
             int &        year,
             int &        month,
             int &        day,
             int &        hour,
             int &        minute,
             int &        second,
             int &        millis);
  //! Convert the current calendar date to a VMS timestamp and store in target
  void
  EncodeDate(void * target);

  //! Strip a string by removing trailing whitespace.
  /*!
   *  The dest must have a size of at least l+1.
   */
  static void
  StripString(char * dest, const char * source, size_t length);
  static void
  PadString(char * dest, const char * source, size_t length);

  void
  InitializeHeader();

  int
  ReadISQHeader(std::ifstream * file, unsigned long bytesRead);

  int
  ReadAIMHeader(std::ifstream * file, unsigned long bytesRead);

  void
  WriteISQHeader(std::ofstream * file);

  // Header information
  char   m_Version[18];
  char   m_PatientName[42];
  int    m_PatientIndex;
  int    m_ScannerID;
  char   m_CreationDate[32];
  char   m_ModificationDate[32];
  int    ScanDimensionsPixels[3];
  double ScanDimensionsPhysical[3];
  double m_SliceThickness;
  double m_SliceIncrement;
  double m_StartPosition;
  double m_EndPosition;
  double m_ZPosition;
  double m_DataRange[2];
  double m_MuScaling;
  int    m_NumberOfSamples;
  int    m_NumberOfProjections;
  double m_ScanDistance;
  double m_SampleTime;
  int    m_ScannerType;
  int    m_MeasurementIndex;
  int    m_Site;
  int    m_ReconstructionAlg;
  double m_ReferenceLine;
  double m_Energy;
  double m_Intensity;
  int    m_RescaleType;
  char   m_RescaleUnits[18];
  char   m_CalibrationData[66];
  double m_RescaleSlope;
  double m_RescaleIntercept;
  double m_MuWater;
  char * m_RawHeader;

  // The compression mode, if any.
  int m_Compression;

  SizeValueType m_HeaderSize{ 0 };
};
} // end namespace itk

#endif // itkScancoImageIO_h
