/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
 * units as compared to the original files:
 *
 * distances are given in millimeters (instead of micrometers)
 * times are given in milliseconds (instead of microseconds)
 * voltage and current given in kV and mA (instead of volts and microamps).
 *
 * If the scanner was calibrated, then
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
#include "itkScancoDataManipulation.h"
#include "itkScancoHeaderIO.h"

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
  enum ScancoFileExtensions
  {
    UNRECOGNIZED = -1,
    AIM,
    ISQ,
    RAD,
    RSQ
  };

  ITK_DISALLOW_COPY_AND_MOVE(ScancoImageIO);

  /** Standard class typedefs. */
  using Self = ScancoImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(ScancoImageIO);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D.
   * \param dimension The dimension to check for support.
   * \return True if the dimension is supported by ScancoIO, false otherwise.
   */
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
    return this->m_HeaderData.m_Version;
  }
  void
  SetVersion(const char * version)
  {
    strncpy(this->m_HeaderData.m_Version, version, 18);
    this->Modified();
  }

  const char *
  GetCalibrationData() const
  {
    return this->m_HeaderData.m_CalibrationData;
  }
  void
  SetCalibrationData(const char * calibrationData)
  {
    strncpy(this->m_HeaderData.m_CalibrationData, calibrationData, 66);
    this->Modified();
  }

  const char *
  GetRescaleUnits() const
  {
    return this->m_HeaderData.m_RescaleUnits;
  }
  void
  SetRescaleUnits(const char * rescaleUnits)
  {
    strncpy(this->m_HeaderData.m_RescaleUnits, rescaleUnits, 18);
    this->Modified();
  }

  ScancoGetConstMacro(PatientIndex, int);
  ScancoSetMacro(PatientIndex, int);

  ScancoGetConstMacro(ScannerID, int);
  ScancoSetMacro(ScannerID, int);

  ScancoGetConstMacro(SliceThickness, double);
  ScancoSetMacro(SliceThickness, double);

  ScancoGetConstMacro(SliceIncrement, double);
  ScancoSetMacro(SliceIncrement, double);

  ScancoGetConstMacro(StartPosition, double);
  ScancoSetMacro(StartPosition, double);

  /** Set / Get the minimum and maximum values */
  const double *
  GetDataRange() const
  {
    return this->m_HeaderData.m_DataRange.data();
  }
  void
  SetDataRange(const double * dataRange)
  {
    this->m_HeaderData.m_DataRange[0] = dataRange[0];
    this->m_HeaderData.m_DataRange[1] = dataRange[1];
  }
  void
  SetDataRange(const std::vector<double> & dataRange)
  {
    if (dataRange.size() >= 2)
    {
      this->m_HeaderData.m_DataRange[0] = dataRange[0];
      this->m_HeaderData.m_DataRange[1] = dataRange[1];
    }
  }

  ScancoGetConstMacro(MuScaling, double);
  ScancoSetMacro(MuScaling, double);

  ScancoGetConstMacro(MuWater, double);
  ScancoSetMacro(MuWater, double);

  ScancoGetConstMacro(RescaleType, int);
  ScancoSetMacro(RescaleType, int);

  ScancoGetConstMacro(RescaleSlope, double);
  ScancoSetMacro(RescaleSlope, double);

  ScancoGetConstMacro(RescaleIntercept, double);
  ScancoSetMacro(RescaleIntercept, double);

  ScancoGetConstMacro(NumberOfSamples, int);
  ScancoSetMacro(NumberOfSamples, int);

  ScancoGetConstMacro(NumberOfProjections, int);
  ScancoSetMacro(NumberOfProjections, int);

  ScancoGetConstMacro(ScanDistance, double);
  ScancoSetMacro(ScanDistance, double);

  ScancoGetConstMacro(ScannerType, int);
  ScancoSetMacro(ScannerType, int);

  ScancoGetConstMacro(SampleTime, double);
  ScancoSetMacro(SampleTime, double);

  ScancoGetConstMacro(MeasurementIndex, int);
  ScancoSetMacro(MeasurementIndex, int);

  ScancoGetConstMacro(Site, int);
  ScancoSetMacro(Site, int);

  ScancoGetConstMacro(ReferenceLine, int);
  ScancoSetMacro(ReferenceLine, int);

  ScancoGetConstMacro(ReconstructionAlg, int);
  ScancoSetMacro(ReconstructionAlg, int);

  /** Get a string that states patient name.
   * Max size: 40 characters. */
  const char *
  GetPatientName() const
  {
    return this->m_HeaderData.m_PatientName;
  }
  void
  SetPatientName(const char * patientName)
  {
    strncpy(this->m_HeaderData.m_PatientName, patientName, 42);
    this->Modified();
  }

  const char *
  GetCreationDate() const
  {
    return this->m_HeaderData.m_CreationDate;
  }
  void
  SetCreationDate(const char * creationDate)
  {
    strncpy(this->m_HeaderData.m_CreationDate, creationDate, 32);
    this->Modified();
  }

  const char *
  GetModificationDate() const
  {
    return this->m_HeaderData.m_ModificationDate;
  }
  void
  SetModificationDate(const char * modificationDate)
  {
    strncpy(this->m_HeaderData.m_ModificationDate, modificationDate, 32);
    this->Modified();
  }

  ScancoGetConstMacro(Energy, double);
  ScancoSetMacro(Energy, double);

  ScancoGetConstMacro(Intensity, double);
  ScancoSetMacro(Intensity, double);

protected:
  ScancoImageIO();
  ~ScancoImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Rescale the image data to Hounsfield Units */
  template <typename TBufferType>
  void
  RescaleToHU(TBufferType * buffer, size_t size);

  /** Rescale the image data to Scanco Units
   * This is the inverse of RescaleToHU.
   * \param buffer Pointer to the buffer containing the image data.
   * \param size Size of the buffer in number of elements.
   */
  template <typename TBufferType>
  void
  RescaleToScanco(TBufferType * buffer, size_t size);

  void
  InitializeHeader();

  void
  PopulateMetaDataDictionary();

  void
  SetHeaderFromMetaDataDictionary();

  void
  ParseAIMComponentType(int dataType);

  /** Set the IO object based on the image type to read/write
   * This method initializes the m_HeaderIO member variable
   * This method sets the m_FileExtension member variable
   */
  void
  SetHeaderIO();

  /** Set the numeric data type field from the ITK Component Enum */
  void
  SetDataTypeFromComponentEnum();

  itkScancoHeaderData m_HeaderData;

  ScancoHeaderIO * m_HeaderIO{ nullptr };

  ScancoFileExtensions m_FileExtension = ScancoFileExtensions::UNRECOGNIZED;

  // The compression mode, if any.
  int m_Compression;

  bool m_HeaderInitialized = false;

  SizeValueType m_HeaderSize{ 0 };
};
} // end namespace itk

#endif // itkScancoImageIO_h
