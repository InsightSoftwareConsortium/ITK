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
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkGDCMImageIO_h
#define itkGDCMImageIO_h

#include "itkCommonEnums.h"
#include "itkImageIOBase.h"
#include "ITKIOGDCMExport.h"
#include <fstream>
#include <string>


#if !defined(ITK_LEGACY_REMOVE)
#  define ITKIO_DEPRECATED_GDCM1_API
#endif


namespace itk
{
/**\class GDCMImageIOEnums
 * \brief
 * \ingroup ITKIOGDCM
 */
class GDCMImageIOEnums
{
public:
  /**
   *\class Compression
   * \ingroup ITKIOGDCM
   * Set/Get a compression type to use. */
  enum class Compression : uint8_t
  {
    JPEG = 0,
    JPEG2000,
    JPEGLS,
    RLE
  };
};

// Define how to print enumeration
extern ITKIOGDCM_EXPORT std::ostream &
                        operator<<(std::ostream & out, const GDCMImageIOEnums::Compression value);
/**
 *\class GDCMImageIO
 *
 *  \brief ImageIO class for reading and writing DICOM V3.0 and ACR/NEMA 1&2 uncompressed images.
 *  This class is only an adaptor to the GDCM library.
 *
 * GDCM can be found at:
 *   http://sourceforge.net/projects/gdcm
 *
 * To learn more about the revision shipped with ITK, call
 *
 *    git log -- Modules/ThirdParty/GDCM/src/
 *
 * from an ITK Git checkout.
 *
 * The compressors supported include "JPEG2000" (default), and
 * "JPEG". The compression level parameter is not supported.
 *
 *  \warning There are several restrictions to this current writer:
 *           -  Even though during the writing process you pass in a DICOM file as input
 *              The output file may not contains ALL DICOM field from the input file.
 *              In particular:
 *                             - The SeQuence DICOM field (SQ).
 *                             - Fields from Private Dictionary.
 *           -  Some very long (>0xfff) binary fields are not loaded (typically 0029|0010),
 *              you need to explicitly set the maximum length of elements to load to be bigger
 *              (see Get/SetMaxSizeLoadEntry).
 *           - In DICOM some fields are stored directly using their binary representation. When loaded into
 *             the MetaDataDictionary some fields are converted to ASCII (only VR: OB/OW/OF and UN are encoded as
 *             mime64).
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIOGDCM
 *
 * \sphinx
 * \sphinxexample{IO/GDCM/ResampleDICOMSeries,Resample DICOM Series}
 * \sphinxexample{IO/GDCM/ReadDICOMSeriesAndWrite3DImage,Read DICOM Series and Write 3D Image}
 * \endsphinx
 */
class InternalHeader;
class ITKIOGDCM_EXPORT GDCMImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GDCMImageIO);

  /** Standard class type aliases. */
  using Self = GDCMImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMImageIO, Superclass);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Read the spacing and dimension information for the current filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * pointer) override;

  /** Set/Get the original component type of the image. This differs from
   * ComponentType which may change as a function of rescale slope and
   * intercept. */
  itkGetEnumMacro(InternalComponentType, ::itk::CommonEnums::IOComponent);
  itkSetEnumMacro(InternalComponentType, ::itk::CommonEnums::IOComponent);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. GDCM triggers on ".dcm" and ".dicom". */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

  /** Macro to access Rescale Slope and Rescale Intercept. */
  itkGetConstMacro(RescaleSlope, double);
  itkGetConstMacro(RescaleIntercept, double);

  /** Macro to access the DICOM UID prefix. By default this is the ITK
   * root id. This default can be overriden if the exam is for example
   * part of an existing study. */
  itkGetStringMacro(UIDPrefix);
  itkSetStringMacro(UIDPrefix);

  /** Access the generated DICOM UIDs. */
  itkGetStringMacro(StudyInstanceUID);
  itkGetStringMacro(SeriesInstanceUID);
  itkGetStringMacro(FrameOfReferenceInstanceUID);

  /** Preserve the original DICOM UIDs of the input files. */
  itkSetMacro(KeepOriginalUID, bool);
  itkGetConstMacro(KeepOriginalUID, bool);
  itkBooleanMacro(KeepOriginalUID);

  /** Parse and load any private tags in the DICOM file. Loading DICOM
   * files is faster when private tags are not needed. Default is false. */
  itkSetMacro(LoadPrivateTags, bool);
  itkGetConstMacro(LoadPrivateTags, bool);
  itkBooleanMacro(LoadPrivateTags);

  /** Convert Y'CbCr (YBR_FULL, YBR_FULL_422) to RGB. Default is true,
   * not required for YBR_RCT and YBR_ICT. */
  itkSetMacro(ReadYBRtoRGB, bool);
  itkGetConstMacro(ReadYBRtoRGB, bool);
  itkBooleanMacro(ReadYBRtoRGB);

#if defined(ITKIO_DEPRECATED_GDCM1_API)
  /** Convenience methods to query patient information and scanner
   * information. These methods are here for compatibility with the
   * DICOMImageIO2 class and as such should not be used in any new code.
   * They rely on properly preallocated buffer, which is not a good practice.
   * Instead user is encouraged to use directly the GetValueFromTag function. */
  void
  GetPatientName(char * name, size_t len = 512);

  void
  GetPatientID(char * name, size_t len = 512);

  void
  GetPatientSex(char * name, size_t len = 512);

  void
  GetPatientAge(char * name, size_t len = 512);

  void
  GetStudyID(char * name, size_t len = 512);

  void
  GetPatientDOB(char * name, size_t len = 512);

  void
  GetStudyDescription(char * name, size_t len = 512);

  void
  GetBodyPart(char * name, size_t len = 512);

  void
  GetNumberOfSeriesInStudy(char * name, size_t len = 512);

  void
  GetNumberOfStudyRelatedSeries(char * name, size_t len = 512);

  void
  GetStudyDate(char * name, size_t len = 512);

  void
  GetModality(char * name, size_t len = 512);

  void
  GetManufacturer(char * name, size_t len = 512);

  void
  GetInstitution(char * name, size_t len = 512);

  void
  GetModel(char * name, size_t len = 512);

  void
  GetScanOptions(char * name, size_t len = 512);
#endif

  /** More general method to retrieve an arbitrary DICOM value based
   * on a DICOM Tag (eg "0123|45ef"). */
  bool
  GetValueFromTag(const std::string & tag, std::string & value);

  /** Method for consulting the DICOM dictionary and recovering the text
   * description of a field using its numeric tag represented as a string. If
   * the tagkey is not found in the dictionary then this static method return
   * false and the value "Unknown " in the labelId. If the tagkey is found then
   * this static method returns true and the actual string descriptor of the
   * tagkey is returned in the variable labelId. */
  static bool
  GetLabelFromTag(const std::string & tag, std::string & labelId);

#if defined(ITKIO_DEPRECATED_GDCM1_API)
  /** A DICOM file can contains multiple binary stream that can be very long.
   * For example an Overlay on the image. Most of the time user do not want to load
   * this binary structure in memory since it can consume lot of memory. Therefore
   * any field that is bigger than the default value 0xfff is discarded and just seek'd.
   * This method allow advanced user to force the reading of such field.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x */
  virtual void
  SetMaxSizeLoadEntry(const long)
  {}

  /** Parse any sequences in the DICOM file. Defaults to the value of
   * LoadSequencesDefault. Loading DICOM files is faster when
   * sequences are not needed.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x */
  virtual void
  SetLoadSequences(const bool)
  {}
  virtual bool
  GetLoadSequences() const
  {
    return true;
  }
  virtual void
  LoadSequencesOn()
  {}
  virtual void
  LoadSequencesOff()
  {}

  /** Global method to define the default value for
   * LoadSequences. When instances of GDCMImageIO are created, the
   * ivar LoadSequences is initialized to the value of
   * LoadSequencesDefault. This method is useful when relying on the
   * IO factory mechanism to load images rather than specifying a
   * particular ImageIO object on the readers. Default is false.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x */
  static void
  SetLoadSequencesDefault(bool)
  {}
  static void
  LoadSequencesDefaultOn()
  {}
  static void
  LoadSequencesDefaultOff()
  {}
  static bool
  GetLoadSequencesDefault()
  {
    return true;
  }

  /** Global method to define the default value for
   * LoadPrivateTags. When instances of GDCMImageIO are created, the
   * ivar LoadPrivateTags is initialized to the value of
   * LoadPrivateTagsDefault. This method is useful when relying on the
   * IO factory mechanism to load images rather than specifying a
   * particular ImageIO object on the readers. Default is false.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x */
  static void
  SetLoadPrivateTagsDefault(bool)
  {}
  static void
  LoadPrivateTagsDefaultOn()
  {}
  static void
  LoadPrivateTagsDefaultOff()
  {}
  static bool
  GetLoadPrivateTagsDefault()
  {
    return true;
  }
#endif


  using CompressionEnum = GDCMImageIOEnums::Compression;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr CompressionEnum JPEG = CompressionEnum::JPEG;
  static constexpr CompressionEnum JPEG2000 = CompressionEnum::JPEG2000;
  static constexpr CompressionEnum JPEGLS = CompressionEnum::JPEGLS;
  static constexpr CompressionEnum RLE = CompressionEnum::RLE;
#endif

  itkSetEnumMacro(CompressionType, CompressionEnum);
  itkGetEnumMacro(CompressionType, CompressionEnum);

  void
  InternalSetCompressor(const std::string & _compressor) override;

protected:
  GDCMImageIO();
  ~GDCMImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InternalReadImageInformation();

  double m_RescaleSlope;

  double m_RescaleIntercept;

  std::string m_UIDPrefix;

  std::string m_StudyInstanceUID;

  std::string m_SeriesInstanceUID;

  std::string m_FrameOfReferenceInstanceUID;

  bool m_KeepOriginalUID;

  bool m_LoadPrivateTags;

  bool m_ReadYBRtoRGB;

private:
#if defined(ITKIO_DEPRECATED_GDCM1_API)
  std::string m_PatientName;

  std::string m_PatientID;

  std::string m_PatientDOB;

  std::string m_StudyID;

  std::string m_StudyDescription;

  std::string m_BodyPart;

  std::string m_NumberOfSeriesInStudy;

  std::string m_NumberOfStudyRelatedSeries;

  std::string m_PatientSex;

  std::string m_PatientAge;

  std::string m_StudyDate;

  std::string m_Modality;

  std::string m_Manufacturer;

  std::string m_Institution;

  std::string m_Model;

  std::string m_ScanOptions;
#endif

  unsigned int m_GlobalNumberOfDimensions;

  CompressionEnum m_CompressionType;

  bool m_SingleBit;

  IOComponentEnum m_InternalComponentType;

  InternalHeader * m_DICOMHeader;
};

} // end namespace itk

#endif // itkGDCMImageIO_h
