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

#define ITKIO_DEPRECATED_GDCM1_API

#include "itkImageIOBase.h"
#include "ITKIOGDCMExport.h"
#include <fstream>
#include <string>

namespace itk
{
/** \class GDCMImageIO
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
 * From an ITK Git checkout.
 *
 * GDCM build, instead of the one included within ITK itself.
 *
 *  \warning There are several restrictions to this current writer:
 *           -  Even though during the writing process you pass in a DICOM file as input
 *              The output file may not contains ALL DICOM field from the input file.
 *              In particular:
 *                             - The SeQuence DICOM field (SQ).
 *                             - Fields from Private Dictionary.
 *           -  Some very long (>0xfff) binary fields are not loaded (typically 0029|0010),
 *              you need to explicitely set the maximum length of elements to load to be bigger
 *              (see Get/SetMaxSizeLoadEntry).
 *           - In DICOM some fields are stored directly using their binary representation. When loaded into
 *             the MetaDataDictionary some fields are converted to ASCII (only VR: OB/OW/OF and UN are encoded as
 *             mime64).
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIOGDCM
 *
 * \wiki
 * \wikiexample{DICOM/ResampleDICOM,Resample a DICOM series}
 * \endwiki
 */
class InternalHeader;
class ITKIOGDCM_EXPORT GDCMImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef GDCMImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMImageIO, Superclass);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimesion information for the current filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /** Set/Get the original component type of the image. This differs from
   * ComponentType which may change as a function of rescale slope and
   * intercept. */
  itkGetEnumMacro(InternalComponentType, IOComponentType);
  itkSetEnumMacro(InternalComponentType, IOComponentType);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. GDCM triggers on ".dcm" and ".dicom". */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Macro to access Rescale Slope and Rescale Intercept. Which are
   * needed to rescale properly image when needed. User then need to
   * Always check those value when access value from the DICOM header */
  itkGetConstMacro(RescaleSlope, double);
  itkGetConstMacro(RescaleIntercept, double);

  /** Macro to access the DICOM UID prefix. By default this is the ITK
   *  root id. This default can be overriden if the exam is for example
   *  part of an existing study.
   */
  itkGetStringMacro(UIDPrefix);
  itkSetStringMacro(UIDPrefix);

  /** Access the generated DICOM UID's. */
  itkGetStringMacro(StudyInstanceUID);
  itkGetStringMacro(SeriesInstanceUID);
  itkGetStringMacro(FrameOfReferenceInstanceUID);

  /** Preserve the original DICOM UID of the input files
   */
  itkSetMacro(KeepOriginalUID, bool);
  itkGetConstMacro(KeepOriginalUID, bool);
  itkBooleanMacro(KeepOriginalUID);

  /** Parse and load any private tags in the DICOM file. Loading DICOM
   * files is faster when private tags are not needed. Default is false.
   */
  itkSetMacro(LoadPrivateTags, bool);
  itkGetConstMacro(LoadPrivateTags, bool);
  itkBooleanMacro(LoadPrivateTags);

#if defined( ITKIO_DEPRECATED_GDCM1_API )
  /** Convenience methods to query patient information and scanner
   * information. These methods are here for compatibility with the
   * DICOMImageIO2 class and as such should not be used in any new code.
   * They rely on properly preallocated buffer, which is not a good practice.
   * Instead user are encourage to use directly the GetValueFromTag function
   */
  void GetPatientName(char *name);

  void GetPatientID(char *id);

  void GetPatientSex(char *sex);

  void GetPatientAge(char *age);

  void GetStudyID(char *id);

  void GetPatientDOB(char *dob);

  void GetStudyDescription(char *desc);

  void GetBodyPart(char *part);

  void GetNumberOfSeriesInStudy(char *series);

  void GetNumberOfStudyRelatedSeries(char *series);

  void GetStudyDate(char *date);

  void GetModality(char *modality);

  void GetManufacturer(char *manu);

  void GetInstitution(char *ins);

  void GetModel(char *model);

  void GetScanOptions(char *options);
#endif

  /** More general method to retrieve an arbitrary DICOM value based
   * on a DICOM Tag (eg "0123|45ef").
   */
  bool GetValueFromTag(const std::string & tag, std::string & value);

  /** Method for consulting the DICOM dictionary and recovering the text
   * description of a field using its numeric tag represented as a string.  If
   * the tagkey is not found in the dictionary then this static method return
   * false and the value "Unknown " in the labelId. If the tagkey is found then
   * this static method returns true and the actual string descriptor of the
   * tagkey is returned in the variable labelId. */
  static bool GetLabelFromTag(const std::string & tag,
                              std::string & labelId);

#if defined( ITKIO_DEPRECATED_GDCM1_API )
  /** A DICOM file can contains multiple binary stream that can be very long
   * For example an Overlay on the image. Most of the time user do not want to load
   * this binary structure in memory since it can consume lot of memory. Therefore
   * any field that is bigger than the default value 0xfff is discarded and just seek'd
   * This method allow advanced user to force the reading of such field
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x
   */
  virtual void SetMaxSizeLoadEntry( const long ) {}

  /** Parse any sequences in the DICOM file. Defaults to the value of
   *  LoadSequencesDefault. Loading DICOM files is faster when
   *  sequences are not needed.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x
   */
  virtual void SetLoadSequences( const bool ) {}
  virtual bool GetLoadSequences () const { return true; }
  virtual void LoadSequencesOn () {}
  virtual void LoadSequencesOff () {}

  /** Global method to define the default value for
   * LoadSequences. When instances of GDCMImageIO are created, the
   * ivar LoadSequences is initialized to the value of
   * LoadSequencesDefault.  This method is useful when relying on the
   * IO factory mechanism to load images rather than specifying a
   * particular ImageIO object on the readers. Default is false.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x
   */
  static void SetLoadSequencesDefault(bool) {}
  static void LoadSequencesDefaultOn() {}
  static void LoadSequencesDefaultOff() {}
  static bool GetLoadSequencesDefault() { return true; }

  /** Global method to define the default value for
   * LoadPrivateTags. When instances of GDCMImageIO are created, the
   * ivar LoadPrivateTags is initialized to the value of
   * LoadPrivateTagsDefault.  This method is useful when relying on the
   * IO factory mechanism to load images rather than specifying a
   * particular ImageIO object on the readers. Default is false.
   * \warning this is a GDCM 1.x only option, no effect on GDCM 2.x
   */
  static void SetLoadPrivateTagsDefault(bool) {}
  static void LoadPrivateTagsDefaultOn() {}
  static void LoadPrivateTagsDefaultOff() {}
  static bool GetLoadPrivateTagsDefault() { return true; }
#endif

  /** Set/Get a compression type to use. */
  typedef enum { JPEG = 0, JPEG2000, JPEGLS, RLE } TCompressionType;
  itkSetEnumMacro(CompressionType, TCompressionType);
  itkGetEnumMacro(CompressionType, TCompressionType);

protected:
  GDCMImageIO();
  ~GDCMImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void InternalReadImageInformation();

  double m_RescaleSlope;
  double m_RescaleIntercept;

  std::string m_UIDPrefix;
  std::string m_StudyInstanceUID;
  std::string m_SeriesInstanceUID;
  std::string m_FrameOfReferenceInstanceUID;

  bool m_KeepOriginalUID;

  bool m_LoadPrivateTags;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GDCMImageIO);

#if defined( ITKIO_DEPRECATED_GDCM1_API )
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

  /** defines whether this image is a 2D out of a 2D image
   *  or a 2D out of a 3D image. */
  unsigned int     m_GlobalNumberOfDimensions;
  TCompressionType m_CompressionType;

  ImageIOBase::IOComponentType m_InternalComponentType;
  InternalHeader *             m_DICOMHeader;
};
} // end namespace itk

#endif // itkGDCMImageIO_h
