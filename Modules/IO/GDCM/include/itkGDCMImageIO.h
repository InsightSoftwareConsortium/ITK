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


namespace itk
{
/** \class GDCMImageIOEnums
 * \ingroup ITKIOGDCM
 */
class GDCMImageIOEnums
{
public:
  /**
   * \class Compression
   * \ingroup ITKIOGDCM
   * Set/Get a compression type to use. */
  enum class Compression : uint8_t
  {
    JPEG = 0,
    JPEG2000,
    JPEGLS,
    RLE,
    HTJ2K
  };
};

// Define how to print enumeration
extern ITKIOGDCM_EXPORT std::ostream &
                        operator<<(std::ostream & out, const GDCMImageIOEnums::Compression value);
/**
 * \class GDCMImageIO
 *
 *  \brief ImageIO class for reading and writing DICOM V3.0 and ACR/NEMA 1&2 uncompressed images.
 *  This class is only an adaptor to the GDCM library.
 *
 * GDCM can be found at:
 *   https://sourceforge.net/projects/gdcm
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
 * DICOM tags are represented as strings in the metadata
 * dictionary. The string format is "XXXX,XXXX", DICOM group number followed
 * by element number, both are hexadecimals. The separator character
 * is either a pipe "|" or a comma ",".
 *
 * \warning As the metadata dictionary uses the DICOM tag strings as
 * keys it is possible to have multiple entries representing the same
 * DICOM tag with different values. The last one encountered will be
 * used when writing the image to file. For example, the patient name tag
 * "0010,0010" and "0010|0010" may both be in the dictionary with different values
 * due to the different separator character. Similarly,
 * the series description tag may appear multiple times as "0008,103e",
 * "0008,103E", "0008|103e", or "0008|103E". The strings differ in
 * the separator character and lower or upper case letters in the
 * hexadecimal numbers. Note that when read from file, letters in
 * the hexadecimal numbers are always set to lower case and the
 * separator character is a pipe. To ensure consistency,
 * it is best to always use lower case letters and the pipe separator
 * when explicitly adding tags to the metadata dictionary.
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

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(GDCMImageIO);

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
  /** @ITKStartGrouping */
  itkGetEnumMacro(InternalComponentType, itk::CommonEnums::IOComponent);
  itkSetEnumMacro(InternalComponentType, itk::CommonEnums::IOComponent);
  /** @ITKEndGrouping */
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
  /** @ITKStartGrouping */
  itkGetConstMacro(RescaleSlope, double);
  itkGetConstMacro(RescaleIntercept, double);
  /** @ITKEndGrouping */
  /** Macro to access the DICOM UID prefix. By default this is the ITK
   * root id. This default can be overridden if the exam is for example
   * part of an existing study. */
  /** @ITKStartGrouping */
  itkGetStringMacro(UIDPrefix);
  itkSetStringMacro(UIDPrefix);
  /** @ITKEndGrouping */
  /** Access the generated DICOM UIDs. */
  /** @ITKStartGrouping */
  itkGetStringMacro(StudyInstanceUID);
  itkGetStringMacro(SeriesInstanceUID);
  itkGetStringMacro(FrameOfReferenceInstanceUID);
  /** @ITKEndGrouping */
  /** Preserve the original DICOM UIDs of the input files. */
  /** @ITKStartGrouping */
  itkSetMacro(KeepOriginalUID, bool);
  itkGetConstMacro(KeepOriginalUID, bool);
  itkBooleanMacro(KeepOriginalUID);
  /** @ITKEndGrouping */
  /** Parse and load any private tags in the DICOM file. Loading DICOM
   * files is faster when private tags are not needed. Default is false. */
  /** @ITKStartGrouping */
  itkSetMacro(LoadPrivateTags, bool);
  itkGetConstMacro(LoadPrivateTags, bool);
  itkBooleanMacro(LoadPrivateTags);
  /** @ITKEndGrouping */
  /** Convert Y'CbCr (YBR_FULL, YBR_FULL_422) to RGB. Default is true,
   * not required for YBR_RCT and YBR_ICT. */
  /** @ITKStartGrouping */
  itkSetMacro(ReadYBRtoRGB, bool);
  itkGetConstMacro(ReadYBRtoRGB, bool);
  itkBooleanMacro(ReadYBRtoRGB);
  /** @ITKEndGrouping */
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


  using CompressionEnum = GDCMImageIOEnums::Compression;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr CompressionEnum JPEG = CompressionEnum::JPEG;
  static constexpr CompressionEnum JPEG2000 = CompressionEnum::JPEG2000;
  static constexpr CompressionEnum JPEGLS = CompressionEnum::JPEGLS;
  static constexpr CompressionEnum RLE = CompressionEnum::RLE;
#endif

  /** Set the compression type to use for writing.
   *
   * Currently only JPEG2000 and JPEG are supported.
   * These map to the following DICOM standards:
   *   JPEG2000: JPEG 2000 Image Compression (Lossless Only)
   *   JPEG:     JPEG Lossless, Non-Hierarchical, First-Order Prediction
   *
   *
   * @param _arg
   */
  itkSetEnumMacro(CompressionType, CompressionEnum);
  /** Get the compression type to use for writing.
   *
   * Currently only JPEG2000 and JPEG are supported.
   * These map to the following DICOM standards:
   *   JPEG2000: JPEG 2000 Image Compression (Lossless Only)
   *   JPEG:     JPEG Lossless, Non-Hierarchical, First-Order Prediction
   *
   */
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

  double m_RescaleSlope{};

  double m_RescaleIntercept{};

  std::string m_UIDPrefix{};

  std::string m_StudyInstanceUID{};

  std::string m_SeriesInstanceUID{};

  std::string m_FrameOfReferenceInstanceUID{};

  bool m_KeepOriginalUID{};

  bool m_LoadPrivateTags{};

  bool m_ReadYBRtoRGB{};

private:
  unsigned int m_GlobalNumberOfDimensions{};

  CompressionEnum m_CompressionType{};

  bool m_SingleBit{};

  IOComponentEnum m_InternalComponentType{};

  InternalHeader * m_DICOMHeader{};
};

} // end namespace itk

#endif // itkGDCMImageIO_h
