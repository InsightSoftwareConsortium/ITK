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

#ifndef itkNiftiImageIO_h
#define itkNiftiImageIO_h
#include "ITKIONIFTIExport.h"


#include <fstream>
#include <memory>
#include "itkImageIOBase.h"

namespace itk
{

/**\class NiftiImageIOEnums
 * \brief
 * \ingroup ITKIONIFTI
 */
class NiftiImageIOEnums
{
public:
  /** \class Analyze75Flavor
   * \ingroup ITKIONIFTI
   * Enum used to define the way to treat legacy Analyze75 files.
   */
  enum class Analyze75Flavor : uint8_t
  {
    /** Behavior introduced in ITK4.0 by NIFTI reader interpreting Analyze files */
    AnalyzeITK4 = 4,
    /** Will ignore orientation code and negative pixel dimensions */
    AnalyzeFSL = 3,
    /** Will ignore orientation code and respect negative pixel dimensions */
    AnalyzeSPM = 2,
    /** Same as AnalyzeITK4 but will show warning about deprecated file format (Default)*/
    AnalyzeITK4Warning = 1,
    /** Reject Analyze files as potentially wrong  */
    AnalyzeReject = 0
  };

  /** \class NiftiFileEnum
   * \ingroup ITKIONIFTI
   * Enum used to define the possible file formats readable by this ImageIO implementation.
   */
  enum class NiftiFileEnum : int8_t
  {
    /** 2-file Nifti (consisting of .hdr and .img file). */
    TwoFileNifti = 2,
    /** 1-file Nifti (consisting of .nii file). */
    OneFileNifti = 1,
    /** Legacy Analyze 7.5 format (consisting of .hdr and .img file). */
    Analyze75 = 0,
    /** Some other file format, or file system error. */
    OtherOrError = -1,
  };
};

/** Backwards compatibility for enum values */
#if !defined(ITK_LEGACY_REMOVE)
using Analyze75Flavor = NiftiImageIOEnums::Analyze75Flavor;
#endif

/** Define how to print enumerations */
extern ITKIONIFTI_EXPORT std::ostream &
                         operator<<(std::ostream & out, const NiftiImageIOEnums::Analyze75Flavor value);
extern ITKIONIFTI_EXPORT std::ostream &
                         operator<<(std::ostream & out, const NiftiImageIOEnums::NiftiFileEnum value);

/**
 *\class NiftiImageIO
 *
 * \author Hans J. Johnson, The University of Iowa 2002
 * \brief Class that defines how to read Nifti file format.
 * Nifti IMAGE FILE FORMAT - As much information as I can determine from sourceforge.net/projects/Niftilib
 *
 * The specification for this file format is taken from the
 * web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 *
 * \ingroup IOFilters
 * \ingroup ITKIONIFTI
 */
class ITKIONIFTI_EXPORT NiftiImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NiftiImageIO);

  /** Standard class type aliases. */
  using Self = NiftiImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NiftiImageIO, Superclass);

  //-------- This part of the interfaces deals with reading data. -----

#if !defined(ITK_LEGACY_REMOVE)
  /** Backwards compatibility for enum values */
  using FileType = NiftiImageIOEnums::NiftiFileEnum;
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr FileType TwoFileNifti = NiftiImageIOEnums::NiftiFileEnum::TwoFileNifti;
  static constexpr FileType OneFileNifti = NiftiImageIOEnums::NiftiFileEnum::OneFileNifti;
  static constexpr FileType Analyze75 = NiftiImageIOEnums::NiftiFileEnum::Analyze75;
  static constexpr FileType OtherOrError = NiftiImageIOEnums::NiftiFileEnum::OtherOrError;
#endif

  /** Reads the file to determine if it can be read with this ImageIO implementation,
   * and to determine what kind of file it is (Analyze vs NIfTI). Note that the value
   * of LegacyAnalyze75Mode is ignored by this method.
   * \param FileNameToRead The name of the file to test for reading.
   * \return Returns one of the IOFileEnum enumerations.
   */
  NiftiImageIOEnums::NiftiFileEnum
  DetermineFileType(const char * FileNameToRead);

  /** Reads the file to determine if it can be read with this ImageIO implementation.
   * Analyze 7.5 format files will only result in true if LegacyAnalyze75Mode is true.
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  bool
  CanReadFile(const char * FileNameToRead) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  //-------- This part of the interfaces deals with writing data. -----

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  bool
  CanWriteFile(const char * FileNameToWrite) override;

  /** Set the spacing and dimension information for the set filename.
   *
   * For Nifti this does not write a file, it only fills in the
   * appropriate header information.
   */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * buffer) override;

  /** Calculate the region of the image that can be efficiently read
   *  in response to a given requested region. */
  ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const override;

  /** Set the slope and intercept for voxel value rescaling. */
  itkSetMacro(RescaleSlope, double);
  itkSetMacro(RescaleIntercept, double);

  /** A mode to allow the Nifti filter to read and write to the LegacyAnalyze75 format as interpreted by
   * the nifti library maintainers.  This format does not properly respect the file orientation fields.
   * By default this is set by configuration option ITK_NIFTI_IO_ANALYZE_FLAVOR
   */
  itkSetMacro(LegacyAnalyze75Mode, NiftiImageIOEnums::Analyze75Flavor);
  itkGetConstMacro(LegacyAnalyze75Mode, NiftiImageIOEnums::Analyze75Flavor);

protected:
  NiftiImageIO();
  ~NiftiImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual bool
  GetUseLegacyModeForTwoFileWriting() const
  {
    return false;
  }

private:
  // Try to use the Q and S form codes from MetaDataDictionary if they are specified
  // there, otherwise default to the backwards compatible values from earlier
  // versions of ITK. The qform guess would probably been better to have
  // been guessed as NIFTI_XFORM_SCANNER_ANAT
  unsigned int
  getSFormCodeFromDictionary() const;
  unsigned int
  getQFormCodeFromDictionary() const;

  bool
  MustRescale() const;

  void
  DefineHeaderObjectDataType();

  void
  SetNIfTIOrientationFromImageIO(unsigned short int origdims, unsigned short int dims);

  void
  SetImageIOOrientationFromNIfTI(unsigned short int dims);

  void
  SetImageIOMetadataFromNIfTI();

  // This proxy class provides a nifti_image pointer interface to the internal implementation
  // of itk::NiftiImageIO, while hiding the niftilib interface from the external ITK interface.
  class NiftiImageProxy;

  // Note that it is essential that m_NiftiImageHolder is defined before m_NiftiImage, to ensure that
  // m_NiftiImage can directly get a proxy from m_NiftiImageHolder during NiftiImageIO construction.
  const std::unique_ptr<NiftiImageProxy> m_NiftiImageHolder;

  NiftiImageProxy & m_NiftiImage;

  double m_RescaleSlope{ 1.0 };
  double m_RescaleIntercept{ 0.0 };

  IOComponentEnum m_OnDiskComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };

  NiftiImageIOEnums::Analyze75Flavor m_LegacyAnalyze75Mode;
};


} // end namespace itk

#endif // itkNiftiImageIO_h
