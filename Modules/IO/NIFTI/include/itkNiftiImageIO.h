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

#ifndef itkNiftiImageIO_h
#define itkNiftiImageIO_h
#include "ITKIONIFTIExport.h"


#include <fstream>
#include "itkAutoPointer.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class NiftiImageIO
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
class ITKIONIFTI_EXPORT NiftiImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef NiftiImageIO         Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NiftiImageIO, Superclass);

  //-------- This part of the interfaces deals with reading data. -----

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  //-------- This part of the interfaces deals with writing data. -----

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename.
   *
   * For Nifti this does not write a file, it only fills in the
   * appropriate header information.
   */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Calculate the region of the image that can be efficiently read
   *  in response to a given requested region. */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const ITK_OVERRIDE;

  /** A mode to allow the Nifti filter to read and write to the LegacyAnalyze75 format as interpreted by
    * the nifti library maintainers.  This format does not properly respect the file orientation fields.
    * The itkAnalyzeImageIO file reader/writer should be used to match the Analyze75 file definitions as
    * specified by the Mayo Clinic BIR laboratory.  By default this is set to false.
    */
  itkSetMacro(LegacyAnalyze75Mode, bool);
  itkGetConstMacro(LegacyAnalyze75Mode, bool);

protected:
  NiftiImageIO();
  ~NiftiImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual bool GetUseLegacyModeForTwoFileWriting(void) const { return false; }

private:
  //Try to use the Q and S form codes from MetaDataDictionary if they are specified
  //there, otherwise default to the backwards compatible values from earlier
  //versions of ITK. The qform guess would probably been better to have
  //been guessed as NIFTI_XFORM_SCANNER_ANAT
  unsigned int getSFormCodeFromDictionary() const;
  unsigned int getQFormCodeFromDictionary() const;

  bool  MustRescale();

  void  DefineHeaderObjectDataType();

  void  SetNIfTIOrientationFromImageIO(unsigned short int origdims, unsigned short int dims);

  void  SetImageIOOrientationFromNIfTI(unsigned short int dims);

  void  SetImageIOMetadataFromNIfTI();

  //This proxy class provides a nifti_image pointer interface to the internal implementation
  //of itk::NiftiImageIO, while hiding the niftilib interface from the external ITK interface.
  class NiftiImageProxy;

  //Note that it is essential that m_NiftiImageHolder is defined before m_NiftiImage, to ensure that
  //m_NiftiImage can directly get a proxy from m_NiftiImageHolder during NiftiImageIO construction.
  const AutoPointer<NiftiImageProxy> m_NiftiImageHolder;

  NiftiImageProxy& m_NiftiImage;

  double m_RescaleSlope;
  double m_RescaleIntercept;

  IOComponentType m_OnDiskComponentType;

  bool m_LegacyAnalyze75Mode;

  ITK_DISALLOW_COPY_AND_ASSIGN(NiftiImageIO);
};
} // end namespace itk

#endif // itkNiftiImageIO_h
