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
/**
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 */

#ifndef __itkNiftiImageIO_h
#define __itkNiftiImageIO_h

#include <fstream>
#include <nifti1_io.h>

#include "itkImageIOBase.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class NiftiImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read Nifti file format.
 * Nifti IMAGE FILE FORMAT - As much information as I can determine from sourceforge.net/projects/Niftilib
 *
 * \ingroup IOFilters
 * \ingroup ITKIONIFTI
 */
class ITK_EXPORT NiftiImageIO:public ImageIOBase
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

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  virtual bool CanWriteFile(const char *FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

  /** Calculate the region of the image that can be efficiently read
   *  in response to a given requested region. */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const;

  /** A mode to allow the Nifti filter to read and write to the LegacyAnalyze75 format as interpreted by
    * the nifti library maintainers.  This format does not properly respect the file orientation fields.
    * The itkAnalyzeImageIO file reader/writer should be used to match the Analyze75 file definitions as
    * specified by the Mayo Clinic BIR laboratory.  By default this is set to false.
    */
  itkSetMacro(LegacyAnalyze75Mode, bool);
  itkGetConstMacro(LegacyAnalyze75Mode, bool);

protected:
  NiftiImageIO();
  ~NiftiImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

  virtual bool GetUseLegacyModeForTwoFileWriting(void) const { return false; }

private:
  bool  MustRescale();

  void  DefineHeaderObjectDataType();

  void  SetNIfTIOrientationFromImageIO(uint16_t origdims, uint16_t dims);

  void  SetImageIOOrientationFromNIfTI(uint16_t dims);

  void  SetImageIOMetadataFromNIfTI();

  nifti_image *m_NiftiImage;

  double m_RescaleSlope;
  double m_RescaleIntercept;

  IOComponentType m_OnDiskComponentType;

  bool m_LegacyAnalyze75Mode;

  NiftiImageIO(const Self &);   //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#endif // __itkNiftiImageIO_h
