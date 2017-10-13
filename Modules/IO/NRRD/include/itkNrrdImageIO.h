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
#ifndef itkNrrdImageIO_h
#define itkNrrdImageIO_h
#include "ITKIONRRDExport.h"


#include "itkImageIOBase.h"
#include <fstream>

namespace itk
{
/** \class NrrdImageIO
 *
 * \brief Read and write the "Nearly Raw Raster Data" (Nrrd) image format.
 * The Nrrd format was developed as part of the Teem package
 * (teem.sourceforge.net).
 *
 *  \ingroup IOFilters
 * \ingroup ITKIONRRD
 */
class ITKIONRRD_EXPORT NrrdImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef NrrdImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NrrdImageIO, ImageIOBase);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool SupportsDimension(unsigned long) ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

protected:
  NrrdImageIO();
  ~NrrdImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Utility functions for converting between enumerated data type
      representations */
  int ITKToNrrdComponentType(const ImageIOBase::IOComponentType) const;

  ImageIOBase::IOComponentType NrrdToITKComponentType(const int) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NrrdImageIO);
};
} // end namespace itk

#endif // itkNrrdImageIO_h
