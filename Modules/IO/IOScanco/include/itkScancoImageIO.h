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

  Program: DICOM for VTK

  Copyright (c) 2015 David Gobbi
  All rights reserved.
  See Copyright.txt or http://dgobbi.github.io/bsd3.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
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
  /** Standard class typedefs. */
  typedef ScancoImageIO            Self;
  typedef ImageIOBase              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScancoImageIO, ImageIOBase);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool
  SupportsDimension(unsigned long dimension) ITK_OVERRIDE
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
  virtual bool
  CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void
  ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  Read(void * buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool
  CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void
  WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void
  Write(const void * buffer) ITK_OVERRIDE;

  /** Method for supporting streaming.  Given a requested region, calculate what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be smaller than the LargestPossibleRegion and
   * greater or equal to the RequestedRegion */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const ITK_OVERRIDE;

  virtual unsigned int
  GetActualNumberOfSplitsForWriting(unsigned int          numberOfRequestedSplits,
                                    const ImageIORegion & pasteRegion,
                                    const ImageIORegion & largestPossibleRegion) ITK_OVERRIDE;

  virtual ImageIORegion
  GetSplitRegionForWriting(unsigned int          ithPiece,
                           unsigned int          numberOfActualSplits,
                           const ImageIORegion & pasteRegion,
                           const ImageIORegion & largestPossibleRegion) ITK_OVERRIDE;

  virtual bool
  CanStreamRead() ITK_OVERRIDE
  {
    return true;
  }

  virtual bool
  CanStreamWrite() ITK_OVERRIDE
  {
    return true;
  }

protected:
  ScancoImageIO();
  ~ScancoImageIO();

  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScancoImageIO);

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

  /** Convert char data to float (single precision). */
  static float
  DecodeFloat(const void * data);

  /** Convert char data to float (double precision). */
  static double
  DecodeDouble(const void * data);
};
} // end namespace itk

#endif // itkScancoImageIO_h
