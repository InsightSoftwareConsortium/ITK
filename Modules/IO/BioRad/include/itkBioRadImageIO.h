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
#ifndef itkBioRadImageIO_h
#define itkBioRadImageIO_h
#include "ITKIOBioRadExport.h"

#include "itkImageIOBase.h"
#include <fstream>

namespace itk
{
/** \class BioRadImageIO
 *
 *  \brief ImageIO class for reading Bio-Rad images.
 *  Bio-Rad file format are used by confocal micropscopes like MRC 1024, MRC 600
 *  http://www.bio-rad.com/
 *
 * The reader/writer was based on a scanned copy of the MRC-600 documentation
 * http://forums.ni.com/attachments/ni/200/7567/1/file%20format.pdf
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOBioRad
 */
class ITKIOBioRad_EXPORT BioRadImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef BioRadImageIO        Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BioRadImageIO, Superclass);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimesion information for the current filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() ITK_OVERRIDE {}

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

protected:
  BioRadImageIO();
  ~BioRadImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void InternalReadImageInformation(std::ifstream & file);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BioRadImageIO);
};
} // end namespace itk

#endif // itkBioRadImageIO_h
