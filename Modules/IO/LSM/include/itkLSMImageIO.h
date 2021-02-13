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
#ifndef itkLSMImageIO_h
#define itkLSMImageIO_h
#include "ITKIOLSMExport.h"

#include "itkTIFFImageIO.h"
#include <fstream>

namespace itk
{
/**
 *\class LSMImageIO
 *
 *  \brief ImageIO class for reading LSM (Zeiss) images
 * LSM is a line of confocal laser scanning microscopes produced by the Zeiss company
 * LSM files are essentially extensions of the TIFF multiple image stack file format.
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOLSM
 */
class ITKIOLSM_EXPORT LSMImageIO : public TIFFImageIO
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LSMImageIO);

  /** Standard class type aliases. */
  using Self = LSMImageIO;
  using Superclass = TIFFImageIO;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LSMImageIO, TIFFImageIO);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the current filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override
  {}

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

protected:
  LSMImageIO();
  ~LSMImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  FillZeissStruct(char * cz);
};
} // end namespace itk

#endif // itkLSMImageIO_h
