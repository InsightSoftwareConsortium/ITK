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
#ifndef itkStimulateImageIO_h
#define itkStimulateImageIO_h
#include "ITKIOStimulateExport.h"


#include <fstream>
#include "itkImageIOBase.h"

namespace itk
{
/** \class StimulateImageIO
 *
 *  \brief ImageIO class for reading SDT/SPR (Stimulate) images
 *  This format is similar to a MetaImageIO file:
 *  The user should specify the .spr file (not the data file : .sdt)
 *
 *  This is based on the notes from:
 *
 *      http://www.cmrr.umn.edu/stimulate/stimUsersGuide/node57.html
 *
 *  It has been tested on:
 *
 *      ftp://ftp.cmrr.umn.edu/pub/stimulate/data/
 *
 *  \warning Beware of the scalar range of some files (See GetDisplayRange).
 *
 * \author Mathieu Malaterre
 *  CREATIS team at INSA - Lyon 2002
 *    http://www.creatis.insa-lyon.fr
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOStimulate
 */
class ITKIOStimulate_EXPORT StimulateImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StimulateImageIO);

  /** Standard class type aliases. */
  using Self = StimulateImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StimulateImageIO, Superclass);

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

  /** Two values used for applying intensity windowing to the data set. The pair
   *  of numbers represent a low value and a hight value. Pixel values below the
   *  low_value will be displayed as black and voxels with values above the
   *  high_value will be displayed as white. Pixels with values within the display
   *  range are displayed with a grey value that is scaled linearly between the
   *  low_value and high_value. */
  itkGetVectorMacro(DisplayRange, const float, 2);
  const float &
  GetHighDisplayValue()
  {
    return m_DisplayRange[1];
  }
  const float &
  GetLowDisplayValue()
  {
    return m_DisplayRange[1];
  }

protected:
  StimulateImageIO();
  ~StimulateImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InternalReadImageInformation(std::ifstream & file);

private:
  std::string m_DataFileName;
  char        m_SdtOrient[256];
  float       m_DisplayRange[2];
  char        m_FidName[256];
};
} // end namespace itk

#endif // itkStimulateImageIO_h
