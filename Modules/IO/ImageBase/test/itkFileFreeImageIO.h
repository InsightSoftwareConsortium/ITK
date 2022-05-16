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
#ifndef itkFileFreeImageIO_h
#define itkFileFreeImageIO_h


#include "itkImageIOBase.h"
#include <vector>
#include <string>

namespace itk
{
/** \class FileFreeImageIO
 * \brief ImageIO object for reading images from memory
 *
 * The "filename" specified will look like a URI:
 *     fileFree:
 *
 */
class FileFreeImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FileFreeImageIO);

  /** Standard class type aliases. */
  using Self = FileFreeImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIO, ImageIOBase);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
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

  /** Writes the header of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

protected:
  FileFreeImageIO();
  ~FileFreeImageIO();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  SplitString(const std::string & text, const std::string & separators, std::vector<std::string> & words);
};

} // end namespace itk
#endif // itkFileFreeImageIO_h
