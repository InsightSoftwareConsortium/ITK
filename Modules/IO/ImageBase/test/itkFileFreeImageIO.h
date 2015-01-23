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
  /** Standard class typedefs. */
  typedef FileFreeImageIO            Self;
  typedef ImageIOBase                Superclass;
  typedef SmartPointer<Self>         Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIO, ImageIOBase);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Writes the header of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);

protected:
  FileFreeImageIO();
  ~FileFreeImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FileFreeImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void SplitString (const std::string &text,
                    const std::string &separators,
                    std::vector<std::string> &words);
};

} // end namespace itk
#endif // itkFileFreeImageIO_h
