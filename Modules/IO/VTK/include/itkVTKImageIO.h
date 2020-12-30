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
#ifndef itkVTKImageIO_h
#define itkVTKImageIO_h
#include "ITKIOVTKExport.h"


#include <fstream>
#include "itkStreamingImageIOBase.h"

namespace itk
{
/**
 *\class VTKImageIO
 *
 *  \brief ImageIO class for reading VTK images
 *
 * This implementation was taken fron the Insight Joural:
 * https://www.insight-journal.org/browse/publication/729
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOVTK
 */
class ITKIOVTK_EXPORT VTKImageIO : public StreamingImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKImageIO);

  /** Standard class type aliases. */
  using Self = VTKImageIO;
  using Superclass = StreamingImageIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageIO, StreamingImageIOBase);

  // see super class for documentation
  //
  // overridden to return true only when supported
  bool
  CanStreamWrite() override;

  // see super class for documentation
  //
  // overridden to return true only when supported
  bool
  CanStreamRead() override;


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

  /** returns the header size, if it is unknown it will return 0 */
  SizeType
  GetHeaderSize() const override
  {
    return this->m_HeaderSize;
  }

protected:
  VTKImageIO();
  ~VTKImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InternalReadImageInformation(std::ifstream & file);

  // Internal function to get next line from a given file (*.vtk)
  int
  GetNextLine(std::ifstream & ifs, std::string & line, bool lowerCase = true, SizeValueType count = 0);

  void
  WriteImageInformation(const void * buffer);

  void
  ReadHeaderSize(std::ifstream & file);

  /** Convenient method to read a buffer as ASCII text. */
  void
  ReadBufferAsASCII(std::istream & is, void * buffer, IOComponentEnum ctype, SizeType numComp) override;

  /** Convenient method to write a buffer as ASCII text. */
  void
  WriteBufferAsASCII(std::ostream & os, const void * buffer, IOComponentEnum ctype, SizeType numComp) override;

  /** We have a special method to read symmetric second rank tensors because
   * the VTK file format expands the symmetry and only supports 3D tensors. */
  virtual void
  ReadSymmetricTensorBufferAsBinary(std::istream & is, void * buffer, StreamingImageIOBase::SizeType num);

  /** We have a special method to write symmetric second rank tensors because
   * the VTK file format expands the symmetry and only supports 3D tensors. */
  virtual void
  WriteSymmetricTensorBufferAsBinary(std::ostream & os, const void * buffer, StreamingImageIOBase::SizeType num);

private:
  void
              SetPixelTypeFromString(const std::string & pixelType);
  std::string GetComponentTypeAsString(IOComponentEnum);

  /** Return the number of pixels in the IOregion. */
  SizeType
  GetIORegionSizeInPixels() const;

  /** Return the number of bytes in the IOregion. */
  SizeType
  GetIORegionSizeInBytes() const;

  /** Return the number of pixels times the number
   * of components in the IOregion. */
  SizeType
  GetIORegionSizeInComponents() const;

  SizeType m_HeaderSize;
};
} // end namespace itk

#endif // itkVTKImageIO_h
