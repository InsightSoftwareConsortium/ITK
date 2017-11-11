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
#ifndef itkVTKImageIO_h
#define itkVTKImageIO_h
#include "ITKIOVTKExport.h"


#include <fstream>
#include "itkStreamingImageIOBase.h"

namespace itk
{
/** \class VTKImageIO
 *
 *  \brief ImageIO class for reading VTK images
 *
 * This implementation was taken fron the Insight Joural:
 * https://hdl.handle.net/10380/3171
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOVTK
 */
class ITKIOVTK_EXPORT VTKImageIO:
  public StreamingImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef VTKImageIO                 Self;
  typedef StreamingImageIOBase       Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageIO, StreamingImageIOBase);

// see super class for documentation
  //
  // overidden to return true only when supported
  virtual bool CanStreamWrite(void) ITK_OVERRIDE;

  // see super class for documentation
  //
  // overidden to return true only when supported
  virtual bool CanStreamRead(void) ITK_OVERRIDE;


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

  /** returns the header size, if it is unknown it will return 0 */
  virtual SizeType GetHeaderSize() const ITK_OVERRIDE { return this->m_HeaderSize; }

protected:
  VTKImageIO();
  ~VTKImageIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void InternalReadImageInformation(std::ifstream & file);

  // Internal function to get next line from a given file (*.vtk)
  int GetNextLine(std::ifstream& ifs, std::string& line, bool lowerCase = true, SizeValueType count= 0);

  void WriteImageInformation(const void *buffer);

  void ReadHeaderSize(std::ifstream & file);

  /** Convenient method to read a buffer as ASCII text. */
  virtual void ReadBufferAsASCII(std::istream & os, void *buffer,
                         IOComponentType ctype,
                         SizeType numberOfBytesToBeRead) ITK_OVERRIDE;

  /** Convenient method to write a buffer as ASCII text. */
  virtual void WriteBufferAsASCII(std::ostream & os, const void *buffer,
                          IOComponentType ctype,
                          SizeType numberOfBytesToWrite) ITK_OVERRIDE;

  /** We have a special method to read symmetric second rank tensors because
   * the VTK file format expands the symmetry and only supports 3D tensors. */
  virtual void ReadSymmetricTensorBufferAsBinary(std::istream& os,
    void *buffer,
    StreamingImageIOBase::SizeType num);

  /** We have a special method to write symmetric second rank tensors because
   * the VTK file format expands the symmetry and only supports 3D tensors. */
  virtual void WriteSymmetricTensorBufferAsBinary(std::ostream& os,
    const void *buffer,
    StreamingImageIOBase::SizeType num);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKImageIO);
  void SetPixelTypeFromString(const std::string & pixelType);
  std::string GetComponentTypeAsString(IOComponentType);

  /** Return the number of pixels in the IOregion. */
  SizeType GetIORegionSizeInPixels() const;

  /** Return the number of bytes in the IOregion. */
  SizeType GetIORegionSizeInBytes() const;

  /** Return the number of pixels times the number
   * of components in the IOregion. */
  SizeType GetIORegionSizeInComponents() const;

  SizeType m_HeaderSize;
};
} // end namespace itk

#endif // itkVTKImageIO_h
