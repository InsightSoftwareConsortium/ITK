/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkImageIO_h
#define __itkImageIO_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkImageIOCommon.h"
#include <string>
#include "itkIndent.h"
#include <deque>
#include <ctype.h>

namespace itk
{

class ITK_EXPORT ImageIO : public ProcessObject
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef ImageIO            Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageIO, Superclass);

  /**
   * Default load; do whatever is appropriate for the filetype.
   */
  virtual void Load() = 0;

  /**
   * Load a 2D image. If fileName="" (the default), will read from m_FileName
   */
  virtual void Load2D(const std::string fileName="") = 0;

  /**
   * Load a 2D slice from a volume dataset.
   * fileName is file to read from. default="", which uses m_FileName instead
   * sliceNum is the slice # to load (starting at 0). (The default = 0.)
   * The offset is expressed in bytes, into fileData at which the data should 
   * be loaded.
   */
  virtual void Load2DSlice(const std::string fileName="",
                           const unsigned int sliceNum=0,
                           const unsigned int offset=0) = 0;

  /**
   * Load multiple slices to form a volume dataset.
   * The argument filePattern is the string of characters after which to 
   * start appending numbers (default is to use fName as the pattern)
   * startSlice is the starting file number (e.g. if 5, then start at 
   * "testSave005") (The default is to start w/ first slice found in 
   * directory.) The argument endSlice is the ending slice number.
   * (The default is to end w/ last consecutively numbered file in directory.)
   */
  virtual void LoadSeveralSlices (const std::string filePattern="",
                                  const int startSlice=-1,
                                  const int endSlice=-1);

  /**
   * Read a file's header to determine image dimensions, etc.
   * fileName is file to read from. default="", which uses m_FileName instead
   */
  virtual void ReadHeader(const std::string fileName="") = 0;

  /**
   * Get the image origin.
   */
  virtual float* GetImageOrigin() const =0;

  /**
   * Get the image spacing.
   */
  virtual float* GetImageSpacing() const =0;

  /**
   * Default save; do whatever is appropriate for the filetype.
   */
  virtual void Save(const std::string headerFile="", 
                    const std::string dataFile="") = 0;

  /**
   * Save a 3D image.
   */
  virtual void Save3D(const std::string headerFile="", 
                      const std::string dataFile="") = 0;

  /**
   * Returns the file extension that a particular ImageIO subclass
   * is capable of handling (e.g. .jpg, .mhd, etc.)
   * Currently only a single string is returned, but can be modified
   * so that a whole list of strings is returned.
   */
  typedef std::deque<std::string> FileExtensionsListType;
  virtual FileExtensionsListType& GetSupportedFileExtensions() const = 0;

  /**
   * Set the filename.
   */
  itkSetStringMacro(FullFileName);

  /**
   * Get the filename.
   */
  itkGetStringMacro(FullFileName);

  /**
   * Return the size in x, y, z, etc. dimension.
   */
  unsigned int GetDimensions(unsigned int i) const;

  /**
   * Return the guts of this class, FileData, which holds the raw
   * pixels of the image loaded from disk.
   */
  void* GetFileData();

  /**
   * Convenient method for accessing # bytes to get to the next pixel.
   * Returns m_Strides[1];
   */
  unsigned int GetPixelStride () const;

protected:
  /**
   * Default constructor.
   */
  ImageIO();

  /**
   * Destructor.
   */
  ~ImageIO();

  /**
   * Does the ImageIO object have enough info to be of use?
   */
  bool m_Initialized;

  /**
   * Full filename: pathname + filename + file extension.
   */
  std::string m_FullFileName;

  /**
   * Atomic pixel type being stored.
   */
  AtomicPixelType m_PixelType;

  /**
   * Stores the number of components per pixel. This will be 1 for 
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images.
   */
  unsigned int m_ComponentsPerPixel;

  /**
   * Set the number of components per pixel.
   */
  itkSetMacro(ComponentsPerPixel, unsigned int);

  /**
   * The number of dimensions in the image.
   */
  unsigned int m_NumberOfDimensions;

  /**
   * Set the number of dimensions in an image
   */
  itkSetMacro(NumberOfDimensions, unsigned int);

  /**
   * The array which stores the number of pixels in the x, y, z directions.
   */
  unsigned int m_Dimensions[ITK_MAX_DIMENSIONS];

  /**
   * Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc.
   */
  unsigned int m_Strides[ITK_MAX_DIMENSIONS];

  /**
   * Stores the raw pixels of the image
   */
  void* m_FileData;

  /**
   * Print info about myself
   */
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Return the object to an initialized state, ready to be used
   */
  virtual void Reset(const bool freeDynamic = true);

  /**
   * Resize the ImageIO object to new dimensions
   */
  void Resize(const unsigned int numDimensions, 
              const unsigned int* dimensions);

  /**
   * Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc.
   */
  void ComputeStrides();

  /**
   * Return the # of pixels in the image
   */
  unsigned int ImageSizeInPixels() const;

  /**
   * Return the # of components in the image
   */
  unsigned int ImageSizeInComponents() const;

  /**
   * Return the # of bytes in the image
   */
  unsigned int ImageSizeInBytes() const;

  /**
   * Convenient method for accessing # bytes to get to the next pixel component.
   * Returns m_Strides[0];
   */
  unsigned int GetComponentStride() const;

  /**
   * Convenient method for accessing # bytes to get to the next row.
   * Returns m_Strides[2];
   */
  unsigned int GetRowStride () const;

  /**
   * Convenient method for accessing # bytes to get to the next slice.
   * Returns m_Strides[3];
   */
  unsigned int GetSliceStride () const;

};

} // end namespace itk

#endif // __itkImageIO_h
