/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReader.h
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
// ***** NOTE REMOVE ME TO COMPILE
#define __itkImageFileReader_h
#ifndef __itkImageFileReader_h
#define __itkImageFileReader_h

#include "itkImageIO.h"
#include "itkImageSource.h"
#include "itkImage.h"
#include "itkExceptionObject.h"

namespace itk
{

/**
 * \brief Base exception class for IO conflicts
 */
class FileIOException : public ExceptionObject 
{
public:
  itkTypeMacro( FileIOException, ExceptionObject );

  FileIOException() 
  {
    SetDescription("Problem during File IO");
  }
};


/**
 * \brief Filter to convert input data to a particular internal type
 *
 *  TOutputImage is the type expected by the external users of the 
 *  filter. Data comming from a file can be stored in any other format
 *  (or type) this filter converts data between the file type and the
 *  external expected type.
 *
 *  The Pluggable factory pattern is used. Different kinds of readers
 *  can be registered (even at run time) without having to modify the
 *  code in this class.
 *
 */
template <class TOutputImage>
class ITK_EXPORT ImageFileReader : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageFileReader         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageFileReader, ImageSource);

  /**
   * typedef for Size.
   */
  typedef Size<TOutputImage::ImageDimension>  Size;

  /**
   * typedef for Region.
   */
  typedef ImageRegion<TOutputImage::ImageDimension>  Region;

  /**
   * Specify the file to load. This is forwarded to the IO instance. 
   * Either the FileName or FilePrefix plus pattern are used to read
   * files.
   */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** 
   * Specify file prefix for the image file(s). You should specify either
   * a FileName or FilePrefix. Use FilePrefix if the data is stored
   * in multiple files. (Note: the FileName ivar is available from the
   * superclass.)
   */
  itkSetStringMacro(FilePrefix);
  itkGetStringMacro(FilePrefix);

  /**
   * The sprintf format used to build filename from FilePrefix and number.
   */
  itkSetStringMacro(FilePattern);
  itkGetStringMacro(FilePattern);

  /**
   * Set/Get the ImageIO helper class. Often this is created via the object
   * factory mechanism that determines whether a particular ImageIO can
   * read a certain file. This method provides a way to get the ImageIO 
   * instance that is created.
   */
  itkSetObjectMacro(ImageIO,ImageIOBase);
  itkGetObjectMacro(ImageIO,ImageIOBase);

protected:
  ImageFileReader();
  ~ImageFileReader();
  ImageFileReader(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  //Does the real work
  virtual void GenerateData();

  ImageIOBase::Pointer m_ImageIO;
  bool m_UserSpecified; //keep track whether the ImageIO is user specified

  std::string m_FileName;
  std::string m_FilePrefix;
  std::string m_FilePattern;
  
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFileReader.txx"
#endif

#endif // __itkImageFileReader_h
