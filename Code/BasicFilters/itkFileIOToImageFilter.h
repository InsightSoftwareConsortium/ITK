/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.h
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
#ifndef __itkFileIOToImageFilter_h
#define __itkFileIOToImageFilter_h

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
class ITK_EXPORT FileIOToImageFilter : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FileIOToImageFilter         Self;

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
   * A useful constructor
   */
  FileIOToImageFilter(std::string fileName);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FileIOToImageFilter, ImageSource);

  /**
   * typedef for Size.
   */
  typedef Size<TOutputImage::ImageDimension>  Size;

  /**
   * typedef for Region.
   */
  typedef ImageRegion<TOutputImage::ImageDimension>  Region;

  /**
   * Set the ImageIO helper class.
   */
  itkSetObjectMacro(IO,ImageIO);

  /**
   * Get the ImageIO helper class.
   */
  itkGetObjectMacro(IO,ImageIO);

  /**
   * Set m_FileToLoad
   */
  itkSetStringMacro(FileToLoad);

protected:
  void GenerateData();
  FileIOToImageFilter();
  ~FileIOToImageFilter();
  void LoadFile();

  ImageIO::Pointer m_IO;
  LightObject::Pointer m_LightObjectIO;
  std::string m_FileToLoad;
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFileIOToImageFilter.txx"
#endif

#endif // __itkFileIOToImageFilter_h
