/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReader.txx
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
#ifndef _itkImageFileReader_txx
#define _itkImageFileReader_txx

#include "itkObjectFactory.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImageIOFactory.h"
#include "itkConvertPixelBuffer.h"


namespace itk
{

// BILL: You were missing the matching template arguments on the method
// implementations in itkImageFileReader.txx.
template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>
::ImageFileReader()
{
  m_ImageIO = 0;
  m_FileName = "";
  m_FilePrefix = "";
  m_FilePattern = "";
}

template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>::~ImageFileReader()
{
}
template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << "m_ImageIO: " << m_ImageIO << "\n";
  os << indent << "m_UserSpecified m_ImageIO flag: " << m_UserSpecified << "\n";
  os << indent << "m_FileName: " << m_ImageIO << "\n";
  os << indent << "m_FilePrefix: " << m_FilePrefix << "\n";
  os << indent << "m_FilePattern: " << m_FilePattern << "\n";
  
}


template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>::GenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro(<<"Reading file" << m_FileName);
  
  // Check to see if we can read the file given the name or prefix
  //
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    throw ImageFileReaderException(__FILE__, __LINE__, "Bad File Name");
    }

  if ( m_ImageIO == 0 ) //try creating via factory
    {
    m_UserSpecified = false;
    m_ImageIO = ImageIOFactory::CreateImageIO(m_FileName.c_str());
    }
  else
    {
    m_UserSpecified = true;
    }
  
  if ( m_ImageIO == 0 )
    {
    throw ImageFileReaderException(__FILE__, __LINE__, "Could not create IO object for file name");
    return;
    }

  // Got to allocate space for the image. Determine the characteristics of
  // the image.
  //
  Size dimSize;
  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_ImageIO->GetDimensions(i);
    }
  output->SetOrigin( m_ImageIO->GetOrigin() );
  output->SetSpacing( m_ImageIO->GetSpacing() );

  const unsigned long startPosition[] = { 0, 0, 0 };
  typename TOutputImage::IndexType start;
  start.SetIndex( startPosition );

  Region region;
  region.SetSize(dimSize);
  region.SetIndex(start);
  ImageIORegion ioRegion(TOutputImage::ImageDimension);
  ImageIORegion::SizeType ioSize = ioRegion.GetSize();
  ImageIORegion::IndexType ioStart = ioRegion.GetIndex();
  for(unsigned int i = 0; i < dimSize.GetSizeDimension(); ++i)
    {
    ioSize[i] = dimSize[i];
    }
  for(unsigned int i = 0; i < start.GetIndexDimension(); ++i)
    {
    ioStart[i] = start[i];
    }
  ioRegion.SetSize(ioSize);
  ioRegion.SetIndex(ioStart);
  
  
  output->SetLargestPossibleRegion(region);
  output->SetBufferedRegion(region);
  output->Allocate();
  
  // Tell the ImageIO to read the file
  //
  OutputImagePixelType *buffer = 
    output->GetPixelContainer()->GetBufferPointer();
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->SetLoadRegion(ioRegion);

  if ( m_ImageIO->GetPixelType() == typeid(TOutputImage::PixelType) &&
       (m_ImageIO->GetNumberOfComponents() == 
        ConvertPixelTraits::GetNumberOfComponents()))
    {
    // allocate a buffer and have the ImageIO read directly into it
    m_ImageIO->Load(buffer);
    return;
    }
  else // a type conversion is necessary
    {
    OutputImagePixelType* loadBuffer = 
      new OutputImagePixelType[m_ImageIO->GetImageSizeInBytes()];
    m_ImageIO->Load(loadBuffer);
    this->DoConvertBuffer(loadBuffer, region.GetNumberOfPixels());
    delete [] loadBuffer;
    }
}


template <class TOutputImage, class ConvertPixelTraits>
void 
ImageFileReader<TOutputImage, ConvertPixelTraits>
::DoConvertBuffer(void* inputData,
                  unsigned long numberOfPixels)
{
  std::cerr << "number of pixels " << numberOfPixels << "\n";
  // get the pointer to the destination buffer
  OutputImagePixelType *outputData =
    this->GetOutput()->GetPixelContainer()->GetBufferPointer();

// Create a macro as this code is a bit lengthy and repetitive
// if the ImageIO pixel type is typeid(type) then use the ConvertPixelBuffer
// class to convert the data block to TOutputImage's pixel type
// see DefaultConvertPixelTraits and ConvertPixelBuffer
#define ITK_CONVERT_BUFFER_IF_BLOCK(type)               \
 else if( m_ImageIO->GetPixelType() == typeid(type) )   \
    {                                                   \
    ConvertPixelBuffer<                                 \
      type,                                             \
      OutputImagePixelType,                             \
      ConvertPixelTraits                                \
      >                                                 \
      ::Convert(                                        \
        static_cast<type*>(inputData),                  \
        m_ImageIO->GetNumberOfComponents(),             \
        outputData,                                     \
        numberOfPixels);                                \
    }
  if(0)
    {
    }
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned char)
  ITK_CONVERT_BUFFER_IF_BLOCK(char)
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned short)
  ITK_CONVERT_BUFFER_IF_BLOCK( short)
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned int)
  ITK_CONVERT_BUFFER_IF_BLOCK( int)
  ITK_CONVERT_BUFFER_IF_BLOCK(float)
  ITK_CONVERT_BUFFER_IF_BLOCK( double)
  else
    {
    itkErrorMacro(<<"Couldn't convert pixel type");
    return;
    }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}


} //namespace ITK

#endif
