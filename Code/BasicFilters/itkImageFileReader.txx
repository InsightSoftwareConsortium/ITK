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

namespace itk
{

template <class TOutputImage>
ImageFileReader<TOutputImage>
::ImageFileReader(std::string fileName)
{
  m_IO = NULL;
  m_FileName = fileName.c_str();
  m_FilePrefix = "";
  m_FilePattern = "";
}

template <class TOutputImage>
ImageFileReader<TOutputImage>
::ImageFileReader()
{
  m_IO = NULL;
  m_FileName = "";
  m_FilePrefix = "";
  m_FilePattern = "";
}

template <class TOutputImage>
ImageFileReader<TOutputImage>::~ImageFileReader()
{
}

template <class TOutputImage>
void ImageFileReader<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer m_OutputImage = this->GetOutput();
  Size dimSize;


  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    throw FileIOException();
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
    throw FileIOException();
    }

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_ImageIO->GetDimensions(i);
    }
  m_OutputImage->SetOrigin( m_ImageIO->GetOrigin() );
  m_OutputImage->SetSpacing( m_ImageIO->GetSpacing() );


  // Now tell the ImageIO to read the file
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->SetLoadRegion(region);
  if ( m_ImageIO->GetPixelType() == typeid(TOutputImage::PixelType) )
    {
    // allocate a buffer
    m_ImageIO->Load(buffer);
    return;
    }

  // Do a type conversion
  m_ImageIO->Load();
  if( m_ImageIO->GetPixelType() == typeid(unsigned char) )
    {
    ConvertBuffer<unsigned char, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                          buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( char) )
    {
    ConvertBuffer<char, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                 buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( unsigned short) )
    {
    ConvertBuffer<unsigned short, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                           buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( short) )
    {
    ConvertBuffer< short, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                   buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( unsigned int) )
    {
    ConvertBuffer< unsigned int, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                          buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid(  int) )
    {
    ConvertBuffer< int, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                 buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( float ) )
    {
    ConvertBuffer< float, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                 buffer);
    }
  else if (m_ImageIO->GetPixelType() == typeid( double ) )
    {
    ConvertBuffer< float, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
                                                   buffer);
    }
//    else if (m_ImageIO->GetPixelType() == typeid( RGBPixel ) )
//      {
//      ConvertBuffer< RGBPixel, TOutputImage::PixelType>(m_ImageIO->GetRequestedRegionData(),
//                                                     buffer);
//      }
  else
    {
    itkErrorMacro(<<"Couldn't convert pixel type");
    return;
    }



  const unsigned long startPosition[] = { 0, 0, 0 };
  typename TOutputImage::IndexType start;
  start.SetIndex( startPosition );

  Region region;

  region.SetSize(dimSize);
  region.SetIndex(start);

  m_OutputImage->SetLargestPossibleRegion(region);
  m_OutputImage->SetBufferedRegion(region);
  m_OutputImage->Allocate();

  typedef typename TOutputImage::PixelType  OutputPixelType;
  typedef SimpleImageRegionIterator< TOutputImage> IteratorType;

  IteratorType it(m_OutputImage,
                  m_OutputImage->GetLargestPossibleRegion());

  OutputPixelType* source = (OutputPixelType*) m_IO->GetFileData();

  for ( it.Begin(); !it.IsAtEnd(); ++it, ++source )
    {
    it.Set(*source);
    }
}


} //namespace ITK

#endif
