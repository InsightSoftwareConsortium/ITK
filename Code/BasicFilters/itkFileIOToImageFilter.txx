/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.txx
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
#ifndef _itkFileIOToImageFilter_txx
#define _itkFileIOToImageFilter_txx

#include "itkObjectFactory.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{

template <class TOutputImage>
FileIOToImageFilter<TOutputImage>::FileIOToImageFilter(std::string fileName)
{
  m_LightObjectIO = ObjectFactoryBase::CreateInstance(ExtractFileExtension(fileName.c_str()));
  m_IO = dynamic_cast<ImageIO*>((LightObject*) m_LightObjectIO);
  if (m_IO == NULL)
    {
    return;
    }

  m_IO->SetFullFileName(fileName.c_str());
  m_IO->Load();
}

template <class TOutputImage>
FileIOToImageFilter<TOutputImage>::FileIOToImageFilter()
{
  m_IO = NULL;
  m_FileToLoad = "";
}

template <class TOutputImage>
FileIOToImageFilter<TOutputImage>::~FileIOToImageFilter()
{
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>::LoadFile()
{
  if (m_FileToLoad == "")
    {
    throw FileIOException();
    }

  m_LightObjectIO = ObjectFactoryBase::CreateInstance(ExtractFileExtension(m_FileToLoad.c_str()));
  m_IO = dynamic_cast<ImageIO*>((LightObject*) m_LightObjectIO);

  if ( m_IO == 0 )
    {
    throw FileIOException();
    }

  m_IO->SetFullFileName(m_FileToLoad.c_str());
  m_IO->Load();
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer m_OutputImage = GetOutput();

  Size dimSize;

  LoadFile();

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_IO->GetDimensions(i);
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
  m_OutputImage->SetOrigin( m_IO->GetImageOrigin() );
  m_OutputImage->SetSpacing( m_IO->GetImageSpacing() );


  typedef typename TOutputImage::PixelType  OutputPixelType;

  typedef SimpleImageRegionIterator< TOutputImage> IteratorType;

  IteratorType it(m_OutputImage,
                  m_OutputImage->GetLargestPossibleRegion());

  OutputPixelType* source = (OutputPixelType*) m_IO->GetFileData();

  it.Begin();
  while(!it.IsAtEnd())
    {
    it.Set(*source++);
    ++it;
    }
}


} //namespace ITK

#endif
