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
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template <class TOutputImage>
FileIOToImageFilter<TOutputImage>
::FileIOToImageFilter()
{
  m_IO = NULL;
  m_FileName = "";
  m_FilePrefix = "";
  m_FilePattern = "";
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>::Update()
{
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    //todo: give a message
    throw FileIOException(__FILE__, __LINE__);
    }

  if ( m_IO == 0 ) //try creating via factory
    {
    m_LightObjectIO = ObjectFactoryBase::
      CreateInstance(ExtractFileExtension(m_FileName.c_str()));
    m_IO = dynamic_cast<ImageIO*>((LightObject*) m_LightObjectIO);
    }

  if ( m_IO == 0 )
    {
    //todo: give a message
    throw FileIOException(__FILE__, __LINE__);
    }

  m_IO->SetFileName(m_FileName.c_str());
  m_IO->SetFilePrefix(m_FilePrefix.c_str());
  m_IO->SetFilePattern(m_FilePattern.c_str());
  
  if (this->GetOutput(0))
    {
    this->GetOutput(0)->Update();
    }
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer m_OutputImage = this->GetOutput();
  Size dimSize;

  m_IO->Update(); //causes the helper to read

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
  m_OutputImage->SetOrigin( m_IO->GetOrigin() );
  m_OutputImage->SetSpacing( m_IO->GetSpacing() );

  typedef typename TOutputImage::PixelType  OutputPixelType;
  typedef ImageRegionIteratorWithIndex< TOutputImage> IteratorType;

  IteratorType it(m_OutputImage,
                  m_OutputImage->GetLargestPossibleRegion());

  OutputPixelType* source = (OutputPixelType*) m_IO->GetFileData();

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it, ++source )
    {
    //todo: progress
    it.Set(*source);
    }
}


} //namespace ITK

#endif
