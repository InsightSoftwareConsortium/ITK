/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFileIOToImageFilter_txx
#define _itkFileIOToImageFilter_txx
#include "itkFileIOToImageFilter.h"

#include "itkObjectFactory.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCommand.h"

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

  this->InvokeEvent( StartEvent() );

  typename TOutputImage::Pointer m_OutputImage = this->GetOutput();
  Size dimSize;

  m_IO->Update(); //causes the helper to read

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_IO->GetDimensions(i);
    }

  typedef typename TOutputImage::IndexType IndexType;

  IndexType start = IndexType::ZeroIndex;

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

  // support progress methods/callbacks
  unsigned long numberOfPixels = 
    m_OutputImage->GetLargestPossibleRegion().GetNumberOfPixels();
  unsigned long visitPeriod  = 100;
  unsigned long updateVisits = numberOfPixels / visitPeriod;
  unsigned long visitCounter = 0;
 

  OutputPixelType* source = (OutputPixelType*) m_IO->GetFileData();

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it, ++source )
    {
    if( visitCounter == updateVisits )
      {
        visitCounter = 0;
        this->UpdateProgress( static_cast<float>( visitCounter ) /
                              static_cast<float>( updateVisits * visitPeriod ) );
      }

    it.Set(*source);
    visitCounter++;
    }
  
  this->InvokeEvent( EndEvent() );
  
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  std::cout << "FilePrefix: " << m_FilePrefix << std::endl;
  std::cout << "FilePattern: " << m_FilePattern << std::endl;
  std::cout << "FileName: " << m_FileName << std::endl;
  if (m_IO)
    {
    std::cout << "IO: " << m_IO << std::endl;
    }
}

} //namespace ITK

#endif
