/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
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
		return;
		// Need to throw an exception instead
	}

	m_LightObjectIO = ObjectFactoryBase::CreateInstance(ExtractFileExtension(m_FileToLoad.c_str()));
	m_IO = dynamic_cast<ImageIO*>((LightObject*) m_LightObjectIO);

  if (m_IO == NULL)
  {
    return;
		// Need to throw an exception instead
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


  typedef typename TOutputImage::PixelType  PixelType;

  typedef SimpleImageRegionIterator< TOutputImage> IteratorType;

  IteratorType it(m_OutputImage,
                  m_OutputImage->GetLargestPossibleRegion());

  PixelType * source = (PixelType *) m_IO->GetFileData();


  it.Begin();
  while( !it.IsAtEnd() )
  {
    it.Set( *source++ );
    ++it;
  }
}

template <class TOutputImage>
void FileIOToImageFilter<TOutputImage>::SetIO(ImageIO *io)
{
  m_IO = io;
}

template <class TOutputImage>
ImageIO* FileIOToImageFilter<TOutputImage>::GetIO()
{
  return m_IO;
}

} //namespace ITK
