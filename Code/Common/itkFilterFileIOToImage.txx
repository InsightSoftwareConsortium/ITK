#include "itkObjectFactory.h"
#include "itkImageRegionSimpleIterator.h"
#include "itkMaker.h"

namespace itk
{

template <class TOutputImage>
FilterFileIOToImage<TOutputImage>::FilterFileIOToImage(std::string fileName)
{
	const Maker<FileIOMaker::KeyType, FileIO> maker;

	m_IO = (FileIO*) maker.Create(ExtractFileExtension(fileName.c_str()));
	if (m_IO == NULL)
		return;

	m_IO->SetFullFileName(fileName.c_str());
	m_IO->Load();
}

template <class TOutputImage>
void FilterFileIOToImage<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer m_OutputImage = GetOutput();

  Size dimSize;

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++) 
	{
		dimSize[i] = m_IO->GetDimensions(i);
	}

  const long startPosition[] = { 0, 0, 0 };
  typename TOutputImage::Index start;
  start.SetIndex( startPosition );

  Region region;
  
  region.SetSize(dimSize);
  region.SetIndex(start);

  m_OutputImage->SetLargestPossibleRegion(region);
  m_OutputImage->SetBufferedRegion(region);
  m_OutputImage->Allocate();
    

  typedef typename TOutputImage::PixelType  PixelType;

  typedef itk::ImageRegionSimpleIterator< TOutputImage> IteratorType;
  
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
void FilterFileIOToImage<TOutputImage>::SetIO(FileIO *io)
{
	m_IO = io;
}

} //namespace ITK