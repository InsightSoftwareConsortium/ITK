#include "itkObjectFactory.h"
#include "itkImageRegionSimpleIterator.h"

namespace itk
{

template <class TOutputImage>
FilterFileIOToImage<TOutputImage>::FilterFileIOToImage(std::string fileName)
{
	m_LightObjectIO = ObjectFactoryBase::CreateInstance(ExtractFileExtension(fileName.c_str()));
	m_IO = dynamic_cast<FileIO*>((LightObject*) m_LightObjectIO);
  if (m_IO == NULL)
  {
    return;
  }

  m_IO->SetFullFileName(fileName.c_str());
  m_IO->Load();
}

template <class TOutputImage>
FilterFileIOToImage<TOutputImage>::FilterFileIOToImage()
{
  m_IO = NULL;
	m_FileToLoad = "";
}

template <class TOutputImage>
FilterFileIOToImage<TOutputImage>::~FilterFileIOToImage()
{
}

template <class TOutputImage>
void FilterFileIOToImage<TOutputImage>::LoadFile()
{
	if (m_FileToLoad == "")
	{
		return;
		// Need to throw an exception instead
	}

	m_LightObjectIO = ObjectFactoryBase::CreateInstance(ExtractFileExtension(m_FileToLoad.c_str()));
	m_IO = dynamic_cast<FileIO*>((LightObject*) m_LightObjectIO);

  if (m_IO == NULL)
  {
    return;
		// Need to throw an exception instead
  }

  m_IO->SetFullFileName(m_FileToLoad.c_str());
  m_IO->Load();
}

template <class TOutputImage>
void FilterFileIOToImage<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer m_OutputImage = GetOutput();

  Size dimSize;

	LoadFile();

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
  {
    dimSize[i] = m_IO->GetDimensions(i);
  }

  const long startPosition[] = { 0, 0, 0 };
  typename TOutputImage::IndexType start;
  start.SetIndex( startPosition );

  Region region;

  region.SetSize(dimSize);
  region.SetIndex(start);

  m_OutputImage->SetLargestPossibleRegion(region);
  m_OutputImage->SetBufferedRegion(region);
  m_OutputImage->Allocate();


  typedef typename TOutputImage::PixelType  PixelType;

  typedef ImageRegionSimpleIterator< TOutputImage> IteratorType;

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

template <class TOutputImage>
FileIO* FilterFileIOToImage<TOutputImage>::GetIO()
{
  return m_IO;
}

} //namespace ITK
