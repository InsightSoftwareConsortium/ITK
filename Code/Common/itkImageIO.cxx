#include "itkImageIO.h"

namespace itk
{

ImageIO::ImageIO()
{
  Reset(false);
}

void ImageIO::Reset(const bool freeDynamic)
{
  m_Initialized = false;
  m_FullFileName = "";
  m_PixelType = ITK_DOUBLE;
  m_ComponentsPerPixel = 0;
  m_NumDimensions = 0;
  for (unsigned int i=0; i < ITK_MAX_DIMENSIONS; i++)
  {
    m_Dimensions[i] = 0;
    m_Strides[i] = 0;
  }
  if (freeDynamic)
  {
    if (m_FileData != NULL)
    {
      delete [] (char*) m_FileData;
    }
  }
  m_FileData = NULL;

}

ImageIO::~ImageIO()
{
  Reset();
}

void ImageIO::Print(std::ostream& os)
{
  Indent newIndent;

  ImageIO::PrintSelf(os, newIndent);
}

void ImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  indent = indent.GetNextIndent();
  os << indent << "Filename: " << m_FullFileName << std::endl;
  os << indent << "# Components/Pixel: " << m_ComponentsPerPixel;
  os << ", PixelType: " << AtomicPixelTypeToString(m_PixelType) << std::endl;
  os << indent << "Dimensions: ";
  for (unsigned int i=0; i < m_NumDimensions; i++)
  {
    os << m_Dimensions[i] << " ";
  }
  os << std::endl;
}

void ImageIO::Resize(const unsigned int numDimensions,
                    const unsigned int* dimensions)
{
  m_NumDimensions = numDimensions;
  if (dimensions != NULL)
  {
    for (unsigned int i=0; i < m_NumDimensions; i++)
    {
      m_Dimensions[i] = dimensions[i];
    }
    CalcStrides();
    delete [] (char*) m_FileData;
    m_FileData = new unsigned char[ImageSizeInBytes()];
  }
}

void ImageIO::CalcStrides()
{
  unsigned int i;

  m_Strides[0] = CalcSizeOfAtomicPixelType(m_PixelType);
  m_Strides[1] = m_ComponentsPerPixel * m_Strides[0];
  for (i = 2; i <= m_NumDimensions; i++)
  {
    m_Strides[i] = m_Dimensions[i-2] * m_Strides[i-1];
  }
}

void ImageIO::LoadSeveralSlices (const std::string filePattern,
                                const int startSlice,
                                const int endSlice)
{
  /*
   * Not yet implemented, because currently requires code from FLTK
   */
}

// Calculates the image size in PIXELS
unsigned int ImageIO::ImageSizeInPixels() const
{
  unsigned int i;
  unsigned int numPixels = 1;

  for (i = 0; i < m_NumDimensions; i++)
  {
    numPixels *= m_Dimensions[i];
  }

  return numPixels;
}

unsigned int ImageIO::ImageSizeInComps() const
{
  return ImageSizeInPixels() * m_ComponentsPerPixel;
}

unsigned int ImageIO::ImageSizeInBytes () const
{
  return (ImageSizeInComps() * CalcSizeOfAtomicPixelType(m_PixelType));
}

unsigned int ImageIO::GetDimensions(unsigned int i) const
{
  if (i > m_NumDimensions)
  {
    return 0;
  }
  else
  {
    return m_Dimensions[i];
  }
}

void* ImageIO::GetFileData()
{
  return m_FileData;
}

unsigned int ImageIO::GetComponentStride() const
{
  return m_Strides[0];
}

unsigned int ImageIO::GetPixelStride () const
{
  return m_Strides[1];
}

unsigned int ImageIO::GetRowStride () const
{
  return m_Strides[2];
}

unsigned int ImageIO::GetSliceStride () const
{
  return m_Strides[3];
}

} //namespace itk
