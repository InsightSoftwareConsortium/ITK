#include "itkFileIO.h"

namespace itk
{

FileIO::FileIO()
{
  Reset(false);
}

void FileIO::Reset(const bool freeDynamic)
{
  m_Initialized = false;
  m_FullFileName = "";
  m_PixelType = ITK_DOUBLE;
  m_ComponentsPerPixel = 0;
  m_NumDimensions = 0;
  for (int i=0; i < ITK_MAX_DIMENSIONS; i++)
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

FileIO::~FileIO()
{
  Reset();
}

void FileIO::Print(std::ostream& os)
{
  Indent newIndent;

  FileIO::PrintSelf(os, newIndent);
}

void FileIO::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);
  indent = indent.GetNextIndent();
  os << indent << "Filename: " << m_FullFileName << std::endl;
  os << indent << "# Components/Pixel: " << m_ComponentsPerPixel;
  os << ", PixelType: " << AtomicPixelTypeToString(m_PixelType) << std::endl;
  os << indent << "Dimensions: ";
  for (int i=0; i < m_NumDimensions; i++)
  {
    os << m_Dimensions[i] << " ";
  }
  os << std::endl;
}

/*std::string FileIO::TAtomicPixelToString(const TAtomicPixel pixelType) const
{
  switch(pixelType)
  {
    case ITK_UCHAR:
      return "unsigned char";
      break;
    case ITK_CHAR:
      return "char";
      break;
    case ITK_USHORT:
      return "unsigned short";
      break;
    case ITK_SHORT:
      return "short";
      break;
    case ITK_UINT:
      return "unsigned int";
      break;
    case ITK_INT:
      return "int";
      break;
    case ITK_ULONG:
      return "unsigned long";
      break;
    case ITK_LONG:
      return "long";
      break;
    case ITK_FLOAT:
      return "float";
      break;
    case ITK_DOUBLE:
      return "double";
      break;
    default:
      return "unknown";
      break;
  }
}*/

void FileIO::Resize(const unsigned int numDimensions,
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

void FileIO::CalcStrides()
{
  unsigned int i;

  m_Strides[0] = CalcSizeOfAtomicPixelType(m_PixelType);
  m_Strides[1] = m_ComponentsPerPixel * m_Strides[0];
  for (i = 2; i <= m_NumDimensions; i++)
  {
    m_Strides[i] = m_Dimensions[i-2] * m_Strides[i-1];
  }
}

void FileIO::LoadSeveralSlices (const std::string filePattern,
                                const int startSlice,
                                const int endSlice)
{
  /*
   * Not yet implemented, because currently requires code from FLTK
   */
}

// Calculates the image size in PIXELS
unsigned int FileIO::ImageSizeInPixels() const
{
  unsigned int i;
  unsigned int numPixels = 1;

  for (i = 0; i < m_NumDimensions; i++)
  {
    numPixels *= m_Dimensions[i];
  }

  return numPixels;
}

unsigned int FileIO::ImageSizeInComps() const
{
  return ImageSizeInPixels() * m_ComponentsPerPixel;
}

unsigned int FileIO::ImageSizeInBytes () const
{
  return (ImageSizeInComps() * CalcSizeOfAtomicPixelType(m_PixelType));
}

unsigned int FileIO::GetDimensions(unsigned int i) const
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

void* FileIO::GetFileData()
{
  return m_FileData;
}

unsigned int FileIO::GetComponentStride() const
{
  return m_Strides[0];
}

unsigned int FileIO::GetPixelStride () const
{
  return m_Strides[1];
}

unsigned int FileIO::GetRowStride () const
{
  return m_Strides[2];
}

unsigned int FileIO::GetSliceStride () const
{
  return m_Strides[3];
}

} //namespace itk
