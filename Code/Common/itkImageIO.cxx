/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
  m_FileName = "";
  m_PixelType = ITK_DOUBLE;
  m_NumberOfComponents = 1;
  m_NumberOfDimensions = 0;
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

void ImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Filename: " << m_FileName << std::endl;
  os << indent << "FilePrefix: " << m_FilePrefix << std::endl;
  os << indent << "FilePattern: " << m_FilePattern << std::endl;
  os << indent << "# Components/Pixel: " << m_NumberOfComponents;
  os << ", PixelType: " << AtomicPixelTypeToString(m_PixelType) << std::endl;
  os << indent << "Dimensions: ( ";
  for (unsigned int i=0; i < m_NumberOfDimensions; i++)
    {
    os << m_Dimensions[i] << " ";
    }
  os << ")" << std::endl;
}

void ImageIO::Resize(const unsigned int numDimensions,
                    const unsigned int* dimensions)
{
  m_NumberOfDimensions = numDimensions;
  if (dimensions != NULL)
    {
    for (unsigned int i=0; i < m_NumberOfDimensions; i++)
      {
      m_Dimensions[i] = dimensions[i];
      }
    ComputeStrides();
    delete [] (char*) m_FileData;
    m_FileData = new unsigned char[ImageSizeInBytes()];
  }
}

void ImageIO::ComputeStrides()
{
  unsigned int i;

  m_Strides[0] = ComputeSizeOfAtomicPixelType(m_PixelType);
  m_Strides[1] = m_NumberOfComponents * m_Strides[0];
  for (i = 2; i <= (m_NumberOfDimensions+1); i++)
    {
    m_Strides[i] = m_Dimensions[i-2] * m_Strides[i-1];
    }
}


// Calculates the image size in PIXELS
unsigned int ImageIO::ImageSizeInPixels() const
{
  unsigned int i;
  unsigned int numPixels = 1;

  for (i = 0; i < m_NumberOfDimensions; i++)
    {
    numPixels *= m_Dimensions[i];
    }

  return numPixels;
}

unsigned int ImageIO::ImageSizeInComponents() const
{
  return ImageSizeInPixels() * m_NumberOfComponents;
}

unsigned int ImageIO::ImageSizeInBytes () const
{
  return (ImageSizeInComponents() * ComputeSizeOfAtomicPixelType(m_PixelType));
}

unsigned int ImageIO::GetDimensions(unsigned int i) const
{
  if (i > m_NumberOfDimensions)
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
