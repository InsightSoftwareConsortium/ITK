/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIO.cxx
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
  for (i = 2; i <= m_NumberOfDimensions; i++)
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
