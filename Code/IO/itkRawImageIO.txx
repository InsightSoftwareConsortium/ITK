/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIO.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRawImageIO.h"
#include "itkByteSwapper.h"

namespace itk
{

template <class TPixel, unsigned int VImageDimension>
RawImageIOFactory<TPixel,VImageDimension>::RawImageIOFactory()
{
  myProductType::Pointer m_MyProduct = myProductType::New();
  RawImageIO<TPixel,VImageDimension>::FileExtensionsListType& extensionsList =
    m_MyProduct->GetSupportedFileExtensions();

  for (int i = 0; i < extensionsList.size(); i++)
    {
    RegisterOverride(m_MyProduct->GetSupportedFileExtensions()[i].c_str(),
                     "RawImageIO", "Create RawImageIO", true,
                     CreateObjectFunction<RawImageIO<TPixel,VImageDimension> >::New());
    }
}

template <class TPixel, unsigned int VImageDimension>
const char* RawImageIOFactory<TPixel,VImageDimension>::GetITKSourceVersion()
{
  return ITK_SOURCE_VERSION;
}

template <class TPixel, unsigned int VImageDimension>
const char* RawImageIOFactory<TPixel,VImageDimension>::GetDescription() const
{
  return "RawImageIOFactory - Object factory with registry";
}

template <class TPixel, unsigned int VImageDimension>
RawImageIO<TPixel,VImageDimension>::RawImageIO() 
: ImageIOBase()
{
  m_FilePrefix = "";
  m_FilePattern = "%s.%d";
  
  this->SetNumberOfComponents(1);
  this->SetNumberOfDimensions(VImageDimension);
  
  for (int idx = 0; idx < VImageDimension; ++idx)
    {
    m_Spacing.insert(m_Spacing.begin()+idx,1.0);
    m_Origin.insert(m_Origin.begin()+idx,0.0);
    }
  
  m_HeaderSize = 0;
  m_ManualHeaderSize = false;
  
  // Left over from short reader
  m_ImageMask = 0xffff;
  m_ByteOrder = ImageIOBase::BigEndian;
  m_FileDimensionality = 2;
}

template <class TPixel, unsigned int VImageDimension>
RawImageIO<TPixel,VImageDimension>::~RawImageIO()
{
}

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ImageMask: " << m_ImageMask << std::endl;
}

template <class TPixel, unsigned int VImageDimension>
unsigned long RawImageIO<TPixel,VImageDimension>::GetHeaderSize()
{
  if ( m_FileName == "" && m_FilePattern == "" )
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return 0;
    }

  if ( ! m_ManualHeaderSize )
    {
//    m_PixelType = typeid(PixelType);
    this->ComputeStrides();

    // make sure we figure out a filename to open
    this->OpenFile();
    
    // Get the size of the header from the size of the image
    m_File.seekg(0,std::ios::end);
    
    return (unsigned long)((unsigned long)m_File.tellg() - 
      (unsigned long)m_Strides[VImageDimension]);
    }

  return m_HeaderSize;
}

/**
template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>::ComputeInternalFileName(unsigned long slice)
{
  char buf[2048];

  if ( m_FileName == "" && m_FilePattern == "" )
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return;
    }

  // make sure we figure out a filename to open
  if ( m_FileName != "" )
    {
    sprintf(buf, "%s", m_FileName.c_str());
    m_InternalFileName = buf;
    }
  else 
    {
    if ( m_FilePrefix != "" )
      {
      sprintf (buf, m_FilePattern.c_str(), m_FilePrefix.c_str(), slice);
      }
    else
      {
      sprintf (buf, m_FilePattern.c_str(), slice);
      }
    m_InternalFileName = buf;
    }
}
*/

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>::OpenFile()
{
  if ( m_FileName == "" && m_FilePattern == "")
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return;
    }

  // Close file from any previous image
  if ( m_File.is_open() )
    {
    m_File.close();
    }
  
  // Open the new file
  itkDebugMacro(<< "Initialize: opening file " << m_FileName);
#ifdef _WIN32
  m_File.open(m_FileName.c_str(), std::ios::in | std::ios::binary);
#else
  m_File.open(m_FileName.c_str(), std::ios::in);
#endif
  if ( m_File.fail() )
    {
    itkErrorMacro(<< "Could not open file: " << m_FileName);
    return;
    }
}

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>
::SetHeaderSize(unsigned long size)
{
  if ( size != m_HeaderSize)
    {
    m_HeaderSize = size;
    this->Modified();
    }
  m_ManualHeaderSize = true;
}


template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>
::Read()
{
  // Open the file
  this->OpenFile();
  
  // Set the dimensions and related
  this->SetNumberOfDimensions(VImageDimension);
  this->ComputeStrides();
  
  // Offset into file
  m_RequestedRegionData = (void*)new char[m_Strides[3]];
  unsigned long streamStart = this->GetHeaderSize();
  m_File.seekg((long)streamStart, std::ios::beg);
  if ( m_File.fail() )
    {
    itkErrorMacro(<<"File seek failed");
    return;
    }

  // Read the image
  m_File.read((char *)m_RequestedRegionData, m_Strides[3]);
  
  // Swap bytes if necessary
  if ( m_ByteOrder == LittleEndian &&
    ByteSwapper<PixelType>::IsBigEndian() )
    {
    ByteSwapper<PixelType>::SwapRangeBE((PixelType *)m_RequestedRegionData, m_Strides[3]);
    }
  else if ( m_ByteOrder == BigEndian &&
    ByteSwapper<PixelType>::IsLittleEndian() )
    {
    ByteSwapper<PixelType>::SwapRangeLE((PixelType *)m_RequestedRegionData, m_Strides[3]);
    }
}



} // namespace itk
