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
#ifndef _itkRawImageIO_txx
#define _itkRawImageIO_txx
#include "itkRawImageIO.h"
#include "itkByteSwapper.h"

namespace itk
{

template <class TPixel, unsigned int VImageDimension>
RawImageIO<TPixel,VImageDimension>::RawImageIO() 
: ImageIOBase()
{
  this->SetNumberOfComponents(1);
  this->SetNumberOfDimensions(VImageDimension);
  
  for (unsigned int idx = 0; idx < VImageDimension; ++idx)
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
  m_FileType = Binary;
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
  os << indent << "FileDimensionality: " << m_FileDimensionality << std::endl;
}

template <class TPixel, unsigned int VImageDimension>
unsigned long RawImageIO<TPixel,VImageDimension>::GetHeaderSize()
{
  std::ifstream file;

  if ( m_FileName == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    }

  if ( ! m_ManualHeaderSize )
    {
    if ( m_FileType == ASCII ) return 0; //cannot determine it

    this->ComputeStrides();

    // make sure we figure out a filename to open
    this->OpenFileForReading(file);
    
    // Get the size of the header from the size of the image
    file.seekg(0,std::ios::end);
    
    m_HeaderSize = (unsigned long)((unsigned long)file.tellg() - 
                   (unsigned long)m_Strides[m_FileDimensionality + 1]);
    }

  return m_HeaderSize;
}

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>::OpenFileForReading(std::ifstream& is)
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    }

  // Close file from any previous image
  if ( is.is_open() )
    {
    is.close();
    }
  
  // Open the new file
  itkDebugMacro(<< "Initialize: opening file " << m_FileName);
#ifdef _WIN32 
  is.open(m_FileName.c_str(), std::ios::in | std::ios::binary);
#else
  is.open(m_FileName.c_str(), std::ios::in);
#endif
  if ( is.fail() )
    {
    itkExceptionMacro(<< "Could not open file: " << m_FileName);
    }
}

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>::OpenFileForWriting(std::ofstream& os)
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro(<<"A FileName must be specified.");
    }

  // Close file from any previous image
  if ( os.is_open() )
    {
    os.close();
    }
  
  // Open the new file
  itkDebugMacro(<< "Initialize: opening file " << m_FileName);
#ifdef _WIN32
  os.open(m_FileName.c_str(), std::ios::out | std::ios::binary);
#else
  os.open(m_FileName.c_str(), std::ios::out);
#endif
  if ( os.fail() )
    {
    itkExceptionMacro(<< "Could not open file: " << m_FileName);
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
::Read(void *buffer)
{
  std::ifstream file;

  // Open the file
  this->OpenFileForReading(file);
  this->ComputeStrides();
  
  // Offset into file
  unsigned long streamStart = this->GetHeaderSize();
  file.seekg((long)streamStart, std::ios::beg);
  if ( file.fail() )
    {
    itkExceptionMacro(<<"File seek failed");
    }

  const unsigned long numberOfBytesToBeRead = 
          static_cast< unsigned long>( this->GetImageSizeInBytes() );

  itkDebugMacro(<< "Reading " << numberOfBytesToBeRead << " bytes");
  
  if ( m_FileType == Binary )
    {
    // Read the image (binary) 
    file.read((char *)buffer, numberOfBytesToBeRead );
    const unsigned long numberOfBytesRead = file.gcount();
#ifdef __APPLE_CC__
    // fail() is broken in the Mac. It returns true when reaches eof().
    if ( numberOfBytesRead != numberOfBytesToBeRead )
#else
    if ( ( numberOfBytesRead != numberOfBytesToBeRead )  || file.fail() )
#endif
      {
      itkExceptionMacro(<<"Read failed: Wanted " 
                        << numberOfBytesToBeRead
                        << " bytes, but read " 
                        << numberOfBytesRead << " bytes.");
      }
    }
  else
    {
    this->ReadBufferAsASCII(file, buffer, this->GetComponentType(),
                            this->GetImageSizeInComponents());
    }
  
  itkDebugMacro(<< "Reading Done");

  // Swap bytes if necessary
  if ( m_ByteOrder == LittleEndian )
    {
    ByteSwapperType::SwapRangeFromSystemToLittleEndian(
                    (ComponentType *)buffer, this->GetImageSizeInComponents() );
    }
  else if ( m_ByteOrder == BigEndian )
    {
    ByteSwapperType::SwapRangeFromSystemToBigEndian(
                    (ComponentType *)buffer, this->GetImageSizeInComponents() );
    }
}

template <class TPixel, unsigned int VImageDimension>
bool RawImageIO<TPixel,VImageDimension>
::CanWriteFile(const char* fname)
{
  std::string filename( fname );
  if ( filename == "" )
    {
    return false;
    }

  return true;
}

template <class TPixel, unsigned int VImageDimension>
void RawImageIO<TPixel,VImageDimension>
::Write(const void* buffer)
{
  std::ofstream file;

  // Open the file
  //
  this->OpenFileForWriting(file);

  // Set up for reading
  this->SetPixelType(typeid(PixelType));
  this->ComputeStrides();
  
  // Actually do the writing
  //
  if ( m_FileType == ASCII )
    {
    this->WriteBufferAsASCII(file, buffer, this->GetComponentType(),
                             this->GetImageSizeInComponents());
    }
  else //binary
    {

    const unsigned long numberOfBytes      = this->GetImageSizeInBytes();
    const unsigned long numberOfComponents = this->GetImageSizeInComponents();
    // Swap bytes if necessary
    if ( m_ByteOrder == LittleEndian )
      {
      char * tempBuffer = new char[ numberOfBytes ];
      memcpy( tempBuffer, buffer , numberOfBytes );
      ByteSwapperType::SwapRangeFromSystemToLittleEndian(
                      (ComponentType *)tempBuffer, numberOfComponents );
      file.write( tempBuffer, numberOfBytes );
      delete [] tempBuffer;
      }
    else if ( m_ByteOrder == BigEndian )
      {
      char * tempBuffer = new char[ numberOfBytes ];
      memcpy( tempBuffer, buffer , numberOfBytes );
      ByteSwapperType::SwapRangeFromSystemToBigEndian(
                      (ComponentType *)tempBuffer, numberOfComponents );
      file.write( tempBuffer, numberOfBytes );
      delete [] tempBuffer;
      }
    else
      {
      file.write(static_cast<const char*>(buffer), numberOfBytes );
      }

    }
  file.close();
}



} // namespace itk
#endif
