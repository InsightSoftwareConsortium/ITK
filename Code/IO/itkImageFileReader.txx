/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFileReader_txx
#define _itkImageFileReader_txx
#include "itkImageFileReader.h"

#include "itkObjectFactory.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImageIOFactory.h"
#include "itkConvertPixelBuffer.h"
#include "itkImageRegion.h"


namespace itk
{

// BILL: You were missing the matching template arguments on the method
// implementations in itkImageFileReader.txx.
template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>
::ImageFileReader()
{
  m_ImageIO = 0;
  m_FileName = "";
  m_FilePrefix = "";
  m_FilePattern = "";
}

template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>::~ImageFileReader()
{
}
template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_ImageIO)
    {       
    os << indent << "m_ImageIO: " << m_ImageIO << "\n";
    }
  else
    {
    os << indent << "m_ImageIO: (null)" << "\n";
    }
  os << indent << "m_UserSpecified m_ImageIO flag: " << m_UserSpecified << "\n";
  os << indent << "m_FileName: " << m_FileName << "\n";
  os << indent << "m_FilePrefix: " << m_FilePrefix << "\n";
  os << indent << "m_FilePattern: " << m_FilePattern << "\n";
  
}


template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>
::GenerateOutputInformation(void)
{

  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro(<<"Reading file for GenerateOutputInformation()" << m_FileName);
  
  // Check to see if we can read the file given the name or prefix
  //
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    throw ImageFileReaderException(__FILE__, __LINE__, "One of FileName or FilePrefix must be non-empty");
    }

  if ( m_ImageIO == 0 ) //try creating via factory
    {
    m_UserSpecified = false;
    m_ImageIO = ImageIOFactory::CreateImageIO(m_FileName.c_str());
    }
  else
    {
    m_UserSpecified = true;
    }
  
  if ( m_ImageIO == 0 )
    {
    ImageFileReaderException e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg << " Could not create IO object for file "
        << m_FileName.c_str()
        << std::ends;
    e.SetDescription(msg.str());
    throw e;
    return;
    }

  // Got to allocate space for the image. Determine the characteristics of
  // the image.
  //
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->ReadImageInformation();

  if( m_ImageIO->GetNumberOfDimensions() < TOutputImage::ImageDimension )
    {
    ImageFileReaderException e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg << "Number of dimensions in file ("
        << m_ImageIO->GetNumberOfDimensions()
        << ") does not match number of dimensions in output ("
        << TOutputImage::ImageDimension
        << ")" << std::ends;
    e.SetDescription(msg.str());
    throw e;
    }
  
  SizeType dimSize;
  double spacing[ TOutputImage::ImageDimension ];
  double origin[ TOutputImage::ImageDimension ];

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_ImageIO->GetDimensions(i);
    spacing[i] = m_ImageIO->GetSpacing(i);
    origin[i]  = m_ImageIO->GetOrigin(i);
    }

  output->SetSpacing( spacing );   // Set the image spacing
  output->SetOrigin( origin );     // Set the image origin

  typedef typename TOutputImage::IndexType   IndexType;

  IndexType start;
  start = IndexType::ZeroIndex;

  ImageRegionType region;
  region.SetSize(dimSize);
  region.SetIndex(start);
 
  output->SetLargestPossibleRegion(region);
  output->SetRequestedRegion(region);
  output->SetBufferedRegion(region);
  output->Allocate();

}




template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>::GenerateData()
{

  typename TOutputImage::Pointer output = this->GetOutput();
  // Tell the ImageIO to read the file
  //
  OutputImagePixelType *buffer = 
    output->GetPixelContainer()->GetBufferPointer();
  m_ImageIO->SetFileName(m_FileName.c_str());

  ImageIORegion ioRegion(TOutputImage::ImageDimension);
  
  ImageIORegion::SizeType ioSize = ioRegion.GetSize();
  ImageIORegion::IndexType ioStart = ioRegion.GetIndex();

  SizeType dimSize;
  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    dimSize[i] = m_ImageIO->GetDimensions(i);
    }

  for(unsigned int i = 0; i < dimSize.GetSizeDimension(); ++i)
    {
    ioSize[i] = dimSize[i];
    }

  typedef typename TOutputImage::IndexType   IndexType;
  IndexType start;
  start = IndexType::ZeroIndex;
  for(unsigned int i = 0; i < start.GetIndexDimension(); ++i)
    {
    ioStart[i] = start[i];
    }

  ioRegion.SetSize(ioSize);
  ioRegion.SetIndex(ioStart);

  itkDebugMacro (<< "ioRegion: " << ioRegion);
 
  m_ImageIO->SetIORegion(ioRegion);

  if ( m_ImageIO->GetPixelType() == typeid(TOutputImage::PixelType) &&
       (m_ImageIO->GetNumberOfComponents() == 
        ConvertPixelTraits::GetNumberOfComponents()))
    {
    itkDebugMacro(<< "No buffer conversion required.");
    // allocate a buffer and have the ImageIO read directly into it
    m_ImageIO->Read(buffer);
    return;
    }
  else // a type conversion is necessary
    {
    // note: char is used here because the buffer is read in bytes
    // regardles of the actual type of the pixels.
    ImageRegionType region = output->GetBufferedRegion();
    char * loadBuffer = 
      new char[m_ImageIO->GetImageSizeInBytes()];
    m_ImageIO->Read(loadBuffer);
    itkDebugMacro(<< "Buffer conversion required from: "
                 << m_ImageIO->GetPixelType().name()
                 << " to: " << typeid(TOutputImage::PixelType).name());
    this->DoConvertBuffer(loadBuffer, region.GetNumberOfPixels());
    delete [] loadBuffer;
    }
}




template <class TOutputImage, class ConvertPixelTraits>
void 
ImageFileReader<TOutputImage, ConvertPixelTraits>
::DoConvertBuffer(void* inputData,
                  unsigned long numberOfPixels)
{
  // get the pointer to the destination buffer
  OutputImagePixelType *outputData =
    this->GetOutput()->GetPixelContainer()->GetBufferPointer();

// Create a macro as this code is a bit lengthy and repetitive
// if the ImageIO pixel type is typeid(type) then use the ConvertPixelBuffer
// class to convert the data block to TOutputImage's pixel type
// see DefaultConvertPixelTraits and ConvertPixelBuffer
#define ITK_CONVERT_BUFFER_IF_BLOCK(type)               \
 else if( m_ImageIO->GetPixelType() == typeid(type) )   \
    {                                                   \
    ConvertPixelBuffer<                                 \
      type,                                             \
      OutputImagePixelType,                             \
      ConvertPixelTraits                                \
      >                                                 \
      ::Convert(                                        \
        static_cast<type*>(inputData),                  \
        m_ImageIO->GetNumberOfComponents(),             \
        outputData,                                     \
        numberOfPixels);                                \
    }
  if(0)
    {
    }
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned char)
  ITK_CONVERT_BUFFER_IF_BLOCK(char)
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned short)
  ITK_CONVERT_BUFFER_IF_BLOCK( short)
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned int)
  ITK_CONVERT_BUFFER_IF_BLOCK( int)
  ITK_CONVERT_BUFFER_IF_BLOCK(unsigned long)
  ITK_CONVERT_BUFFER_IF_BLOCK( long)
  ITK_CONVERT_BUFFER_IF_BLOCK(float)
  ITK_CONVERT_BUFFER_IF_BLOCK( double)
  else
    {
    ImageFileReaderException e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg <<"Couldn't convert pixel type: "
        << std::ends << "    " << m_ImageIO->GetPixelType().name()
        << std::ends << "to one of: "
        << std::ends << "    " << typeid(unsigned char).name()
        << std::ends << "    " << typeid(char).name()
        << std::ends << "    " << typeid(unsigned short).name()
        << std::ends << "    " << typeid(short).name()
        << std::ends << "    " << typeid(unsigned int).name()
        << std::ends << "    " << typeid(int).name()
        << std::ends << "    " << typeid(unsigned long).name()
        << std::ends << "    " << typeid(long).name()
        << std::ends << "    " << typeid(float).name()
        << std::ends << "    " << typeid(double).name()
        << std::ends;
    e.SetDescription(msg.str());
    throw e;
    return;
    }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}


} //namespace ITK

#endif
