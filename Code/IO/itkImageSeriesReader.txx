/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageSeriesReader_txx
#define _itkImageSeriesReader_txx
#include "itkImageSeriesReader.h"

#include "itkObjectFactory.h"
#include "itkImageIOFactory.h"
#include "itkConvertPixelBuffer.h"
#include "itkImageRegion.h"


namespace itk
{

template <class TOutputImage, class ConvertPixelTraits>
ImageSeriesReader<TOutputImage, ConvertPixelTraits>
::ImageSeriesReader() :
  m_ImageIO(0), m_UserSpecifiedImageIO(false), 
  m_FileIterator(0)
{
}

template <class TOutputImage, class ConvertPixelTraits>
ImageSeriesReader<TOutputImage, ConvertPixelTraits>::~ImageSeriesReader()
{
}

template <class TOutputImage, class ConvertPixelTraits>
void ImageSeriesReader<TOutputImage, ConvertPixelTraits>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_ImageIO)
    {       
    os << indent << "m_ImageIO: " << m_ImageIO << "\n";
    }
  else
    {
    os << indent << "m_ImageIO: (none)" << "\n";
    }

  os << indent << "File Iterator: ";
  if ( m_FileIterator == 0 )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_FileIterator << "\n";
    }
}

template <class TOutputImage, class ConvertPixelTraits>
void ImageSeriesReader<TOutputImage, ConvertPixelTraits>
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();
  itkDebugMacro(<<"Reading series for GenerateOutputInformation()");
  
  // Okay, set up the FileIterator and ImageIO
  //
  if ( m_FileIterator.IsNull() )
    {
    if ( m_ImageIO.IsNull() )
      {
      itkExceptionMacro(<< "Either a file iterator or ImageIO must be set");
      }
    else
      {
      m_FileIterator = m_ImageIO->NewFileIterator();
      }
    }

  else //have a FileIterator, may have to create ImageIO
    {
    const char *format = (m_FileIterator->Begin()).c_str();
    if ( m_ImageIO.IsNull() ) //try creating via factory
      {
      itkDebugMacro(<<"Attempting factory creation of ImageIO" << format);
      m_ImageIO = ImageIOFactory::CreateImageIO( format,
                                                 ImageIOFactory::ReadMode );
      }
    else
      {
      if( !m_ImageIO->CanReadFile( format ) )
        {
        itkDebugMacro(<<"ImageIO exists but doesn't know how to read: "
                      << format );
        itkDebugMacro(<<"Attempting factory creation of ImageIO:" << format);
        m_ImageIO = ImageIOFactory::CreateImageIO( format,
                                                   ImageIOFactory::ReadMode );
        }
      }
    }

  if ( m_ImageIO.IsNull() || m_FileIterator.IsNull() )
    {
    itkExceptionMacro(<<"Cannot determine what type of files to create.");
    }

  // Got to allocate space for the image. Determine the characteristics of
  // the image.
  //
//  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->ReadImageInformation();

  SizeType dimSize;
  double spacing[ TOutputImage::ImageDimension ];
  double origin[ TOutputImage::ImageDimension ];

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( i < m_ImageIO->GetNumberOfDimensions() )
      {
      dimSize[i] = m_ImageIO->GetDimensions(i);
      spacing[i] = m_ImageIO->GetSpacing(i);
      origin[i]  = m_ImageIO->GetOrigin(i);
      }
    else
      {
      // Number of dimensions in the output is more than number of dimensions
      // in the ImageIO object (the file).  Use default values for the size,
      // spacing, and origin for the final (degenerate) dimensions.
      dimSize[i] = 1;  
      spacing[i] = 1.0;
      origin[i] = 0.0;
      }
    }

  output->SetSpacing( spacing );   // Set the image spacing
  output->SetOrigin( origin );     // Set the image origin

  typedef typename TOutputImage::IndexType   IndexType;

  IndexType start;
  start.Fill(0);

  ImageRegionType region;
  region.SetSize(dimSize);
  region.SetIndex(start);
 
  output->SetLargestPossibleRegion(region);
}


template <class TOutputImage, class ConvertPixelTraits>
void
ImageSeriesReader<TOutputImage, ConvertPixelTraits>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  typename TOutputImage::Pointer out = dynamic_cast<TOutputImage*>(output);

  if (out)
    {
    out->SetRequestedRegion( out->GetLargestPossibleRegion() );
    }
  else
    {
    throw ImageSeriesReaderException(__FILE__, __LINE__, "Invalid output object type");
    }
}


template <class TOutputImage, class ConvertPixelTraits>
void ImageSeriesReader<TOutputImage, ConvertPixelTraits>
::GenerateData()
{

  typename TOutputImage::Pointer output = this->GetOutput();

  // allocate the output buffer
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

}




template <class TOutputImage, class ConvertPixelTraits>
void 
ImageSeriesReader<TOutputImage, ConvertPixelTraits>
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
      ImageSeriesReaderException e(__FILE__, __LINE__);
      OStringStream msg;
      msg <<"Couldn't convert pixel type: "
          << std::endl << "    " << m_ImageIO->GetPixelType().name()
          << std::endl << "to one of: "
          << std::endl << "    " << typeid(unsigned char).name()
          << std::endl << "    " << typeid(char).name()
          << std::endl << "    " << typeid(unsigned short).name()
          << std::endl << "    " << typeid(short).name()
          << std::endl << "    " << typeid(unsigned int).name()
          << std::endl << "    " << typeid(int).name()
          << std::endl << "    " << typeid(unsigned long).name()
          << std::endl << "    " << typeid(long).name()
          << std::endl << "    " << typeid(float).name()
          << std::endl << "    " << typeid(double).name()
          << std::endl;
      e.SetDescription(msg.str().c_str());
      throw e;
      return;
      }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}


} //namespace ITK

#endif
