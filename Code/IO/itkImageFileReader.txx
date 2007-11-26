/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageFileReader_txx
#define __itkImageFileReader_txx
#include "itkImageFileReader.h"

#include "itkObjectFactory.h"
#include "itkImageIOFactory.h"
#include "itkConvertPixelBuffer.h"
#include "itkImageRegion.h"
#include "itkPixelTraits.h"
#include "itkVectorImage.h"

#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{

template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>
::ImageFileReader()
{
  m_ImageIO = 0;
  m_FileName = "";
  m_UserSpecifiedImageIO = false;
  m_UseStreaming = false;
}

template <class TOutputImage, class ConvertPixelTraits>
ImageFileReader<TOutputImage, ConvertPixelTraits>
::~ImageFileReader()
{
}

template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_ImageIO)
    {
    os << indent << "ImageIO: \n";
    m_ImageIO->Print(os, indent.GetNextIndent());
    }
  else
    {
    os << indent << "ImageIO: (null)" << "\n";
    }

  os << indent << "UserSpecifiedImageIO flag: " << m_UserSpecifiedImageIO << "\n";
  os << indent << "m_FileName: " << m_FileName << "\n";
  os << indent << "m_UseStreaming: " << m_UseStreaming << "\n";
}


template <class TOutputImage, class ConvertPixelTraits>
void 
ImageFileReader<TOutputImage, ConvertPixelTraits>
::SetImageIO( ImageIOBase * imageIO)
{
  itkDebugMacro("setting ImageIO to " << imageIO ); 
  if (this->m_ImageIO != imageIO ) 
    {
    this->m_ImageIO = imageIO;
    this->Modified(); 
    } 
  m_UserSpecifiedImageIO = true;
}


template <class TOutputImage, class ConvertPixelTraits>
void 
ImageFileReader<TOutputImage, ConvertPixelTraits>
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro(<<"Reading file for GenerateOutputInformation()" << m_FileName);
  
  // Check to see if we can read the file given the name or prefix
  //
  if ( m_FileName == "" )
    {
    throw ImageFileReaderException(__FILE__, __LINE__, "FileName must be specified", ITK_LOCATION);
    }

  // Test if the file exists and if it can be open.
  // An exception will be thrown otherwise.
  //
  try
    {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
    }
  catch(itk::ExceptionObject &err)
    {
    m_ExceptionMessage = err.GetDescription();
    }

  if ( m_UserSpecifiedImageIO == false ) //try creating via factory
    {
    m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), ImageIOFactory::ReadMode );
    }
  
  if ( m_ImageIO.IsNull() )
    {
    OStringStream msg;
    msg << " Could not create IO object for file "
        << m_FileName.c_str() << std::endl;
    if (m_ExceptionMessage.size())
      {
      msg << m_ExceptionMessage;
      }
    else
      {
      msg << "  Tried to create one of the following:" << std::endl;
      std::list<LightObject::Pointer> allobjects = 
        ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
      for(std::list<LightObject::Pointer>::iterator i = allobjects.begin();
          i != allobjects.end(); ++i)
        {
        ImageIOBase* io = dynamic_cast<ImageIOBase*>(i->GetPointer());
        msg << "    " << io->GetNameOfClass() << std::endl; 
        }
      msg << "  You probably failed to set a file suffix, or" << std::endl;
      msg << "    set the suffix to an unsupported type." << std::endl;
      }
    ImageFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
    }
  
  // Got to allocate space for the image. Determine the characteristics of
  // the image.
  //
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->ReadImageInformation();

  SizeType dimSize;
  double spacing[ TOutputImage::ImageDimension ];
  double origin[ TOutputImage::ImageDimension ];
  typename TOutputImage::DirectionType direction;
  std::vector<double> axis;

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( i < m_ImageIO->GetNumberOfDimensions() )
      {
      dimSize[i] = m_ImageIO->GetDimensions(i);
      spacing[i] = m_ImageIO->GetSpacing(i);
      origin[i]  = m_ImageIO->GetOrigin(i);

      // Please note: direction cosines are stored as columns of the
      // direction matrix
      axis = m_ImageIO->GetDirection(i);
      for (unsigned j=0; j<TOutputImage::ImageDimension; j++)
        {
        if (j < m_ImageIO->GetNumberOfDimensions())
          {
          direction[j][i] = axis[j];
          }
        else
          {
          direction[j][i] = 0.0;
          }
        }
      }
    else
      {
      // Number of dimensions in the output is more than number of dimensions
      // in the ImageIO object (the file).  Use default values for the size,
      // spacing, origin and direction for the final (degenerate) dimensions.
      dimSize[i] = 1;  
      spacing[i] = 1.0;
      origin[i] = 0.0;
      for (unsigned j = 0; j < TOutputImage::ImageDimension; j++)
        {
        if (i == j)
          {
          direction[j][i] = 1.0;
          }
        else
          {
          direction[j][i] = 0.0;
          }
        }
      }
    }

  output->SetSpacing( spacing );     // Set the image spacing
  output->SetOrigin( origin );       // Set the image origin
  output->SetDirection( direction ); // Set the image direction cosines

  //Copy MetaDataDictionary from instantiated reader to output image.
  output->SetMetaDataDictionary(m_ImageIO->GetMetaDataDictionary());
  this->SetMetaDataDictionary(m_ImageIO->GetMetaDataDictionary());

  IndexType start;
  start.Fill(0);

  ImageRegionType region;
  region.SetSize(dimSize);
  region.SetIndex(start);
 
  // If a VectorImage, this requires us to set the 
  // VectorLength before allocate
  if( strcmp( output->GetNameOfClass(), "VectorImage" ) == 0 ) 
    {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength( output, m_ImageIO->GetNumberOfComponents() );
    }
  
  output->SetLargestPossibleRegion(region);
}

template <class TOutputImage, class ConvertPixelTraits>
void
ImageFileReader<TOutputImage, ConvertPixelTraits>
::TestFileExistanceAndReadability()
{
  // Test if the file exists.
  if( ! itksys::SystemTools::FileExists( m_FileName.c_str() ) )
    {
    ImageFileReaderException e(__FILE__, __LINE__);
    OStringStream msg;
    msg <<"The file doesn't exist. "
        << std::endl << "Filename = " << m_FileName
        << std::endl;
    e.SetDescription(msg.str().c_str());
    throw e;
    return;
    }

  // Test if the file can be open for reading access.
  std::ifstream readTester;
  readTester.open( m_FileName.c_str() );
  if( readTester.fail() )
    {
    readTester.close();
    OStringStream msg;
    msg <<"The file couldn't be opened for reading. "
        << std::endl << "Filename: " << m_FileName
        << std::endl;
    ImageFileReaderException e(__FILE__, __LINE__,msg.str().c_str(),ITK_LOCATION);
    throw e;
    return;
    
    }
  readTester.close();
}

template <class TOutputImage, class ConvertPixelTraits>
void
ImageFileReader<TOutputImage, ConvertPixelTraits>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  itkDebugMacro (<< "Starting EnlargeOutputRequestedRegion() ");
  typename TOutputImage::Pointer out = dynamic_cast<TOutputImage*>(output);

  // Delegate to the ImageIO the computation of how much the 
  // requested region must be enlarged.

  //
  // The following code converts the ImageRegion (templated over dimension)
  // into an ImageIORegion (not templated over dimension).
  //
  ImageRegionType imageRequestedRegion = out->GetRequestedRegion();

  ImageIORegion ioRequestedRegion( TOutputImage::ImageDimension );

  typedef ImageIORegionAdaptor< TOutputImage::ImageDimension >  ImageIOAdaptor;
  
  ImageIOAdaptor::Convert( imageRequestedRegion, ioRequestedRegion );

  // Tell the IO if we should use streaming while reading
  m_ImageIO->SetUseStreamedReading(m_UseStreaming);

  ImageIORegion ioStreamableRegion  = 
    m_ImageIO->GenerateStreamableReadRegionFromRequestedRegion( ioRequestedRegion );


  ImageIOAdaptor::Convert( ioStreamableRegion, this->m_StreamableRegion );

  //
  // Check whether the imageRequestedRegion is fully contained inside the
  // streamable region or not.
  if( !this->m_StreamableRegion.IsInside( imageRequestedRegion ) )
    {
    itkExceptionMacro(
      << "ImageIO returns IO region that does not fully contain the requested region" << std::ends
      << "Requested region: " << imageRequestedRegion << std::ends
      << "StreamableRegion region: " << this->m_StreamableRegion);
    }
    
  itkDebugMacro (<< "StreamableRegion set to =" << this->m_StreamableRegion );
}


template <class TOutputImage, class ConvertPixelTraits>
void ImageFileReader<TOutputImage, ConvertPixelTraits>
::GenerateData()
{

  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro ( << "ImageFileReader::GenerateData() \n" 
     << "Allocating the buffer with the StreamableRegion \n" 
     << this->m_StreamableRegion << "\n");

  output->SetBufferedRegion( this->m_StreamableRegion );
  output->Allocate();

  // Test if the file exist and if it can be open.
  // and exception will be thrown otherwise.
  try
    {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
    }
  catch(itk::ExceptionObject &err)
    {
    m_ExceptionMessage = err.GetDescription();
    }

  // Tell the ImageIO to read the file
  //
  OutputImagePixelType *buffer = output->GetPixelContainer()->GetBufferPointer();

  m_ImageIO->SetFileName(m_FileName.c_str());

  ImageIORegion ioRegion(TOutputImage::ImageDimension);

  typedef ImageIORegionAdaptor< TOutputImage::ImageDimension >  ImageIOAdaptor;
  
  // Convert the m_StreamableRegion from ImageRegion type to ImageIORegion type
  ImageIOAdaptor::Convert( this->m_StreamableRegion, ioRegion );

  itkDebugMacro (<< "ioRegion: " << ioRegion);
 
  m_ImageIO->SetIORegion( ioRegion );

  if ( m_ImageIO->GetComponentTypeInfo()
       == typeid(ITK_TYPENAME ConvertPixelTraits::ComponentType)
       && (m_ImageIO->GetNumberOfComponents()
           == ConvertPixelTraits::GetNumberOfComponents()))
    {
    itkDebugMacro(<< "No buffer conversion required.");
    // allocate a buffer and have the ImageIO read directly into it
    m_ImageIO->Read(buffer);
    return;
    }
  else // a type conversion is necessary
    {
    itkDebugMacro(<< "Buffer conversion required.");
    // note: char is used here because the buffer is read in bytes
    // regardles of the actual type of the pixels.
    ImageRegionType region = output->GetBufferedRegion();
    std::vector<char> loadBuffer(m_ImageIO->GetImageSizeInBytes());

    m_ImageIO->Read(static_cast<void *>(&loadBuffer[0]));
    
    itkDebugMacro(<< "Buffer conversion required from: "
                  << m_ImageIO->GetComponentTypeInfo().name()
                  << " to: "
                  << typeid(ITK_TYPENAME ConvertPixelTraits::ComponentType).name());

    this->DoConvertBuffer(static_cast<void *>(&loadBuffer[0]), region.GetNumberOfPixels());
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


  // TODO:
  // Pass down the PixelType (RGB, VECTOR, etc.) so that any vector to
  // scalar conversion be type specific. i.e. RGB to scalar would use
  // a formula to convert to luminance, VECTOR to scalar would use
  // vector magnitude.
  
  
  // Create a macro as this code is a bit lengthy and repetitive
  // if the ImageIO pixel type is typeid(type) then use the ConvertPixelBuffer
  // class to convert the data block to TOutputImage's pixel type
  // see DefaultConvertPixelTraits and ConvertPixelBuffer

  // The first else if block applies only to images of type itk::VectorImage  
  // VectorImage needs to copy out the buffer differently.. The buffer is of
  // type InternalPixelType, but each pixel is really 'k' consecutive pixels.

#define ITK_CONVERT_BUFFER_IF_BLOCK(type)               \
 else if( m_ImageIO->GetComponentTypeInfo() == typeid(type) )   \
   {                                                   \
   if( strcmp( this->GetOutput()->GetNameOfClass(), "VectorImage" ) == 0 ) \
     { \
     ConvertPixelBuffer<                                 \
      type,                                             \
      OutputImagePixelType,                             \
      ConvertPixelTraits                                \
      >                                                 \
      ::ConvertVectorImage(                             \
        static_cast<type*>(inputData),                  \
        m_ImageIO->GetNumberOfComponents(),             \
        outputData,                                     \
        static_cast<int>(numberOfPixels));              \
     } \
   else \
     { \
     ConvertPixelBuffer<                                 \
      type,                                             \
      OutputImagePixelType,                             \
      ConvertPixelTraits                                \
      >                                                 \
      ::Convert(                                        \
        static_cast<type*>(inputData),                  \
        m_ImageIO->GetNumberOfComponents(),             \
        outputData,                                     \
        static_cast<int>(numberOfPixels));              \
      } \
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
    OStringStream msg;
    msg <<"Couldn't convert component type: "
        << std::endl << "    "
        << m_ImageIO->GetComponentTypeAsString(m_ImageIO->GetComponentType())
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
    e.SetLocation(ITK_LOCATION);
    throw e;
    return;
    }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}


} //namespace ITK

#endif
