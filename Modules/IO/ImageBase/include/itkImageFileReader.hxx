/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageFileReader_hxx
#define itkImageFileReader_hxx
#include "itkImageFileReader.h"

#include "itkObjectFactory.h"
#include "itkImageIOFactory.h"
#include "itkConvertPixelBuffer.h"
#include "itkPixelTraits.h"
#include "itkVectorImage.h"

#include "itksys/SystemTools.hxx"
#include <fstream>

namespace itk
{
template< typename TOutputImage, typename ConvertPixelTraits >
ImageFileReader< TOutputImage, ConvertPixelTraits >
::ImageFileReader()
{
  m_ImageIO = ITK_NULLPTR;
  this->SetFileName("");
  m_UserSpecifiedImageIO = false;
  m_UseStreaming = true;
}

template< typename TOutputImage, typename ConvertPixelTraits >
ImageFileReader< TOutputImage, ConvertPixelTraits >
::~ImageFileReader()
{}

template< typename TOutputImage, typename ConvertPixelTraits >
void ImageFileReader< TOutputImage, ConvertPixelTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro( ImageIO );

  os << indent << "UserSpecifiedImageIO flag: " << m_UserSpecifiedImageIO << "\n";
  os << indent << "m_UseStreaming: " << m_UseStreaming << "\n";
}

template< typename TOutputImage, typename ConvertPixelTraits >
void
ImageFileReader< TOutputImage, ConvertPixelTraits >
::SetImageIO(ImageIOBase *imageIO)
{
  itkDebugMacro("setting ImageIO to " << imageIO);
  if ( this->m_ImageIO != imageIO )
    {
    this->m_ImageIO = imageIO;
    this->Modified();
    }
  m_UserSpecifiedImageIO = true;
}

template< typename TOutputImage, typename ConvertPixelTraits >
void
ImageFileReader< TOutputImage, ConvertPixelTraits >
::GenerateOutputInformation(void)
{
  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro(<< "Reading file for GenerateOutputInformation()" << this->GetFileName());

  // Check to see if we can read the file given the name or prefix
  //
  if ( this->GetFileName() == "" )
    {
    throw ImageFileReaderException(__FILE__, __LINE__, "FileName must be specified", ITK_LOCATION);
    }

  // Test if the file exists and if it can be opened.
  // An exception will be thrown otherwise.
  // We catch the exception because some ImageIO's may not actually
  // open a file. Still reports file error if no ImageIO is loaded.

  try
    {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
    }
  catch ( itk::ExceptionObject & err )
    {
    m_ExceptionMessage = err.GetDescription();
    }

#if !defined(SPECIFIC_IMAGEIO_MODULE_TEST)
  if ( m_UserSpecifiedImageIO == false ) //try creating via factory
    {
    m_ImageIO = ImageIOFactory::CreateImageIO(this->GetFileName().c_str(), ImageIOFactory::ReadMode);
    }
#endif

  if ( m_ImageIO.IsNull() )
    {
    std::ostringstream msg;
    msg << " Could not create IO object for reading file "
        << this->GetFileName().c_str() << std::endl;
    if ( m_ExceptionMessage.size() )
      {
      msg << m_ExceptionMessage;
      }
    else
      {
      std::list< LightObject::Pointer > allobjects =
        ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
      if (allobjects.size() > 0)
        {
        msg << "  Tried to create one of the following:" << std::endl;
        for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
              i != allobjects.end(); ++i )
          {
          ImageIOBase *io = dynamic_cast< ImageIOBase * >( i->GetPointer() );
          msg << "    " << io->GetNameOfClass() << std::endl;
          }
        msg << "  You probably failed to set a file suffix, or" << std::endl;
        msg << "    set the suffix to an unsupported type." << std::endl;
        }
      else
        {
        msg << "  There are no registered IO factories." << std::endl;
        msg << "  Please visit https://www.itk.org/Wiki/ITK/FAQ#NoFactoryException to diagnose the problem." << std::endl;
        }
      }
    ImageFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
    }

  // Got to allocate space for the image. Determine the characteristics of
  // the image.
  //
  m_ImageIO->SetFileName( this->GetFileName().c_str() );
  m_ImageIO->ReadImageInformation();

  SizeType dimSize;
  double   spacing[TOutputImage::ImageDimension];
  double   origin[TOutputImage::ImageDimension];
  typename TOutputImage::DirectionType direction;

  std::vector< std::vector< double > > directionIO;

  const unsigned int numberOfDimensionsIO = m_ImageIO->GetNumberOfDimensions();

  if ( numberOfDimensionsIO > TOutputImage::ImageDimension )
    {
    for ( unsigned int k = 0; k < numberOfDimensionsIO; ++k )
      {
      directionIO.push_back( m_ImageIO->GetDefaultDirection(k) );
      }
    }
  else
    {
    for ( unsigned int k = 0; k < numberOfDimensionsIO; ++k )
      {
      directionIO.push_back( m_ImageIO->GetDirection(k) );
      }
    }

  std::vector< double > axis;

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
    {
    if ( i < numberOfDimensionsIO )
      {
      dimSize[i] = m_ImageIO->GetDimensions(i);
      spacing[i] = m_ImageIO->GetSpacing(i);
      origin[i]  = m_ImageIO->GetOrigin(i);

      // Please note: direction cosines are stored as columns of the
      // direction matrix
      axis = directionIO[i];
      for ( unsigned j = 0; j < TOutputImage::ImageDimension; ++j )
        {
        if ( j < numberOfDimensionsIO )
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
      for ( unsigned j = 0; j < TOutputImage::ImageDimension; ++j )
        {
        if ( i == j )
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
  // Spacing is expected to be greater than 0
  // If negative, flip image direction along this axis.
  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
    {
    if(spacing[i] < 0)
      {
      spacing[i] = -spacing[i];
      for ( unsigned j = 0; j < TOutputImage::ImageDimension; ++j )
        {
        direction[j][i] = -direction[j][i];
        }
      }
    }
  output->SetSpacing(spacing);       // Set the image spacing
  output->SetOrigin(origin);         // Set the image origin
  output->SetDirection(direction);   // Set the image direction cosines

  //Copy MetaDataDictionary from instantiated reader to output image.
  output->SetMetaDataDictionary( m_ImageIO->GetMetaDataDictionary() );
  this->SetMetaDataDictionary( m_ImageIO->GetMetaDataDictionary() );

  IndexType start;
  start.Fill(0);

  ImageRegionType region;
  region.SetSize(dimSize);
  region.SetIndex(start);

  // If a VectorImage, this requires us to set the
  // VectorLength before allocate
  if ( strcmp(output->GetNameOfClass(), "VectorImage") == 0 )
    {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength( output, m_ImageIO->GetNumberOfComponents() );
    }

  output->SetLargestPossibleRegion(region);
}

template< typename TOutputImage, typename ConvertPixelTraits >
void
ImageFileReader< TOutputImage, ConvertPixelTraits >
::TestFileExistanceAndReadability()
{
  // Test if the file exists.
  if ( !itksys::SystemTools::FileExists( this->GetFileName().c_str() ) )
    {
    ImageFileReaderException e(__FILE__, __LINE__);
    std::ostringstream       msg;
    msg << "The file doesn't exist. "
        << std::endl << "Filename = " << this->GetFileName()
        << std::endl;
    e.SetDescription( msg.str().c_str() );
    throw e;
    return;
    }

  // Test if the file can be open for reading access.
  std::ifstream readTester;
  readTester.open( this->GetFileName().c_str() );
  if ( readTester.fail() )
    {
    readTester.close();
    std::ostringstream msg;
    msg << "The file couldn't be opened for reading. "
        << std::endl << "Filename: " << this->GetFileName()
        << std::endl;
    ImageFileReaderException e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
    }
  readTester.close();
}

template< typename TOutputImage, typename ConvertPixelTraits >
void
ImageFileReader< TOutputImage, ConvertPixelTraits >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  itkDebugMacro (<< "Starting EnlargeOutputRequestedRegion() ");
  typename TOutputImage::Pointer out = dynamic_cast< TOutputImage * >( output );
  typename TOutputImage::RegionType largestRegion = out->GetLargestPossibleRegion();
  ImageRegionType streamableRegion;

  // The following code converts the ImageRegion (templated over dimension)
  // into an ImageIORegion (not templated over dimension).
  ImageRegionType imageRequestedRegion = out->GetRequestedRegion();
  ImageIORegion   ioRequestedRegion(TOutputImage::ImageDimension);

  typedef ImageIORegionAdaptor< TOutputImage::ImageDimension > ImageIOAdaptor;

  ImageIOAdaptor::Convert( imageRequestedRegion, ioRequestedRegion, largestRegion.GetIndex() );

  // Tell the IO if we should use streaming while reading
  m_ImageIO->SetUseStreamedReading(m_UseStreaming);

  // Delegate to the ImageIO the computation of how the
  // requested region must be enlarged.
  m_ActualIORegion  =
    m_ImageIO->GenerateStreamableReadRegionFromRequestedRegion(ioRequestedRegion);

  // the m_ActualIORegion may be more dimensions then the output
  // Image, in which case we still need to read this larger region to
  // support reading the "first slice" of a larger image
  // see bug 9212

  // convert the IORegion to a ImageRegion (which is dimension templated)
  // if the ImageIO must read a higher dimension region, this will
  // truncate the last dimensions
  ImageIOAdaptor::Convert( m_ActualIORegion, streamableRegion, largestRegion.GetIndex() );

  // Check whether the imageRequestedRegion is fully contained inside the
  // streamable region. Since, ImageRegion::IsInside regards zero
  // sized regions, as not being inside any other region, we must
  // specially check this condition to enable zero sized regions to
  // pass the region propagation phase of the pipeline.
  if ( !streamableRegion.IsInside(imageRequestedRegion)
       && imageRequestedRegion.GetNumberOfPixels() != 0 )
    {
    // we must use a InvalidRequestedRegionError since
    // DataObject::PropagateRequestedRegion() has an exception
    // specification
    std::ostringstream message;
    message << "ImageIO returns IO region that does not fully contain the requested region"
            << "Requested region: " << imageRequestedRegion
            << "StreamableRegion region: " << streamableRegion;
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription( message.str().c_str() );
    throw e;
    }

  itkDebugMacro (
    << "RequestedRegion is set to:" << streamableRegion << " while the m_ActualIORegion is: " << m_ActualIORegion);

  out->SetRequestedRegion(streamableRegion);
}

template< typename TOutputImage, typename ConvertPixelTraits >
void ImageFileReader< TOutputImage, ConvertPixelTraits >
::GenerateData()
{
  this->UpdateProgress( 0.0f );

  typename TOutputImage::Pointer output = this->GetOutput();

  itkDebugMacro (<< "ImageFileReader::GenerateData() \n"
                 << "Allocating the buffer with the EnlargedRequestedRegion \n"
                 << output->GetRequestedRegion() << "\n");

  // allocated the output image to the size of the enlarge requested region
  this->AllocateOutputs();

  // Test if the file exists and if it can be opened.
  // An exception will be thrown otherwise, since we can't
  // successfully read the file. We catch the exception because some
  // ImageIO's may not actually open a file. Still
  // reports file error if no ImageIO is loaded.

  try
    {
    m_ExceptionMessage = "";
    this->TestFileExistanceAndReadability();
    }
  catch ( itk::ExceptionObject & err )
    {
    m_ExceptionMessage = err.GetDescription();
    }

  // Tell the ImageIO to read the file
  m_ImageIO->SetFileName( this->GetFileName().c_str() );

  itkDebugMacro (<< "Setting imageIO IORegion to: " << m_ActualIORegion);
  m_ImageIO->SetIORegion(m_ActualIORegion);

  char *loadBuffer = ITK_NULLPTR;
  // the size of the buffer is computed based on the actual number of
  // pixels to be read and the actual size of the pixels to be read
  // (as opposed to the sizes of the output)
  size_t sizeOfActualIORegion = m_ActualIORegion.GetNumberOfPixels()
                                * ( m_ImageIO->GetComponentSize() * m_ImageIO->GetNumberOfComponents() );

  try
    {
    ImageIOBase::IOComponentType ioType =
      ImageIOBase
      ::MapPixelType< typename ConvertPixelTraits::ComponentType >::CType;
    if ( m_ImageIO->GetComponentType() != ioType
         || ( m_ImageIO->GetNumberOfComponents() !=
              ConvertPixelTraits::GetNumberOfComponents() ) )
      {
      // the pixel types don't match so a type conversion needs to be
      // performed
      itkDebugMacro( << "Buffer conversion required from: "
                     << m_ImageIO->GetComponentTypeAsString(m_ImageIO->GetComponentType())
                     << " to: "
                     << m_ImageIO->GetComponentTypeAsString(ioType)
                     << " ConvertPixelTraits::NumComponents "
                     << ConvertPixelTraits::GetNumberOfComponents()
                     << " m_ImageIO->NumComponents "
                     << m_ImageIO->GetNumberOfComponents() );

      loadBuffer = new char[sizeOfActualIORegion];
      m_ImageIO->Read( static_cast< void * >( loadBuffer ) );

      // See note below as to why the buffered region is needed and
      // not actualIOregion
      this->DoConvertBuffer( static_cast< void * >( loadBuffer ),
                             output->GetBufferedRegion().GetNumberOfPixels() );
      }
    else if ( m_ActualIORegion.GetNumberOfPixels() !=
              output->GetBufferedRegion().GetNumberOfPixels() )
      {
      // NOTE:
      // for the number of pixels read and the number of pixels
      // requested to not match, the dimensions of the two regions may
      // be different, therefore we buffer and copy the pixels

      itkDebugMacro(<< "Buffer required because file dimension is greater then image dimension");

      OutputImagePixelType *outputBuffer = output->GetPixelContainer()->GetBufferPointer();

      loadBuffer = new char[sizeOfActualIORegion];
      m_ImageIO->Read( static_cast< void * >( loadBuffer ) );

      // we use std::copy here as it should be optimized to memcpy for
      // plain old data, but still is oop
      std::copy(reinterpret_cast< const OutputImagePixelType * >( loadBuffer ),
                             reinterpret_cast< const OutputImagePixelType * >( loadBuffer ) + output->GetBufferedRegion().GetNumberOfPixels(),
                             outputBuffer);
      }
    else
      {
      itkDebugMacro(<< "No buffer conversion required.");

      OutputImagePixelType *outputBuffer = output->GetPixelContainer()->GetBufferPointer();
      m_ImageIO->Read(outputBuffer);
      }
    }
  catch ( ... )
    {
    // if an exception is thrown catch it

    // clean up
    delete[] loadBuffer;
    loadBuffer = ITK_NULLPTR;

    // then rethrow
    throw;
    }

  this->UpdateProgress( 1.0f );

  // clean up
  delete[] loadBuffer;
  loadBuffer = ITK_NULLPTR;
}

template< typename TOutputImage, typename ConvertPixelTraits >
void
ImageFileReader< TOutputImage, ConvertPixelTraits >
::DoConvertBuffer(void *inputData,
                  size_t numberOfPixels)
{
  // get the pointer to the destination buffer
  OutputImagePixelType *outputData =
    this->GetOutput()->GetPixelContainer()->GetBufferPointer();
  bool isVectorImage(strcmp(this->GetOutput()->GetNameOfClass(),
                            "VectorImage") == 0);
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

#define ITK_CONVERT_BUFFER_IF_BLOCK(_CType,type)                        \
  else if(m_ImageIO->GetComponentType() == _CType)                      \
    {                                                                   \
    if (isVectorImage)                                                  \
      {                                                                 \
      ConvertPixelBuffer<type,                                          \
                         OutputImagePixelType,                          \
                         ConvertPixelTraits                             \
                         >                                              \
        ::ConvertVectorImage(static_cast< type * >( inputData ),        \
                             m_ImageIO->GetNumberOfComponents(),        \
                             outputData,                                \
                             numberOfPixels);                           \
      }                                                                 \
    else                                                                \
      {                                                                 \
      ConvertPixelBuffer<type,                                          \
                         OutputImagePixelType,                          \
                         ConvertPixelTraits                             \
                         >                                              \
        ::Convert(static_cast< type * >( inputData ),                   \
                  m_ImageIO->GetNumberOfComponents(),                   \
                  outputData,                                           \
                  numberOfPixels);                                      \
      }                                                                 \
    }

  if(0) {}
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::UCHAR,unsigned char)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::CHAR,char)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::USHORT,unsigned short)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::SHORT,short)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::UINT,unsigned int)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::INT,int)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::ULONG,unsigned long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::LONG,long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::ULONGLONG,unsigned long long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::LONGLONG,long long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::FLOAT,float)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::DOUBLE,double)
  else
    {
#define TYPENAME(x)                                     \
    m_ImageIO->GetComponentTypeAsString                 \
      (ImageIOBase::MapPixelType<x>::CType)

    ImageFileReaderException e(__FILE__, __LINE__);
    std::ostringstream       msg;
    msg << "Couldn't convert component type: "
        << std::endl << "    "
        << m_ImageIO->GetComponentTypeAsString( m_ImageIO->GetComponentType() )
        << std::endl << "to one of: "
        << std::endl << "    " << TYPENAME( unsigned char )
        << std::endl << "    " << TYPENAME( char )
        << std::endl << "    " << TYPENAME( unsigned short )
        << std::endl << "    " << TYPENAME( short )
        << std::endl << "    " << TYPENAME( unsigned int )
        << std::endl << "    " << TYPENAME( int )
        << std::endl << "    " << TYPENAME( unsigned long )
        << std::endl << "    " << TYPENAME( long )
        << std::endl << "    " << TYPENAME( unsigned long long )
        << std::endl << "    " << TYPENAME( long long )
        << std::endl << "    " << TYPENAME( float )
        << std::endl << "    " << TYPENAME( double )
        << std::endl;
    e.SetDescription( msg.str().c_str() );
    e.SetLocation(ITK_LOCATION);
    throw e;
    return;
    }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}
} //namespace ITK

#endif
