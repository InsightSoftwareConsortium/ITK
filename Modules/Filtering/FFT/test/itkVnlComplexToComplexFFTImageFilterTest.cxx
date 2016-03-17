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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

template< typename TPixel, unsigned int VDimension >
int transformImage( const char * inputImageFileName, const char * outputImageFileName )
{
  typedef TPixel                        RealPixelType;
  typedef std::complex< RealPixelType > ComplexPixelType;
  const unsigned int Dimension = VDimension;

  typedef itk::Image< RealPixelType, Dimension >    RealImageType;
  typedef itk::Image< ComplexPixelType, Dimension > ComplexImageType;

  typedef itk::ImageFileReader< RealImageType >  ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputImageFileName );

  typedef itk::ForwardFFTImageFilter< RealImageType, ComplexImageType > ForwardFilterType;
  typename ForwardFilterType::Pointer forwardFilter = ForwardFilterType::New();
  forwardFilter->SetInput( reader->GetOutput() );

  typedef itk::VnlComplexToComplexFFTImageFilter< ComplexImageType > ComplexFilterType;
  typename ComplexFilterType::Pointer inverseComplexFilter = ComplexFilterType::New();
  inverseComplexFilter->SetInput( forwardFilter->GetOutput() );
  inverseComplexFilter->SetTransformDirection( ComplexFilterType::INVERSE );

  typename ComplexFilterType::Pointer forwardComplexFilter = ComplexFilterType::New();
  forwardComplexFilter->SetInput( inverseComplexFilter->GetOutput() );
  forwardComplexFilter->SetTransformDirection( ComplexFilterType::FORWARD );

  typedef itk::InverseFFTImageFilter< ComplexImageType, RealImageType > InverseFilterType;
  typename InverseFilterType::Pointer inverseFilter = InverseFilterType::New();
  inverseFilter->SetInput( forwardComplexFilter->GetOutput() );

  typedef itk::ImageFileWriter< RealImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputImageFileName );
  writer->SetInput( inverseFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkVnlComplexToComplexFFTImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " <InputImage> <OutputImage> <float|double>" << std::endl;
    return EXIT_FAILURE;
    }
  const char * inputImageFileName = argv[1];
  const char * outputImageFileName = argv[2];
  const std::string pixelTypeString( argv[3] );

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO( inputImageFileName, itk::ImageIOFactory::ReadMode );
  imageIO->SetFileName( inputImageFileName );
  imageIO->ReadImageInformation();
  const unsigned int dimension = imageIO->GetNumberOfDimensions();

  if( pixelTypeString.compare( "float" ) == 0 )
    {
    switch( dimension )
      {
    case 2:
      return transformImage< float, 2 >( inputImageFileName, outputImageFileName );
    case 3:
      return transformImage< float, 3 >( inputImageFileName, outputImageFileName );
    default:
      std::cerr << "Unknown image dimension." << std::endl;
      return EXIT_FAILURE;
      }
    return EXIT_SUCCESS;
    }
  else if( pixelTypeString.compare( "double" ) == 0 )
    {
    switch( dimension )
      {
    case 2:
      return transformImage< double, 2 >( inputImageFileName, outputImageFileName );
    case 3:
      return transformImage< double, 3 >( inputImageFileName, outputImageFileName );
    default:
      std::cerr << "Unknown image dimension." << std::endl;
      return EXIT_FAILURE;
      }
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "Unknown pixel type string." << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_FAILURE;
}
