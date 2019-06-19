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

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkSimpleFilterWatcher.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"

namespace
{

template< typename TFilter >
int InPlaceTest( char * inputFilename, bool normalizeAcrossScale, typename TFilter::SigmaArrayType::ValueType sigmaValue )
{
  // Read the input image
  using ReaderType = itk::ImageFileReader< typename TFilter::InputImageType >;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFilename );

  // Create the filter
  typename TFilter::Pointer filter = TFilter::New();

  filter->SetNormalizeAcrossScale( normalizeAcrossScale );
  filter->SetSigma( sigmaValue );

  filter->SetInput( reader->GetOutput() );

  if( !filter->CanRunInPlace() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Expected the filter to be able to run in-place!" << std::endl;
    std::cerr << "Expected itk:SmoothingRecursiveGaussianImageFilter::CanRunInPlace to be true, but got: "
      << filter->CanRunInPlace() << std::endl;
    return EXIT_FAILURE;
    }


  ITK_TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  typename TFilter::OutputImageType::Pointer outputImage1 = filter->GetOutput();
  outputImage1->DisconnectPipeline();


  // Set the InPlace flag to On
  filter->InPlaceOn();
  ITK_TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  typename TFilter::OutputImageType::Pointer outputImage2 = filter->GetOutput();
  outputImage2->DisconnectPipeline();

  using IteratorType = itk::ImageRegionConstIterator< typename TFilter::OutputImageType >;
  IteratorType it1( outputImage1, outputImage1->GetBufferedRegion() );
  IteratorType it2( outputImage2, outputImage2->GetBufferedRegion() );

  // Check whether the values of the in-place and not in-place executions are the same
  it1.GoToBegin();
  it2.GoToBegin();
  double epsilon = itk::NumericTraits< double >::epsilon();
  while( !it1.IsAtEnd() )
    {
    if( !itk::Math::FloatAlmostEqual( static_cast< double >( it1.Get() ), static_cast< double >( it2.Get() ), 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in pixel value at index [" << std::endl;
      std::cerr << "Error in pixel value at index [" << it1.GetIndex() << "]" << std::endl;
      std::cerr << "Expected value " << it1.Get() << std::endl;
      std::cerr << " differs from " << it2.Get();
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++it1;
    ++it2;
    }


  return EXIT_SUCCESS;
}

}

int itkSmoothingRecursiveGaussianImageFilterTest( int argc, char* argv[] )
{
  if( argc != 5 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile outputImageFile normalizeAcrossScale sigma" << std::endl;
    return EXIT_FAILURE;
    }

  int testStatus = EXIT_SUCCESS;

  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  // Declare the types of the images
  using PixelType = unsigned char;
  using ImageType = itk::Image< PixelType, Dimension >;

  // Read the input image
  using ReaderType = itk::ImageFileReader< ImageType >;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Declare the type for the itk::SmoothingRecursiveGaussianImageFilter
  using SmoothingRecursiveGaussianImageFilterType = itk::SmoothingRecursiveGaussianImageFilter< ImageType >;

  // Create the filter
  SmoothingRecursiveGaussianImageFilterType::Pointer filter = SmoothingRecursiveGaussianImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( filter, SmoothingRecursiveGaussianImageFilter, InPlaceImageFilter );

  itk::SimpleFilterWatcher watcher( filter );


  // Set the scale normalization flag
  bool normalizeAcrossScale = std::stoi( argv[3] );
  ITK_TEST_SET_GET_BOOLEAN( filter, NormalizeAcrossScale, normalizeAcrossScale );

  // Set the value ofthe standard deviation of the Gaussian used for smoothing
  SmoothingRecursiveGaussianImageFilterType::SigmaArrayType::ValueType sigmaValue = std::stod( argv[4] );
  SmoothingRecursiveGaussianImageFilterType::SigmaArrayType sigma;
  sigma.Fill( sigmaValue );

  filter->SetSigma( sigmaValue );
  ITK_TEST_SET_GET_VALUE( sigmaValue, filter->GetSigma() );

  filter->SetSigmaArray( sigma );
  ITK_TEST_SET_GET_VALUE( sigma, filter->GetSigmaArray() );


  // Set the input image
  filter->SetInput( reader->GetOutput() );

  // Run the filter
  ITK_TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Write the output
  using WriterType = itk::ImageFileWriter< ImageType >;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( filter->GetOutput() );

  ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // Test the InPlaceOn option output
  if( InPlaceTest< SmoothingRecursiveGaussianImageFilterType >( argv[1], normalizeAcrossScale, sigmaValue ) == EXIT_FAILURE )
    {
    testStatus = EXIT_FAILURE;
    }


  // All objects should be automatically destroyed at this point
  std::cout << "Test finished." << std::endl;
  return testStatus;
}
