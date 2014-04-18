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

#include "itkRGBPixel.h"
#include "itkRGBToLuminanceImageFilter.h"
#include "itkRGBToLuminanceImageAdaptor.h"
#include "itkSubtractImageFilter.h"


int itkRGBToLuminanceImageFilterAndAdaptorTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::RGBPixel< float >                        InputPixelType;
  typedef float                                         OutputPixelType;

  typedef itk::Image< InputPixelType,  ImageDimension > InputImageType;
  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType>  InputIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType>  OutputIteratorType;

  // Declare the type of the index to access images
  typedef itk::Index<ImageDimension>         IndexType;

  // Declare the type of the size
  typedef itk::Size<ImageDimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  // Create two images
  InputImageType::Pointer inputImage  = InputImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  InputPixelType pixel;
  pixel.SetRed( 1.0 );
  pixel.SetGreen( 1.0 );
  pixel.SetBlue( 1.0 );
  std::cout << "Content of the Input " << std::endl;
  it.GoToBegin();
  while( !it.IsAtEnd() )
  {
    it.Set( pixel );
    std::cout << it.Get() << std::endl;
    ++it;
  }

  // Declare the type for the RGBToLuminance filter
  typedef itk::RGBToLuminanceImageFilter< InputImageType,
                               OutputImageType  >  FilterType;


  // Create an ADD Filter
  FilterType::Pointer filter = FilterType::New();


  // Connect the input images
  filter->SetInput( inputImage );

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  const OutputImageType::PixelType epsilon = 1e-6;

  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    std::cout <<  ot.Get() << " = ";
    std::cout <<  it.Get().GetLuminance()  << std::endl;
    const InputPixelType  input  = it.Get();
    const OutputPixelType output = ot.Get();
    const OutputPixelType value  = static_cast< OutputPixelType >( input.GetLuminance() );
    if( std::fabs( value - output ) > epsilon )
      {
      std::cerr << "Error in itkRGBToLuminanceImageFilterTest " << std::endl;
      std::cerr << " Luminance( " << input << ") = " << value << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++it;
    }

  //---------------------------------------
  // This section tests for RGBToLuminanceImageAdaptor
  //---------------------------------------

  typedef itk::RGBToLuminanceImageAdaptor<InputImageType,
                          OutputPixelType>  AdaptorType;

  AdaptorType::Pointer luminanceAdaptor = AdaptorType::New();

  luminanceAdaptor->SetImage( inputImage );

  typedef itk::SubtractImageFilter<
                        OutputImageType,
                        AdaptorType,
                        OutputImageType   > DiffFilterType;

  DiffFilterType::Pointer diffFilter = DiffFilterType::New();

  diffFilter->SetInput1( outputImage );
  diffFilter->SetInput2( luminanceAdaptor  );

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  std::cout << "Comparing the results with those of an Adaptor" << std::endl;
  std::cout << "Verification of the output " << std::endl;

  // Create an iterator for going through the image output
  OutputIteratorType dt(diffImage, diffImage->GetRequestedRegion());

  dt.GoToBegin();
  while( !dt.IsAtEnd() )
    {
    std::cout <<  dt.Get() << std::endl;
    const OutputPixelType diff = dt.Get();
    if( std::fabs( diff ) > epsilon )
      {
      std::cerr << "Error in itkRGBToLuminanceImageFilterTest " << std::endl;
      std::cerr << "Comparing results with Adaptors" << std::endl;
      std::cerr << " difference = " << diff << std::endl;
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++dt;
    }


  return EXIT_SUCCESS;
}
