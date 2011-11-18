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

#include "itkExponentialDisplacementFieldImageFilter.h"

#include "vnl/vnl_random.h"


int itkExponentialDisplacementFieldImageFilterTest(int, char* [] )
{
  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  typedef itk::Vector< double, ImageDimension >   PixelType;

  // Declare the types of the images
  typedef itk::Image<PixelType, ImageDimension>  ImageType;

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex< ImageType>  IteratorType;


  // Declare the type of the index to access images
  typedef itk::Index<ImageDimension>         IndexType;

  // Declare the type of the size
  typedef itk::Size<ImageDimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  // Create two images
  ImageType::Pointer inputImage  = ImageType::New();

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
  IteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  PixelType vectorValue;
  vectorValue.Fill( 5.0 ); // FIXME: replace with something more interesting...

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( vectorValue );
    std::cout << it.Get() << std::endl;
    ++it;
    }

  // Declare the type for the filter
  typedef itk::ExponentialDisplacementFieldImageFilter<
                                  ImageType, ImageType  >   FilterType;


  // Create one filter
  FilterType::Pointer filter = FilterType::New();


  // Connect the input images
  filter->SetInput( inputImage );

  filter->SetMaximumNumberOfIterations( 20 );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  ImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  IteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  const PixelType::ValueType epsilon = 1e-6;

  bool testpassed = true;

  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    PixelType input  = it.Get();
    PixelType output = ot.Get();
    // The input is a constant field, its exponential
    // should be exactly equal
    testpassed &= ( (input-output).GetNorm() < epsilon );
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot;
    ++it;
    }


  // Ask for the inverse deformation
  filter->ComputeInverseOn();

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  ImageType::Pointer outputImage2 = filter->GetOutput();

  // Create an iterator for going through the image output
  IteratorType ot2(outputImage2, outputImage2->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the inverse output " << std::endl;

  ot2.GoToBegin();
  it.GoToBegin();
  while( !ot2.IsAtEnd() )
    {
    PixelType input  = it.Get();
    PixelType output = ot2.Get();
    // The input is a constant field, its inverse exponential
    // should be exactly equal to its opposite
    testpassed &= ( (input+output).GetNorm() < epsilon );
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot2;
    ++it;
    }


  // Try with 0 iterations
  filter->ComputeInverseOff();
  filter->SetMaximumNumberOfIterations( 0 );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  ImageType::Pointer outputImage3 = filter->GetOutput();

  // Create an iterator for going through the image output
  IteratorType ot3(outputImage3, outputImage3->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output with 0 iterations " << std::endl;

  ot3.GoToBegin();
  it.GoToBegin();
  while( !ot3.IsAtEnd() )
    {
    PixelType input  = it.Get();
    PixelType output = ot3.Get();
    // The input is a constant field, its inverse exponential
    // should be exactly equal to its opposite
    testpassed &= ( (input-output).GetNorm() < epsilon );
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot3;
    ++it;
    }


  // Try inverse with 0 iterations
  filter->ComputeInverseOn();
  filter->SetMaximumNumberOfIterations( 0 );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  ImageType::Pointer outputImage4 = filter->GetOutput();

  // Create an iterator for going through the image output
  IteratorType ot4(outputImage4, outputImage4->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the inverse output with 0 iterations " << std::endl;

  ot4.GoToBegin();
  it.GoToBegin();
  while( !ot4.IsAtEnd() )
    {
    PixelType input  = it.Get();
    PixelType output = ot4.Get();
    // The input is a constant field, its inverse exponential
    // should be exactly equal to its opposite
    testpassed &= ( (input+output).GetNorm() < epsilon );
    std::cout << input << " => ";
    std::cout << output  << std::endl;
    ++ot4;
    ++it;
    }


  // See if the output is consistent when the spacing is changed
  // (in an isotropic manner)
  const double isospacing = 10;
  typedef ImageType::SpacingType SpacingType;
  SpacingType spacing;
  for (unsigned int d=0; d<ImageDimension; ++d)
    {
    spacing[d] = isospacing;
    }

  filter->SetInput( inputImage );
  filter->SetMaximumNumberOfIterations( 20 );
  filter->ComputeInverseOff();

  // Random number generator
  vnl_random rng;
  const double power = 5.0;

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    for (unsigned int d=0; d<ImageDimension; ++d)
      {
      it.Value()[d] = power * rng.normal();
      }
    ++it;
    }

  filter->Update();
  ImageType::Pointer outputImage5 = filter->GetOutput();
  outputImage5->DisconnectPipeline();

  // Change the spacing
  inputImage->SetSpacing(spacing);
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Value() *= isospacing;
    ++it;
    }

  filter->Update();
  ImageType::Pointer outputImage6 = filter->GetOutput();

  IteratorType ot5(outputImage5, outputImage5->GetRequestedRegion());
  IteratorType ot6(outputImage6, outputImage6->GetRequestedRegion());

  std::cout << "Verification of the consistency when spacing is changed " << std::endl;

  ot5.GoToBegin();
  ot6.GoToBegin();
  while( !ot5.IsAtEnd() )
    {
    testpassed &= ( (ot5.Value()-(ot6.Value()/isospacing)).GetNorm() < epsilon );
    std::cout << ot5.Value() << " => ";
    std::cout << ot6.Value()/isospacing << std::endl;
    ++ot5;
    ++ot6;
    }

  if (!testpassed)
    {
    std::cout<<"Test failed"<<std::endl;
    return EXIT_FAILURE;
    }

  std::cout<<"Test passed"<<std::endl;
  return EXIT_SUCCESS;
}
