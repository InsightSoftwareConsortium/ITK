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

#include "itkRandomImageSource.h"
#include "itkChangeLabelImageFilter.h"


int itkChangeLabelImageFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::Image<unsigned short, ImageDimension> InputImageType;
  typedef itk::Image<unsigned char, ImageDimension>  OutputImageType;
  typedef InputImageType::PixelType                  InputPixelType;
  typedef OutputImageType::PixelType                 OutputPixelType;

  // Declare iterator type
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType>  InputIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType>  OutputIteratorType;

  // Use a random image source as input
  typedef itk::RandomImageSource<InputImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  InputImageType::SizeValueType sizeArray[ImageDimension] = { 3,3,3 };

  // limit to a few labels
  InputPixelType upper = 10;
  source->SetMin( itk::NumericTraits<InputPixelType>::ZeroValue() );
  source->SetMax( upper );
  source->SetSize( sizeArray );

  // Declare the type for the binary threshold filter
  typedef itk::ChangeLabelImageFilter< InputImageType,
                               OutputImageType  >  FilterType;


  // Create a filter
  FilterType::Pointer filter = FilterType::New();

  // Eliminate most labels
  InputPixelType background = 0;
  InputPixelType maxRemainingLabel = 2;
  for (InputPixelType i = maxRemainingLabel; i <= upper; i++) {
    filter->SetChange( i, background );
  }

  filter->Print( std::cout );

  // exercise Get methods


  // Connect the input images
  filter->SetInput( source->GetOutput() );

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  try
    {
    filter->Update();
    filter->SetFunctor(filter->GetFunctor());
    }
  catch(...)
    {
    std::cerr << "Caught an unexpected exception. " << std::endl;
    std::cerr << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // Create an iterator for going through the image output
  InputIteratorType  it( source->GetOutput(), source->GetOutput()->GetRequestedRegion() );
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  bool pass = true;

  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
  {

    const InputPixelType  input  = it.Get();
    const OutputPixelType output = ot.Get();
    std::cout <<  (double) input  << " " << (double) output << std::endl;

    if( output > maxRemainingLabel )
      {
        pass = false;
      }
    if ( !pass )
      {
      std::cerr << "Error in itkChangeLaelImageFilterTest " << std::endl;
      std::cerr << " input = " << input;
      std::cerr << " output = " << output;
      std::cerr << std::endl;
      return EXIT_FAILURE;
      }

    ++ot;
    ++it;
  }


  // Test to see if clearing the changemap works
  filter->ClearChangeMap();

  // reexecute the filter
  try
    {
    filter->Update();
    }
  catch(...)
    {
    std::cerr << "Caught an unexpected exception. " << std::endl;
    std::cerr << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // Create an iterator for going through the image output
  InputIteratorType  ita( source->GetOutput(), source->GetOutput()->GetRequestedRegion() );
  OutputIteratorType ota(outputImage, outputImage->GetRequestedRegion());


  //  Check the content of the result image
  //  Since the change map is clear, input is expected to be the same as output
  std::cout << "Verification of the output " << std::endl;
  ota.GoToBegin();
  ita.GoToBegin();
  while( !ota.IsAtEnd() )
  {

    const InputPixelType  input  = ita.Get();
    const OutputPixelType output = ota.Get();
    std::cout <<  (double) input  << " " << (double) output << std::endl;

    if( input != output )
      {
        pass = false;
      }
    if ( !pass )
      {
      std::cerr << "Error in itkChangeLaelImageFilterTest " << std::endl;
      std::cerr << " input = " << input;
      std::cerr << " output = " << output;
      std::cerr << std::endl;
      return EXIT_FAILURE;
      }

    ++ota;
    ++ita;
  }

  if ( pass )
    {
    std::cout << "Test passsed. " << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }
}
