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

#include "itkEdgePotentialImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkImageRegionIterator.h"

int itkEdgePotentialImageFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the types of the images
  typedef itk::CovariantVector<double,ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension>      InputImageType;
  typedef itk::Image<float, ImageDimension>           OutputImageType;

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIterator<InputImageType>  InputIteratorType;
  typedef itk::ImageRegionIterator<OutputImageType> OutputIteratorType;


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

  // Initialize input image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Create one iterator for the Input Image.
  InputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content input image
  VectorType vec;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    vec[j] = 10.0;
    }
  std::cout << "Content of the Input " << std::endl;
  it.GoToBegin();
  while( !it.IsAtEnd() )
  {
    it.Set( vec );
    std::cout << it.Get() << std::endl;
    ++it;
  }

  // create an EdgePotentialImageFilter
  typedef itk::EdgePotentialImageFilter< InputImageType,
                               OutputImageType  >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter);

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
    const InputImageType::PixelType  input  = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType pot  = std::exp( -1.0 * (it.Get().GetNorm() ) );
    std::cout <<  ot.Get() << " = ";
    std::cout <<  pot  << std::endl;
    if( std::fabs( pot - output ) > epsilon )
    {
      std::cerr << "Error in itkEdgePotentialImageFilterTest " << std::endl;
      std::cerr << " potential( " << input << ") = " << pot << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
    }
    ++ot;
    ++it;
  }

  return EXIT_SUCCESS;
}
