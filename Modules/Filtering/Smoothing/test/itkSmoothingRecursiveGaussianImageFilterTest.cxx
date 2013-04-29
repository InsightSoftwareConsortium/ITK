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
#include "itkFilterWatcher.h"
#include "itkImageRegionConstIterator.h"

namespace
{

int InPlaceTest( void )
{
// Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Define their size, and start index
  mySizeType size;
  size[0] = 11;
  size[1] = 11;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();

  // Initialize Image
  inputImage->SetRegions( region );
  inputImage->Allocate();

  inputImage->FillBuffer( 0.0 );


  myIndexType index;
  index[0] = ( size[0] - 1 ) / 2;  // the middle pixel
  index[1] = ( size[1] - 1 ) / 2;  // the middle pixel

  inputImage->SetPixel( index, 1.0 );

  typedef itk::SmoothingRecursiveGaussianImageFilter<myImageType >  myFilterType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();
  filter->SetInput( inputImage );
  filter->SetSigma( 1.0 );

  filter->Update();

  myImageType::Pointer outputImage1 = filter->GetOutput();
  outputImage1->DisconnectPipeline();


  filter->InPlaceOn();
  filter->Update();

  myImageType::Pointer outputImage2 = filter->GetOutput();
  outputImage2->DisconnectPipeline();

  typedef itk::ImageRegionConstIterator< myImageType > IteratorType;
  IteratorType  it1( outputImage1, outputImage1->GetBufferedRegion() );
  IteratorType  it2( outputImage2, outputImage2->GetBufferedRegion() );


  // check value of the in-place and not in-place executions are the same
  it1.GoToBegin();
  it2.GoToBegin();
  while( ! it1.IsAtEnd() )
    {
    if ( it1.Get() - it2.Get() > itk::NumericTraits<double>::epsilon() )
      {
      std::cout << "ERROR at " << it1.GetIndex() << " " << it1.Get() << " "
                << it2.Get() << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << it1.Get() << std::endl;
    ++it1;
    ++it2;
    }

  std::cout << "CanRunInPlace: " << filter->CanRunInPlace() << std::endl;
  std::cout << "input buffer region: " << inputImage->GetBufferedRegion() << std::endl;
  std::cout << "output buffer region: " << outputImage2->GetBufferedRegion() << std::endl;

  if ( inputImage->GetBufferedRegion().GetNumberOfPixels() != 0 )
    {
    std::cerr << "Failure for filter to run in-place!" << std::endl;
    return EXIT_FAILURE;
    }

  if ( !filter->CanRunInPlace() )
    {
    std::cerr << "expected CanRunInPlace to be true!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

}

int itkSmoothingRecursiveGaussianImageFilterTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator type for the input image
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  while( !it.IsAtEnd() )
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  start[0] = 2;
  start[1] = 2;
  start[2] = 2;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  myIteratorType itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() )
  {
    itb.Set( 100.0 );
    ++itb;
  }

  // Declare the type for the
  typedef itk::SmoothingRecursiveGaussianImageFilter<
                                            myImageType >  myFilterType;

  typedef myFilterType::OutputImageType myGradientImageType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();
  FilterWatcher watchit(filter);

  // Connect the input images
  filter->SetInput( inputImage );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );


  // Execute the filter
  try
    {
    filter->Update();
    }
  catch(itk::ExceptionObject &err)
    {
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
    }


  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // Declare Iterator type for the output image
  typedef itk::ImageRegionIteratorWithIndex<
                                 myGradientImageType>  myOutputIteratorType;

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage,
                            outputImage->GetRequestedRegion() );

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while( !itg.IsAtEnd() )
  {
    std::cout << itg.Get() << std::endl;
    ++itg;
  }

  if ( InPlaceTest() == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
