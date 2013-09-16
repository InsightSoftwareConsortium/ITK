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

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSimpleFilterWatcher.h"

template<typename TImage1Type,typename TImage2Type>
class ImageInformationIsEqual
{
public:
static bool Check(const TImage1Type * image1, const TImage2Type * image2)
{
  if (image1->GetSpacing() != image2->GetSpacing())
    {
    return false;
    }
  if (image1->GetOrigin() != image2->GetOrigin())
    {
    return false;
    }
  if (image1->GetDirection() != image2->GetDirection())
    {
    return false;
    }
  return true;
}
};

int itkGradientMagnitudeRecursiveGaussianFilterTest(int, char* [] )
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

  // Set the metadata for the image
  myImageType::PointType origin;
  myImageType::SpacingType spacing;
  myImageType::DirectionType direction;

  origin[0] = 1.0; origin[1] = 2.0; origin[2] = 3.0;
  spacing[0] = .1; spacing[1] = .2; spacing[2] = .3;
  direction.SetIdentity();
  direction(1,1) = -1.0;

  inputImage->SetSpacing(spacing);
  inputImage->SetOrigin(origin);
  inputImage->SetDirection(direction);

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
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<
                                            myImageType >  myFilterType;

  typedef myFilterType::OutputImageType myGradientImageType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();
  itk::SimpleFilterWatcher watcher(filter);


  // Connect the input images
  filter->SetInput( inputImage );

  // Select the value of Sigma
  filter->SetSigma( 2.5 );


  // Execute the filter
  try
    {
    filter->Update();
    }
  catch(...)
    {
    std::cerr << "Exception thrown during Update() " << std::endl;
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

  if (!ImageInformationIsEqual<myImageType,myImageType>::Check(inputImage, outputImage))
    {
    std::cout << "ImageInformation mismatch!" << std::endl;
    std::cout << "inputImage Origin:  " << inputImage->GetOrigin() << std::endl;
    std::cout << "outputImage Origin: " << outputImage->GetOrigin() << std::endl;
    std::cout << "inputImage Spacing:  " << inputImage->GetSpacing() << std::endl;
    std::cout << "outputImage Spacing: " << outputImage->GetSpacing() << std::endl;
    std::cout << "inputImage Direction:  " << inputImage->GetDirection() << std::endl;
    std::cout << "outputImage Direction: " << outputImage->GetDirection() << std::endl;
    return EXIT_FAILURE;
    }

  // check that the same filter is able to run on a smaller image
  size.Fill( 5 );
  region.SetSize( size );

  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer( 1 );

  // Execute the filter
  try
    {
    filter->UpdateLargestPossibleRegion();
     }
   catch(...)
     {
     std::cerr << "Exception thrown during Update() " << std::endl;
     return EXIT_FAILURE;
     }

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
