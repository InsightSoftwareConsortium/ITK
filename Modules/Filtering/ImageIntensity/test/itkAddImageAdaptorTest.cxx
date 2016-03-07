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

#include "itkAddImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSubtractImageFilter.h"

int itkAddImageAdaptorTest(int, char* [] )
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel type
  typedef int  PixelType;

  // Declare the types of the image
  typedef itk::Image<PixelType, Dimension>    ImageType;

  // Declare the type of the index to access images
  typedef itk::Index<Dimension>         IndexType;

  // Declare the type of the size
  typedef itk::Size<Dimension>          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<Dimension>   RegionType;

  // Create input image
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

  // Initialize Image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator type apropriated for this image
  typedef itk::ImageRegionIteratorWithIndex<ImageType>  IteratorType;

  // Create one iterator for Image A.
  IteratorType it1( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  PixelType value = 13;
  while( !it1.IsAtEnd() )
  {
    it1.Set( value );
    value += 1;
    std::cout << it1.Get() << std::endl;
    ++it1;
  }

  //---------------------------------------
  // This section tests for AddImageAdaptor
  //---------------------------------------

  typedef itk::AddImageAdaptor< ImageType >  AdaptorType;

  AdaptorType::Pointer addAdaptor = AdaptorType::New();

  PixelType additiveConstant = 19;

  addAdaptor->SetImage( inputImage );
  addAdaptor->SetValue( additiveConstant );

  typedef itk::SubtractImageFilter<
                        AdaptorType,
                        ImageType,
                        ImageType   > DiffFilterType;

  DiffFilterType::Pointer diffFilter = DiffFilterType::New();

  diffFilter->SetInput1( addAdaptor  );
  diffFilter->SetInput2( inputImage );

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  ImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  std::cout << "Comparing the results with those of an Adaptor" << std::endl;
  std::cout << "Verification of the output " << std::endl;

  // Create an iterator for going through the image output
  IteratorType dt(diffImage, diffImage->GetBufferedRegion());

  typedef itk::NumericTraits< PixelType >::RealType  RealPixelType;

  dt.GoToBegin();

  while( !dt.IsAtEnd() )
    {
    std::cout <<  dt.Get() << std::endl;

    RealPixelType v1 = static_cast< RealPixelType >( dt.Get() );
    RealPixelType v2 = static_cast< RealPixelType >( additiveConstant );

    RealPixelType diff = std::fabs( v1 - v2 );

    if( diff > itk::Math::eps )
      {
      std::cerr << "Error in itkAddImageFilterTest " << std::endl;
      std::cerr << "Comparing results with Adaptors" << std::endl;
      std::cerr << " difference = " << diff << std::endl;
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << itk::Math::eps << std::endl;
      return EXIT_FAILURE;
      }
    ++dt;
    }

  IndexType index;

  index[0] = 1;
  index[1] = 1;
  index[2] = 1;

  PixelType  p1 = addAdaptor->GetPixel( index );

  std::cout << " Pixel " << index << " had value = " << p1 << std::endl;

  PixelType newValue = 27;

  std::cout << " We set Pixel " << index << " to value = " << newValue << std::endl;
  addAdaptor->SetPixel( index, newValue );

  PixelType  p2 = addAdaptor->GetPixel( index );
  std::cout << " Now Pixel " << index << " has value = " << p2 << std::endl;

  if( p2 != newValue )
    {
    std::cerr << "SetPixel()/GetPixel() methods failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
