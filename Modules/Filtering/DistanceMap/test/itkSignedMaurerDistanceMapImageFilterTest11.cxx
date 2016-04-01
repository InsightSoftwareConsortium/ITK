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

#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkStdStreamStateSave.h"
#include <iostream>

int itkSignedMaurerDistanceMapImageFilterTest11(int, char* [] )
{

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "Test ITK Liza Signed Maurer Distance Map" << std::endl << std::endl;

  std::cout << "Compute the distance map of a 5x5 image" << std::endl;
  std::cout << "with a point at (4,4) (value=1)" << std::endl << std::endl;
  std::cout << "with a point at (1,6) (value=2)" << std::endl << std::endl;

  typedef itk::Image<unsigned char, 2>  myImageType2D1;
  typedef itk::Image<float, 2>          myImageType2D2;

  /* Allocate the 2D image */
  myImageType2D1::SizeType size2D = {{5,5}};
  myImageType2D1::IndexType index2D = {{0,0}};
  myImageType2D1::RegionType region2D;
  region2D.SetSize( size2D );
  region2D.SetIndex( index2D );

  myImageType2D1::Pointer inputImage2D = myImageType2D1::New();
  inputImage2D->SetLargestPossibleRegion( region2D );
  inputImage2D->SetBufferedRegion( region2D );
  inputImage2D->SetRequestedRegion( region2D );
  inputImage2D->Allocate();

  /* Set pixel (4,4) with the value 1
   * and pixel (1,6) with the value 2
   * The SignedMaurer Distance is performed for each pixel with a value > 0
   * The ClosestPoints computation is based on the value of the pixel.
   */

  typedef  itk::ImageRegionIteratorWithIndex<myImageType2D1> myIteratorType2D1;

  myIteratorType2D1 it2D1(inputImage2D,region2D);

  // Set the image to 0
  while( !it2D1.IsAtEnd() )
    {
    it2D1.Set( 0 );
    ++it2D1;
    }

  index2D[0] = 3;
  index2D[1] = 3;
  inputImage2D->SetPixel( index2D, 1);
  //index2D[0] = 1;
  //index2D[1] = 6;
  //inputImage2D->SetPixel( index2D, 2);

  /* Create SignedMaurerDistance Map filter */
  typedef itk::SignedMaurerDistanceMapImageFilter<
                                            myImageType2D1,
                                            myImageType2D2 > myFilterType2D;

  myFilterType2D::Pointer filter2D = myFilterType2D::New();

  filter2D->SetInput( inputImage2D );

  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();
  filter2D->Update();

  /* Show Distance map */

  itk::ImageSliceConstIteratorWithIndex<myImageType2D2> it2D2(
                                outputDistance2D,
                                outputDistance2D->GetRequestedRegion() );

  it2D2.GoToBegin();
  it2D2.SetFirstDirection ( 0 );
  it2D2.SetSecondDirection( 1 );

  while( !it2D2.IsAtEnd() )
    {
    while( !it2D2.IsAtEndOfSlice() )
      {
      while( !it2D2.IsAtEndOfLine() )
        {
        std::cout.width(5);
        std::cout << it2D2.Get() << "\t";
        ++it2D2;
        }
      std::cout << std::endl;
      it2D2.NextLine();
      }
    it2D2.NextSlice();
    }

  /* Test Squared Distance functionality */
  // Get the value of pixel at location [0,0] before distance is squared
  myImageType2D2::IndexType index;
  index[0] = 0;
  index[1] = 0;
  std::cout<<"here"<<std::endl;
  const double distance1 = outputDistance2D->GetPixel( index );
  std::cout<<"distance1: "<<distance1<< std::endl;

  filter2D->SquaredDistanceOn();
  //filter2D->SquaredDistanceOff();

  if( filter2D->GetSquaredDistance() != true )
    {
    std::cerr << "filter2D->GetSquaredDistance() != true" <<std::endl;
    return EXIT_FAILURE;
    }

//  filter2D->SetSquaredDistance( true );
  filter2D->Update();

  const double distance2 = outputDistance2D->GetPixel( index );
  std::cout<<"distance2: "<<distance2<< std::endl;
  const myImageType2D2::PixelType epsilon = 1e-5;
  if( std::fabs( distance2 - distance1 * distance1 ) > epsilon )
    {
    std::cerr << "Error in use of the SetSquaredDistance() method" << std::endl;
    return EXIT_FAILURE;
    }

  /* Show Squared Distance map */

  std::cout << "Squared Distance Map " << std::endl;
  itk::ImageSliceConstIteratorWithIndex<myImageType2D2> it2D3(
                                outputDistance2D,
                                outputDistance2D->GetRequestedRegion() );

  it2D3.GoToBegin();
  it2D3.SetFirstDirection ( 0 );
  it2D3.SetSecondDirection( 1 );

  while( !it2D3.IsAtEnd() )
    {
    while( !it2D3.IsAtEndOfSlice() )
      {
      while( !it2D3.IsAtEndOfLine() )
        {
        std::cout.width(5);
        std::cout << it2D3.Get() << "\t";
        ++it2D3;
        }
      std::cout << std::endl;
      it2D3.NextLine();
      }
    it2D3.NextSlice();
    }

  /* Test for images with anisotropic spacing */
  myImageType2D1::SpacingType anisotropicSpacing;

  anisotropicSpacing[0] = 1.0;
  anisotropicSpacing[1] = 5.0;

  inputImage2D->SetSpacing( anisotropicSpacing );

  inputImage2D->FillBuffer( 0 );

  index2D[0] = 2;
  index2D[1] = 2;
  inputImage2D->SetPixel( index2D, 1);

  filter2D->SetInput( inputImage2D );
  filter2D->UseImageSpacingOn();
  filter2D->SquaredDistanceOff();
  filter2D->InsideIsPositiveOn();
  filter2D->SetBackgroundValue(0);

  if( filter2D->GetUseImageSpacing() != true )
    {
    std::cerr << "filter2D->GetUseImageSpacing() != true" <<std::endl;
    return EXIT_FAILURE;
    }

  if( filter2D->GetInsideIsPositive() != true )
    {
    std::cerr << "filter2D->GetInsideIsPositive() != true" <<std::endl;
    return EXIT_FAILURE;
    }

  if( filter2D->GetBackgroundValue() != 0 )
    {
    std::cerr << "filter2D->GetBackgroundValue() != 0" <<std::endl;
    return EXIT_FAILURE;
    }

  if( filter2D->GetSquaredDistance() == true )
    {
    std::cerr << "filter2D->GetSquaredDistance() == true & it should not" <<std::endl;
    return EXIT_FAILURE;
    }
  filter2D->SetUseImageSpacing( true );
  myImageType2D2::Pointer outputDistance2D2 = filter2D->GetOutput();
  filter2D->Update();

  /* Show ImageSpacing Distance map */
  std::cout << "Use ImageSpacing Distance Map with squared distance turned off" << std::endl;
  itk::ImageSliceConstIteratorWithIndex<myImageType2D2> it2D5(
                                outputDistance2D2,
                                outputDistance2D2->GetRequestedRegion() );
  it2D5.GoToBegin();
  it2D5.SetFirstDirection ( 0 );
  it2D5.SetSecondDirection( 1 );

  while( !it2D5.IsAtEnd() )
    {
    while( !it2D5.IsAtEndOfSlice() )
      {
      while( !it2D5.IsAtEndOfLine() )
        {
        std::cout.width(5);
        std::cout << it2D5.Get() << "\t";
        ++it2D5;
        }
      std::cout << std::endl;
      it2D5.NextLine();
      }
    it2D5.NextSlice();
    }

  return EXIT_SUCCESS;
}
