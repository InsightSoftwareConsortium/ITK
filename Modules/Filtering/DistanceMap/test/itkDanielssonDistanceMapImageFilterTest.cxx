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
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkStdStreamStateSave.h"

int itkDanielssonDistanceMapImageFilterTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "Test ITK Danielsson Distance Map" << std::endl << std::endl;

  std::cout << "Compute the distance map of a 9x9 image" << std::endl;
  std::cout << "with a point at (4,4) (value=1)" << std::endl << std::endl;
  std::cout << "with a point at (1,6) (value=2)" << std::endl << std::endl;


  typedef itk::Image<unsigned char, 2>  myImageType2D1;
  typedef itk::Image<float, 2>          myImageType2D2;

  /* Allocate the 2D image */
  myImageType2D1::SizeType size2D = {{9,9}};
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
   * The Danielsson Distance is performed for each pixel with a value > 0
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

  index2D[0] = 4;
  index2D[1] = 4;
  inputImage2D->SetPixel( index2D, 1);
  index2D[0] = 1;
  index2D[1] = 6;
  inputImage2D->SetPixel( index2D, 2);

  /* Create Danielsson Distance Map filter */
  typedef itk::DanielssonDistanceMapImageFilter<
                                            myImageType2D1,
                                            myImageType2D2 > myFilterType2D;

  myFilterType2D::Pointer filter2D = myFilterType2D::New();

  filter2D->SetInput( inputImage2D );

  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();

  typedef myFilterType2D::VoronoiImageType VoronoiImageType;

  VoronoiImageType::Pointer outputVoronoi2D  = filter2D->GetVoronoiMap();

  myFilterType2D::VectorImagePointer
                    outputComponents = filter2D->GetVectorDistanceMap();

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

  /* Show Closest Points map */
  std::cout << std::endl << std::endl;
  std::cout << "Voronoi Map Image 2D" << std::endl << std::endl;

  itk::ImageSliceConstIteratorWithIndex< VoronoiImageType > it2D3(
                                outputVoronoi2D,
                                outputVoronoi2D->GetRequestedRegion() );

  it2D3.SetFirstDirection( 0 );
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

  /* Show VectorsComponents Points map */
  std::cout << std::endl << std::endl;
  std::cout << "Components Map Image 2D" << std::endl << std::endl;

  itk::ImageSliceConstIteratorWithIndex< myFilterType2D::VectorImageType > it2D4(
                                outputComponents,
                                outputComponents->GetRequestedRegion() );

  it2D4.SetFirstDirection( 0 );
  it2D4.SetSecondDirection( 1 );

  while( !it2D4.IsAtEnd() )
    {
    while( !it2D4.IsAtEndOfSlice() )
      {
      while( !it2D4.IsAtEndOfLine() )
        {
        std::cout << "[";
        for (unsigned int i=0;i<2;i++)
          {
          std::cout << it2D4.Get()[i];
          if(i==0)
            {
            std::cout << ",";
            }
          }
        std::cout << "]";
        std::cout << "\t";
        ++it2D4;

        }
      std::cout << std::endl;
      it2D4.NextLine();
      }
    it2D4.NextSlice();
    }


  /* Test Squared Distance functionality */
  myImageType2D2::IndexType index;
  index[0] = 0;
  index[1] = 0;
  const double distance1 = outputDistance2D->GetPixel( index );

  filter2D->SquaredDistanceOn();

  if( filter2D->GetSquaredDistance() != true )
    {
    std::cerr << "filter2D->GetSquaredDistance() != true" <<std::endl;
    return EXIT_FAILURE;
    }

  filter2D->SetSquaredDistance( true );
  filter2D->Update();

  const double distance2 = outputDistance2D->GetPixel( index );
  const myImageType2D2::PixelType epsilon = 1e-5;
  if( std::fabs( distance2 - distance1 * distance1 ) > epsilon )
    {
    std::cerr << "Error in use of the SetSquaredDistance() method" << std::endl;
    return EXIT_FAILURE;
    }


  /* Show Squared Distance map */
  std::cout << "Squared Distance Map " << std::endl;

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


  /* Test for images with anisotropic spacing */
  myImageType2D1::SpacingType anisotropicSpacing;

  anisotropicSpacing[0] = 1.0;
  anisotropicSpacing[1] = 5.0;

  inputImage2D->SetSpacing( anisotropicSpacing );

  inputImage2D->FillBuffer( 0 );

  index2D[0] = 4;
  index2D[1] = 4;
  inputImage2D->SetPixel( index2D, 1);

  filter2D->SetInput( inputImage2D );
  filter2D->SetInputIsBinary(true);

  if( filter2D->GetInputIsBinary() != true )
    {
    std::cerr << "filter2D->GetInputIsBinary() != true" <<std::endl;
    return EXIT_FAILURE;
    }


  filter2D->UseImageSpacingOn();

  if( filter2D->GetUseImageSpacing() != true )
    {
    std::cerr << "filter2D->GetUseImageSpacing() != true" << std::endl;
    return EXIT_FAILURE;
    }

  filter2D->SetUseImageSpacing(true);
  filter2D->Update();

  index2D[1] = 5;
  myImageType2D2::PixelType expectedValue =
    static_cast<myImageType2D2::PixelType>(anisotropicSpacing[1]);
  expectedValue *= expectedValue;
  myImageType2D2::PixelType pixelValue =
    filter2D->GetOutput()->GetPixel( index2D );
  if ( std::fabs( expectedValue - pixelValue ) > epsilon )
    {
    std::cerr << "Error when image spacing is anisotropic." << std::endl;
    std::cerr << "Pixel value was " << pixelValue << ", expected " << expectedValue << std::endl;
    return EXIT_FAILURE;
    }

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

  // Create a large 3D image with a small foreground object.  The foreground is denoted by a pixel value of 0,
  // and the background by a non-zero pixel value.  This will test speedups to the code that ignore all background
  // pixels in the computation since those pixels do not influence the result.

  // Allocate the 3D image
  typedef itk::Image< float, 3>  ImageType3D;
  ImageType3D::SizeType size3D = {{200,200,200}};
  ImageType3D::IndexType index3D = {{0,0}};
  ImageType3D::RegionType region3D;
  region3D.SetSize( size3D );
  region3D.SetIndex( index3D );
  ImageType3D::Pointer inputImage3D = ImageType3D::New();
  inputImage3D->SetRegions( region3D );
  inputImage3D->Allocate();
  inputImage3D->FillBuffer(1);

  // Set a few pixels in the middle of the image to 0.  These are the foreground pixels for which the distance will be solved.
  ImageType3D::IndexType foregroundIndex;
  ImageType3D::SizeType foregroundSize;
  for( unsigned int i = 0; i < 3; i++ )
  {
    foregroundSize[i] =  5;
    foregroundIndex[i] = (size3D[i]/2) - foregroundSize[i]/2;
  }
  ImageType3D::RegionType foregroundRegion;
  foregroundRegion.SetSize( foregroundSize );
  foregroundRegion.SetIndex( foregroundIndex );

  itk::ImageRegionIteratorWithIndex<ImageType3D> it3D(inputImage3D,foregroundRegion);
  for( it3D.GoToBegin(); !it3D.IsAtEnd(); ++it3D )
  {
    it3D.Set( 0 );
  }

  // Create Danielsson Distance Map filter
  typedef itk::DanielssonDistanceMapImageFilter< ImageType3D, ImageType3D > myFilterType3D;

  myFilterType3D::Pointer filter3D = myFilterType3D::New();

  filter3D->SetInput( inputImage3D );
  filter3D->Update();

  return EXIT_SUCCESS;
}
