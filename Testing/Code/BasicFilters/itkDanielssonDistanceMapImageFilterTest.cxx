/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDanielssonDistanceMapImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkImage.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkImageSliceConstIteratorWithIndex.h>
#include <itkDanielssonDistanceMapImageFilter.h>


int itkDanielssonDistanceMapImageFilterTest(int, char* [] ) 
{
  
  std::cout << "Test ITK Danielsson Distance Map" << std::endl << std::endl;

  std::cout << "Compute the distance map of a 9x9 image" << std::endl;
  std::cout << "with a point at (4,4) (value=1)" << std::endl << std::endl;
  std::cout << "with a point at (1,6) (value=2)" << std::endl << std::endl;

  
  typedef itk::Image<float, 2>  myImageType2D1;
  typedef itk::Image<float, 2>  myImageType2D2;

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
  typedef  itk::ImageRegionIteratorWithIndex<myImageType2D2> myIteratorType2D2;

  myIteratorType2D1 it2D1(inputImage2D,region2D);

  // Set the image to 0
  while( !it2D1.IsAtEnd() ) 
  { 
    it2D1.Set( 0 );
    ++it2D1;
  }
 
  index2D[0] = 4;
  index2D[1] = 4;
  inputImage2D->GetPixel( index2D ) = 1;
  index2D[0] = 1;
  index2D[1] = 6;
  inputImage2D->GetPixel( index2D ) = 2;

  /* Create Danielsson Distance Map filter */
  typedef itk::DanielssonDistanceMapImageFilter< 
                                            myImageType2D1,
                                            myImageType2D2 > myFilterType2D;

  myFilterType2D::Pointer filter2D = myFilterType2D::New();

  filter2D->SetInput( inputImage2D ); 
  
  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();
  myImageType2D1::Pointer outputVoronoi2D  = filter2D->GetVoronoiMap();

  myFilterType2D::VectorImageType::Pointer 
                    outputComponents = filter2D->GetVectorDistanceMap();

  filter2D->SetInputIsBinary(true); 
  filter2D->Update();

  /* Show Distance map */
  itk::ImageSliceConstIteratorWithIndex<myImageType2D2> it2D2( 
                                outputDistance2D,
                                outputDistance2D->GetRequestedRegion() );

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

  itk::ImageSliceConstIteratorWithIndex< myImageType2D2 > it2D3( 
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
        std::cout << "[" ;
        for (unsigned int i=0;i<2;i++)
        {
          std::cout << it2D4.Get()[i] ;
          if(i==0) 
          {
            std::cout << ",";
          }
        }
        std::cout << "]";
        std::cout << "\t" ;
        ++it2D4;

      }
      std::cout << std::endl;
      it2D4.NextLine();
    }
    it2D4.NextSlice();
  }



  return EXIT_SUCCESS;

}
