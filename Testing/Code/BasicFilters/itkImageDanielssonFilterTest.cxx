/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageDanielssonFilterTest.cxx itkDanielssonFilterTest.cxx,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkImage.h>
#include <itkSimpleImageRegionIterator.h>
#include <itkImageDanielssonFilter.h>


int main() 
{
  
  std::cout << "Test ITK Danielsson Distance Map" << std::endl << std::endl;

  std::cout << "Compute the distance map of a 9x9 image" ;
  std::cout << "with two points at (4,4) and (1,6)" << std::endl << std::endl; 
  
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

  typedef  itk::SimpleImageRegionIterator<myImageType2D1> myIteratorType2D1;
  typedef  itk::SimpleImageRegionIterator<myImageType2D2> myIteratorType2D2;

  myIteratorType2D1 it2D1(inputImage2D,region2D);

  it2D1.Begin();
  int i=0;
  while( !it2D1.IsAtEnd() ) 
  {	if(
       ( i == 40 )
	  ||( i == 55 )
    )	  
  {if ( i == 40 )  it2D1.Set(1);
   if ( i == 55 )  it2D1.Set(2);
	  }
	else it2D1.Set(0);
    ++it2D1;
	i++;
  }

 
  /* Create Danielsson filter */
  typedef itk::ImageDanielssonFilter<myImageType2D1,myImageType2D2> myFilterType2D;
  myFilterType2D::Pointer filter2D = myFilterType2D::New();

  filter2D->SetInputImage( inputImage2D ); 
  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();
  myImageType2D1::Pointer outputClosestPoints2D = filter2D->GetClosestPoints();
  filter2D->SetClosestComputation(true); /* Set that we want the closest points map computed */ 
  filter2D->SetMetric(8);/* Set the metric by default it is 8 for a 2D image */
  filter2D->Execute();


  /* Show Distance map */
  myIteratorType2D2 it2D2(outputDistance2D,outputDistance2D->GetRequestedRegion());

  it2D2.Begin();
  i = 1;
  while( !it2D2.IsAtEnd() ) 
  {
   std::cout << it2D2.Get() << "\t";
   if(i%9 == 0) {
     std::cout << std::endl;
   }
   i++; 
   ++it2D2;
  }

  /* Show Closest Points map */
  std::cout << std::endl ;
  std::cout << "Closest Points Image 2D" << std::endl << std::endl;

  myIteratorType2D1 it2D3(outputClosestPoints2D,outputClosestPoints2D->GetRequestedRegion());

  it2D3.Begin();
  i = 1;
  while( !it2D3.IsAtEnd() ) 
  {
   std::cout << it2D3.Get() <<"\t";
   if(i%9 == 0) {
     std::cout<<std::endl;
   }
   i++; 
   ++it2D3;
  }



  std::cout << std::endl;
  std::cout << "Compute the distance map of a 3x3x3 image" ;
  std::cout << "with two blank points at (1,2,0) and (0,2,3)" << std::endl; 

     
  typedef itk::Image<float, 3>  myImageType3D1;
  typedef itk::Image<float, 3>  myImageType3D2;

  /* Allocate the 3D image */
  myImageType3D1::SizeType size3D = {{3,3,3}};
  myImageType3D1::IndexType index3D = {{0,0,0}};
  myImageType3D1::RegionType region3D;
  region3D.SetSize( size3D );
  region3D.SetIndex( index3D );

  myImageType3D1::Pointer inputImage3D = myImageType3D1::New();
  inputImage3D->SetLargestPossibleRegion( region3D );
  inputImage3D->SetBufferedRegion( region3D );
  inputImage3D->SetRequestedRegion( region3D );
  inputImage3D->Allocate();


  /* Set pixel (1,2,0) with the value 1 
   * and pixel (0,2,3) with the value 2 
   * The Danielsson Distance is performed for each pixel with a value > 0
   * The ClosestPoints computation is based on the value of the pixel.
   */
  typedef  itk::SimpleImageRegionIterator<myImageType3D1> myIteratorType3D1;
  typedef  itk::SimpleImageRegionIterator<myImageType3D2> myIteratorType3D2;

  myIteratorType3D1 it3D1(inputImage3D,region3D);

  it3D1.Begin();
  i=0;
  while( !it3D1.IsAtEnd() ) {
    if( (i == 5) || (i == 20)) {
	  if (i == 5) it3D1.Set(1);
	  if (i == 20) it3D1.Set(2);
	}
	else {
	  it3D1.Set(0);
    }
	
	++it3D1;
	i++;
  }

  /* Create Danielsson filter */
  typedef itk::ImageDanielssonFilter<myImageType3D1,myImageType3D2> myFilterType3D;

  myFilterType3D::Pointer filter3D = myFilterType3D::New();

  filter3D->SetInputImage( inputImage3D );
  myImageType3D2::Pointer outputDistance3D = filter3D->GetOutput();
  myImageType3D1::Pointer outputClosestPoints3D = filter3D->GetClosestPoints();
  filter3D->SetClosestComputation(true);
  filter3D->SetMetric(6);/* Set the metric by default it is 6 for a 3D image */
  filter3D->Execute();

  
  /* Show Distance map */
  myIteratorType3D2 it3D2(outputDistance3D,outputDistance3D->GetRequestedRegion());
  
  it3D2.Begin();
  i = 1;
  while( !it3D2.IsAtEnd() ) 
  { if(i == 1) {
    std::cout << "\n first slice:" << std::endl;
	}
	if(i == 10) {
	  std::cout << "\n second slice:" << std::endl;
	}
	if(i == 19) {
	  std::cout << "\n third slice:" << std::endl;
	}
    std::cout << it3D2.Get() << "\t";
    if(i%3 == 0) {
	  std::cout<<std::endl;
    }
	i++; 
    ++it3D2;
  }

  std::cout<<std::endl<<"Closest Points Image 3D"<<std::endl;
  
  /* Show Closest Points map */
  myIteratorType3D1 it3D3(outputClosestPoints3D,outputClosestPoints3D->GetRequestedRegion()); 

  it3D3.Begin();
  i = 1;
  while( !it3D3.IsAtEnd() ) 
  {
    if(i == 1) {
    std::cout << "\n first slice:" << std::endl;
	}
	if(i == 10) {
	  std::cout << "\n second slice:" << std::endl;
	}
	if(i == 19) {
	  std::cout << "\n third slice:" << std::endl;
	}
    std::cout << it3D3.Get() <<"\t";
    if(i%3 == 0) std::cout<<std::endl;

    i++;  
    ++it3D3;
  }

  return EXIT_SUCCESS;

}




