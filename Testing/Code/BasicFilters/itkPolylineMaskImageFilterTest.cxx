/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <itkImage.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkPolylineMaskImageFilter.h>
#include <itkPolyLineParametricPath.h>

int itkPolylineMaskImageFilterTest(int, char* [] ) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  //Define the dimension of the polyline
  const unsigned int pDimension = 2;

  // Declare the types of the images
  typedef itk::Image<unsigned short, myDimension>     inputImageType;
  typedef itk::Image<unsigned short, myDimension>     outputImageType;
  typedef itk::Vector<unsigned short, myDimension>    inputVectorType;
  typedef itk::PolyLineParametricPath<pDimension>     inputPolylineType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         inputIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>          inputSizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>   inputRegionType;

  // Create images
  inputImageType::Pointer inputImage    = inputImageType::New();
  outputImageType::Pointer outputImage  = outputImageType::New();

  // Create vector
  inputVectorType*   inputVector     = new inputVectorType();

  // Create polyline
  inputPolylineType::Pointer inputPolyline   = inputPolylineType::New();

  // Define their size, and start index
  inputSizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  inputIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  inputRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();


  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<inputImageType>  inputIteratorType;


  // Create one iterator for Image A (this is a light object)
  inputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  while( !it.IsAtEnd() ) 
  {
    it.Set( 255 );
    std::cout << it.Get() << std::endl;
    ++it;
  }

  // Declare the type for the Mask image filter
  typedef itk::PolylineMaskImageFilter<
                           inputImageType, inputPolylineType,   
                           inputVectorType,
                           outputImageType  >     inputFilterType;
            

  // Create a mask  Filter                                
  inputFilterType::Pointer filter = inputFilterType::New();

  // Connect the input images
  // filter->SetImageInput    ( inputImage ); 
  // filter->SetPolylineInput ( inputPolyline ); 
  // filter->SetVectorInput   ( inputVector );

  // Get the Smart Pointer to the Filter Output 
  // outputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  // filter->Update();

  // All objects should be automatically destroyed at this point
  return 0;

}




