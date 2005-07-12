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
  const unsigned int iDimension = 2;

  //Define the dimension of the polyline
  const unsigned int pDimension = 2;

  //Define the dimension of the viewing direction
  const unsigned int vDimension = 2;

  // Declare the types of the images
  typedef itk::Image<unsigned short, iDimension>     inputImageType;
  typedef itk::Image<unsigned short, iDimension>     outputImageType;
  typedef itk::Vector<unsigned short, vDimension>    inputVectorType;
  typedef itk::PolyLineParametricPath<pDimension>     inputPolylineType;

  // Declare the type of the index to access images
  typedef itk::Index<iDimension>         inputIndexType;

  // Declare the type of the size 
  typedef itk::Size<iDimension>          inputSizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<iDimension>   inputRegionType;

  // Create images
  inputImageType::Pointer inputImage    = inputImageType::New();
  outputImageType::Pointer outputImage  = outputImageType::New();

  // Create vector
  inputVectorType   inputVector;

  // Create polyline
  inputPolylineType::Pointer inputPolyline   = inputPolylineType::New();

  // Define their size, and start index
  inputSizeType size;
  size[0] = 512;
  size[1] = 512;


  inputIndexType start;
  start[0] = 0;
  start[1] = 0;


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
    ++it;
  }

  std::cout << "Second operand " << std::endl;
  // Initialize the polyline 
  typedef inputPolylineType::VertexType VertexType;
  
    
  // Add vertices to the polyline
  VertexType v;

  v[0] = 0;
  v[1] = 256;
  inputPolyline->AddVertex(v);
  
  v[0] = 256;
  v[1] = 256;
  inputPolyline->AddVertex(v);
  
  v[0] = 256;
  v[1] = 0;
  inputPolyline->AddVertex(v);
  
  
  std::cout << "Third operand " << std::endl;

  // Initialize the viewing direction
  inputVector[0] = 10;
  inputVector[1] = 10;

  std::cout<< "Define the filter type.....................\n";
  // Declare the type for the Mask image filter
  typedef itk::PolylineMaskImageFilter<
                           inputImageType, inputPolylineType,   
                           inputVectorType,
                           outputImageType  >     inputFilterType;
            

  std::cout<< "Generate the filter....................." << std::endl;
  // Create a mask  Filter                                

  inputFilterType::Pointer filter = inputFilterType::New();

  // Connect the input image
  filter->SetInput    ( inputImage ); 
 
  // Connect the Polyline 
  filter->SetInput    ( inputPolyline ); 

  // Connect the Viewing direction vector
  filter->SetVector   ( inputVector );

  inputVectorType vtest ;
  
  vtest = filter->GetVector();

  std::cout<<"Vector direction:"<<vtest[0]<<","<<vtest[1]<<"\n";  

  // Get the Smart Pointer to the Filter Output 
  // outputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  // filter->Update();

  // All objects should be automatically destroyed at this point
  return 0;

}




