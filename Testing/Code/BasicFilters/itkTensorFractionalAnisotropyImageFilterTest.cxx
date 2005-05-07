/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTensorFractionalAnisotropyImageFilterTest.cxx
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
#include <itkHessianRecursiveGaussianImageFilter.h>
#include <itkTensorFractionalAnisotropyImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkDiffusionTensor3D.h>


int itkTensorFractionalAnisotropyImageFilterTest(int, char* [] ) 
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

  // Declare the type for the tensor pixel and tensor image.
  typedef itk::DiffusionTensor3D< double > myTensorPixelType;
  typedef itk::Image< myTensorPixelType, myDimension > myDTIImageType;
  typedef myTensorPixelType::RealValueType myRealValueType;

  // Declare the type for the image generator
  typedef itk::HessianRecursiveGaussianImageFilter< 
                                            myImageType, 
                                            myDTIImageType >  myFilterType;
            
  typedef itk::Image< myRealValueType, myDimension > myFaImageType;


  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 

  // Select the value of Sigma
  filter->SetSigma( 2.5 ); 

  
  typedef itk::TensorFractionalAnisotropyImageFilter< 
                                                  myDTIImageType,
                                                  myFaImageType 
                                                        > FAFilterType;

  FAFilterType::Pointer fractionalAnisotropyFilter = FAFilterType::New();
    
  fractionalAnisotropyFilter->SetInput( filter->GetOutput() );

  // Execute the filter
  fractionalAnisotropyFilter->Update();

  // Get the Smart Pointer to the Filter Output 
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myFaImageType::Pointer outputImage = fractionalAnisotropyFilter->GetOutput();

  // Declare Iterator type for the output image
  typedef itk::ImageRegionIteratorWithIndex<
                                 myFaImageType>  myOutputIteratorType;

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage, 
                            outputImage->GetRequestedRegion() );
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while( !itg.IsAtEnd() ) 
    {
    std::cout << itg.Get();
    ++itg;
    }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}




