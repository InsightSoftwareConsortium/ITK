/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientVectorFlowImageFilterTest.cxx
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
#include <itkGradientVectorFlowImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkCovariantVector.h>


int itkGradientVectorFlowImageFilterTest(int, char**) 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare gradient type
  typedef itk::CovariantVector<float, myDimension> myGradientType;

  // Declare the types of the images
  typedef itk::Image<myGradientType, myDimension>  myGradientImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  myGradientImageType::Pointer inputImage  = myGradientImageType::New();

  
  // Define their size, and start index
  mySizeType size;
  size[0] = 8;
  size[1] = 8;
  size[2] = 8;

  myIndexType start;
  start = myIndexType::ZeroIndex;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIterator<myGradientImageType>  myIteratorType;

  typedef itk::ImageRegionIterator<
                                 myGradientImageType>  myOutputIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  myGradientType gradientValueA;
  gradientValueA.Fill( 1.5 );

  // Initialize the content of Image A
  while( !it.IsAtEnd() ) 
  {
    it.Set( gradientValueA );
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

  myGradientType gradientValueB;
  gradientValueB.Fill( 2.5 );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() ) 
  {
    itb.Set( gradientValueB );
    ++itb;
  }

  // Declare the type for the 
  typedef itk::GradientVectorFlowImageFilter<
                                      myGradientImageType,
                                      myGradientImageType
                                                   >  myFilterType;
            

  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage ); 

  
  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output 
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

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
  return 0;

}




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientVectorFlowImageFilterTest.cxx
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
#include <itkGradientRecursiveGaussianImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkCovariantVector.h>
#include <itkGradientImageFilter.h>
#include <itkGradientToMagnitudeImageFilter.h>
#include <itkDerivativeImageFilter.h>
#include <itkGradientVectorFlowImageFilter.h>
#include <itkLaplacianImageFilter.h>
#include "itkNullImageToImageFilterDriver.txx"

#define WIDTH   128
#define HEIGHT  128

int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 2;

  // Declare gradient type
  typedef itk::CovariantVector<double, myDimension> myGradientType;

  // Declare the types of the images
  typedef itk::Image<double, myDimension>           myImageType;
  typedef itk::Image<myGradientType, myDimension>   myGradientImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>       myRegionType;

  typedef itk::LaplacianImageFilter<myImageType, myImageType> myLaplacianFilterType;
  typedef itk::GradientVectorFlowImageFilter<myGradientImageType, myGradientImageType>
                                              myGVFFilterType;

  typedef itk::GradientImageFilter<myImageType, double, double>
                                              myGFilterType;

  typedef itk::GradientToMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myGToMFilterType;
  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();
  myImageType::Pointer interImage  = myImageType::New();
  myImageType::Pointer inter1Image  = myImageType::New();
 

//  myGradientType grad_vec;

  // Define their size, and start index
  mySizeType size;
  size[0] = WIDTH;
  size[1] = HEIGHT;

  myIndexType start;
  start = myIndexType::ZeroIndex;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  interImage->SetLargestPossibleRegion( region );
  interImage->SetBufferedRegion( region );
  interImage->SetRequestedRegion( region );
  interImage->Allocate();

  inter1Image->SetLargestPossibleRegion( region );
  inter1Image->SetBufferedRegion( region );
  inter1Image->SetRequestedRegion( region );
  inter1Image->Allocate();

  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                 myGradientImageType>  myOutputIteratorType;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  std::cout << "Input Image initialization " << std::endl;

  while( !it.IsAtEnd() ) 
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 100;
  size[1] = 100;

  start[0] = 14;
  start[1] = 14;

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
  typedef itk::GradientRecursiveGaussianImageFilter<
                                            myImageType,
                                            myGradientImageType,
                                            double       >  myFilterType;
            

  // Create a  Filter                                
  myFilterType::Pointer filter = myFilterType::New();

  myGFilterType::Pointer gfilter = myGFilterType::New();
  myGToMFilterType::Pointer gtomfilter = myGToMFilterType::New();

  // Connect the input images
  filter->SetInput( inputImage ); 

  // Set sigma
  filter->SetSigma( 2.0 );
  
  // Execute the filter
  filter->Update();

  std::cout << "Filter: " << filter;

  myLaplacianFilterType::Pointer m_LFilter = myLaplacianFilterType::New();
  myGVFFilterType::Pointer m_GVFFilter = myGVFFilterType::New();

  m_GVFFilter->SetInput(gfilter->GetOutput());
  m_GVFFilter->SetLaplacianFilter(m_LFilter);
  m_GVFFilter->SetNoiseLevel(500);

  // Get the Smart Pointer to the Filter Output 
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the output image
  myOutputIteratorType itg( outputImage, 
                            outputImage->GetRequestedRegion() );
  
  //  Print the content of the result image
  std::cout << " Result " << std::endl;

  gtomfilter->SetInput(filter->GetOutput());
  gtomfilter->Update();

  gfilter->SetInput(gtomfilter->GetOutput());
  gfilter->Update();

  m_GVFFilter->Update();
  myOutputIteratorType itgvf( m_GVFFilter->GetOutput(), 
                            m_GVFFilter->GetOutput()->GetRequestedRegion() );

  std::cout << "Completed" << std::endl;
  // All objects should be automatically destroyed at this point
  return 0;

}




