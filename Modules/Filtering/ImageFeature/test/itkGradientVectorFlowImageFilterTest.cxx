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

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"

int itkGradientVectorFlowImageFilterTest(int , char *[])
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

  typedef itk::VectorMagnitudeImageFilter<myGradientImageType, myImageType>
                                              myVectorMagnitudeFilterType;
  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();
  myImageType::Pointer interImage  = myImageType::New();
  myImageType::Pointer inter1Image  = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = 128;
  size[1] = 128;

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
                                            myGradientImageType
                                                  >  myFilterType;


  // Create a  Filter
  myFilterType::Pointer filter = myFilterType::New();

  myGFilterType::Pointer gfilter = myGFilterType::New();
  myVectorMagnitudeFilterType::Pointer gtomfilter = myVectorMagnitudeFilterType::New();

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
  m_GVFFilter->SetTimeStep(0.001);
  m_GVFFilter->SetIterationNum(2);

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

  std::cout << m_GVFFilter->GetTimeStep() << std::endl;

  std::cout << m_GVFFilter->GetNoiseLevel() << std::endl;

  std::cout << m_GVFFilter->GetIterationNum() << std::endl;

  myOutputIteratorType itgvf( m_GVFFilter->GetOutput(),
                            m_GVFFilter->GetOutput()->GetRequestedRegion() );

  std::cout << "Completed" << std::endl;
  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
