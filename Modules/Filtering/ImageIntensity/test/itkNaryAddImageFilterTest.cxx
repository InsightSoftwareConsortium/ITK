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

#include "itkNaryAddImageFilter.h"
#include "itkImageRegionIterator.h"
#include <iostream>


// Function for image initialization
template <typename ImageType>
void InitializeImage( ImageType * image, const typename ImageType::PixelType & value   )
{
  typename ImageType::Pointer inputImage( image );

  // Define their size, and start index
  typename ImageType::SizeType size;
  size.Fill(2);

  typename ImageType::IndexType start;
  start.Fill(0);

  typename ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  typename itk::ImageRegionIterator<ImageType> it(
     inputImage, inputImage->GetRequestedRegion() );

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( value );
    ++it;
    }
}


// Function for image printing
template <typename ImageType>
void PrintImage( ImageType * image, const char * text)
{
  typename ImageType::Pointer inputImage( image );

  // Create an iterator for going through the image
  typename itk::ImageRegionIterator<ImageType> it(
     inputImage, inputImage->GetRequestedRegion() );

  it.GoToBegin();
  //  Print the content of the image
  std::cout << text << std::endl;
  while( !it.IsAtEnd() )
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }

}


int itkNaryAddImageFilterTest(int, char* [] )
{
  bool testpassed = true;

  // Create some images
  typedef itk::Image<float, 3>  InputImageType;
  InputImageType::Pointer inputImageA  = InputImageType::New();
  InputImageType::Pointer inputImageB  = InputImageType::New();
  InputImageType::Pointer inputImageC  = InputImageType::New();

  InitializeImage<InputImageType>( inputImageA, 12 );
  InitializeImage<InputImageType>( inputImageB, 17 );
  InitializeImage<InputImageType>( inputImageC, -4 );

  PrintImage<InputImageType>( inputImageA, "Input image A" );
  PrintImage<InputImageType>( inputImageB, "Input image B" );
  PrintImage<InputImageType>( inputImageC, "Input image C" );

  // Create an ADD Filter
  typedef itk::Image<float, 3>  OutputImageType;
  typedef itk::NaryAddImageFilter<
                              InputImageType,
                              OutputImageType  >  AdderType;
  AdderType::Pointer filter = AdderType::New();


  // Connect the input images
  filter->SetInput( 0, inputImageA );
  filter->SetInput( 1, inputImageB );
  filter->SetInput( 2, inputImageC );

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();
  filter->SetFunctor(filter->GetFunctor());

  PrintImage<OutputImageType>( outputImage, "Resulting image" );


  // Test the validity of the output
  typedef itk::ImageRegionConstIterator<InputImageType>  IteratorIn;
  typedef itk::ImageRegionConstIterator<OutputImageType> IteratorOut;

  IteratorIn  iterA( inputImageA, inputImageA->GetRequestedRegion() );
  IteratorIn  iterB( inputImageB, inputImageA->GetRequestedRegion() );
  IteratorIn  iterC( inputImageC, inputImageA->GetRequestedRegion() );
  IteratorOut iterO( outputImage, inputImageA->GetRequestedRegion() );

  const double epsilon = 1e-9;
  unsigned int failures = 0;
  while ( !iterO.IsAtEnd() )
    {
    if ( vcl_abs( iterO.Get() - (iterA.Get() + iterB.Get() + iterC.Get()) ) > epsilon ) ++failures;
    ++iterA;
    ++iterB;
    ++iterC;
    ++iterO;
    }

  if ( failures > 0 )
    {
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testpassed = false;
    }


  // Execute the filter in place
  filter->InPlaceOn();
  filter->Update();

  PrintImage<OutputImageType>( outputImage, "Resulting image" );


  // Test the validity of the output
  IteratorOut iterO2( outputImage, inputImageA->GetRequestedRegion() );
  failures = 0;
  while ( !iterO2.IsAtEnd() )
    {
    // Here we cannot test using the input iterators anymore since
    // inputImageA should have been overwritten
    if ( vcl_abs( iterO2.Get() - (12+17-4) ) > epsilon ) ++failures;
    ++iterO2;
    }

  if ( failures > 0 )
    {
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testpassed = false;
    }


  // Testing with vector Images

  // Create some images
  typedef itk::Vector<int,2>                 VectorPixelType;
  typedef itk::Image< VectorPixelType, 2>    VectorImageType;
  VectorImageType::Pointer vectorImageA  = VectorImageType::New();
  VectorImageType::Pointer vectorImageB  = VectorImageType::New();
  VectorImageType::Pointer vectorImageC  = VectorImageType::New();

  VectorPixelType va, vb, vc;
  va.Fill(12);
  va[0] = 5;
  vb.Fill(17);
  vb[0] = 9;
  vc.Fill(-4);
  vc[0] = -80;
  InitializeImage<VectorImageType>( vectorImageA, va );
  InitializeImage<VectorImageType>( vectorImageB, vb );
  InitializeImage<VectorImageType>( vectorImageC, vc );

  PrintImage<VectorImageType>( vectorImageA, "Input image A" );
  PrintImage<VectorImageType>( vectorImageB, "Input image B" );
  PrintImage<VectorImageType>( vectorImageC, "Input image C" );

  // Create an ADD Filter
  typedef itk::NaryAddImageFilter<
                              VectorImageType,
                              VectorImageType  >  VectorAdderType;
  VectorAdderType::Pointer vfilter = VectorAdderType::New();


  // Connect the input images
  vfilter->SetInput( 0, vectorImageA );
  vfilter->SetInput( 1, vectorImageB );
  vfilter->SetInput( 2, vectorImageC );

  // Get the Smart Pointer to the Filter Output
  VectorImageType::Pointer voutputImage = vfilter->GetOutput();


  // Execute the filter
  vfilter->Update();

  PrintImage<VectorImageType>( voutputImage, "Resulting image" );


  // Test the validity of the output
  typedef itk::ImageRegionConstIterator<VectorImageType>  VectorIterator;

  VectorIterator viterA( vectorImageA, vectorImageA->GetRequestedRegion() );
  VectorIterator viterB( vectorImageB, vectorImageA->GetRequestedRegion() );
  VectorIterator viterC( vectorImageC, vectorImageA->GetRequestedRegion() );
  VectorIterator viterO( voutputImage, vectorImageA->GetRequestedRegion() );

  failures = 0;
  while ( !viterO.IsAtEnd() )
    {
    if ( viterO.Get() != (viterA.Get() + viterB.Get() + viterC.Get()) ) ++failures;
    ++viterA;
    ++viterB;
    ++viterC;
    ++viterO;
    }

  if ( failures > 0 )
    {
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testpassed = false;
    }

  if ( !testpassed ) return EXIT_FAILURE;

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
