/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPermuteAxesImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPermuteAxesImageFilter.h"
#include "itkImage.h"
#include "itkTextOutput.h"
#include "itkCommand.h"
#include "itkImageRegionIteratorWithIndex.h"


// The following classe is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

int itkPermuteAxesImageFilterTest(int, char* [] )
{

  itk::OutputWindow::SetInstance( itk::TextOutput::New() );

  typedef unsigned char PixelType;
  enum { ImageDimension = 4 };
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::PermuteAxesImageFilter<ImageType> PermuterType;


  // define a small input test
  ImageType::IndexType index = {{ 10, 20, 30, 40 }};
  ImageType::SizeType size = {{5,4,3,2}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  double spacing[ImageDimension] = { 1.1, 1.2, 1.3, 1.4 };
  double origin[ImageDimension] = { 0.5, 0.4, 0.3, 0.2 };

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->Allocate();

  inputImage->SetSpacing( spacing );
  inputImage->SetOrigin( origin );
 
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inputIter( inputImage, inputImage->GetBufferedRegion() );

  PixelType counter = 0;
  while ( !inputIter.IsAtEnd() )
    {
    inputIter.Set( counter );
    ++counter;
    ++inputIter;
    }
  

  // permute the image
  PermuterType::Pointer permuter = PermuterType::New();

  unsigned int order[ImageDimension] = { 2, 0, 3, 1 };
  permuter->SetOrder( order );

  permuter->SetInput( inputImage );

  ShowProgressObject progressWatch(permuter);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  permuter->AddObserver( itk::ProgressEvent(), command);

  permuter->Update();

  permuter->Print( std::cout );

  // check the output
  ImageType::Pointer outputImage = permuter->GetOutput();

  inputIter.GoToBegin();
  bool passed = true;
  while ( !inputIter.IsAtEnd() )
    {

    ImageType::IndexType inputIndex = inputIter.GetIndex();
    ImageType::IndexType outputIndex;

    for ( int j = 0; j < ImageDimension; j++ )
      {
      outputIndex[j] = inputIndex[order[j]];
      }

    if ( inputIter.Get() != outputImage->GetPixel( outputIndex ) )
      {
      passed = false;
      std::cout << "Mismatch at index: " << inputIndex << std::endl;
      }

    ++inputIter;

    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // test SetOrder logic - repeating indices
  passed = false;
  order[0] = order[1];
  try
    {
    permuter->SetOrder( order );
    }
  catch( itk::ExceptionObject & err )
    {
    passed = true;
    std::cout << err << std::endl;
    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // test SetOrder logic - indices out of range
  passed = false;
  order[0] = ImageDimension + 2;
  try
    {
    permuter->SetOrder( order );
    }
  catch( itk::ExceptionObject & err )
    {
    passed = true;
    std::cout << err << std::endl;
    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
