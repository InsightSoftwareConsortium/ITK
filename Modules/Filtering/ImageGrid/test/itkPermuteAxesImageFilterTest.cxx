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

#include "itkPermuteAxesImageFilter.h"
#include "itkTextOutput.h"
#include "itkCommand.h"


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
  typedef itk::Image<PixelType,ImageDimension>   ImageType;
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

  permuter->GetOutput()->Print( std::cout );
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

  // exercise Get methods
  std::cout << "Order: " << permuter->GetOrder() << std::endl;
  std::cout << "InverseOrder: " << permuter->GetInverseOrder() << std::endl;

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
