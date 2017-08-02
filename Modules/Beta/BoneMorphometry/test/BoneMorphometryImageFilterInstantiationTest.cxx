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
#include "itkBoneMorphometryImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int BoneMorphometryImageFilterInstantiationTest( int argc, char *argv[] )
{
    if( argc < 3 )
      {
      std::cerr << "Missing parameters." << std::endl;
      std::cerr << "Usage: " << argv[0]
        << " inputImageFile"
        << " maskImageFile" << std::endl;
      return EXIT_FAILURE;
      }

  const unsigned int ImageDimension = 3;

  // Declare types
  typedef int                                         InputPixelType;

  typedef itk::Image< InputPixelType, ImageDimension >  InputImageType;
  typedef itk::ImageFileReader< InputImageType >        ReaderType;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName( argv[2] );

  // Create the filter
  typedef itk::BoneMorphometryImageFilter<InputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter,
  BoneMorphometryImageFilter, ImageToImageFilter );

  filter->SetInput( reader->GetOutput() );

  filter->SetMaskImage( maskReader->GetOutput() );
  TEST_SET_GET_VALUE( maskReader->GetOutput(), filter->GetMaskImage() );

  filter->SetThreshold( 1300 );
  TEST_SET_GET_VALUE( 1300, filter->GetThreshold() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  std::cout<<std::endl;
  std::cout<<"BVTV = "<<filter->GetBVTV()<<std::endl;
  std::cout<<std::endl;
  std::cout<<"TbN = "<<filter->GetTbN()<<std::endl;
  std::cout<<std::endl;
  std::cout<<"TbTh = "<<filter->GetTbTh()<<std::endl;
  std::cout<<std::endl;
  std::cout<<"TbSp = "<<filter->GetTbSp()<<std::endl;
  std::cout<<std::endl;
  std::cout<<"BSBV = "<<filter->GetBSBV()<<std::endl;
  \
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
