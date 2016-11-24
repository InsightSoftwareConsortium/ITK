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


#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVotingBinaryHoleFillingImageFilter.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"


int itkVotingBinaryHoleFillingImageFilterTest( int argc, char* argv[] )
{

  if ( argc != 3 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Declare the pixel types of the images
  typedef unsigned short                            InputPixelType;
  typedef unsigned char                             OutputPixelType;
  typedef itk::Image< InputPixelType, Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;
  typedef itk::ImageFileReader< InputImageType >    ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  InputImageType::PixelType foreground = 97; // Prime numbers are good testers
  InputImageType::PixelType background = 29;

  itk::BinaryThresholdImageFilter< InputImageType, InputImageType >::Pointer thresholder =
    itk::BinaryThresholdImageFilter< InputImageType, InputImageType >::New();

  thresholder->SetInput( reader->GetOutput() );
  thresholder->SetLowerThreshold(  30 );
  thresholder->SetUpperThreshold( 100 );
  thresholder->SetInsideValue( foreground );
  thresholder->SetOutsideValue( background );

  // Define the voting binary hole filling filter
  typedef itk::VotingBinaryHoleFillingImageFilter< InputImageType, OutputImageType >
    VotingBinaryHoleFillingImageFilterType;

  VotingBinaryHoleFillingImageFilterType::Pointer voting =
    VotingBinaryHoleFillingImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( voting, VotingBinaryHoleFillingImageFilter,
    VotingBinaryImageFilter );

  // Define the neighborhood size used for the voting filter (3x3)
  InputImageType::SizeType neighRadius;
  neighRadius[0] = 1;
  neighRadius[1] = 1;
  voting->SetRadius( neighRadius );

  // Set the number of pixels over 50% that will tip the decision about
  // switching a pixel
  unsigned int majorityThreshold = 1;
  voting->SetMajorityThreshold( majorityThreshold );
  TEST_SET_GET_VALUE( majorityThreshold, voting->GetMajorityThreshold() );

  voting->SetForegroundValue( foreground );
  voting->SetBackgroundValue( background );

  voting->SetInput( thresholder->GetOutput());

  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION( voting->Update() );


  std::cout << "Number of pixels changed: "
    << voting->GetNumberOfPixelsChanged() << std::endl;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( voting->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
