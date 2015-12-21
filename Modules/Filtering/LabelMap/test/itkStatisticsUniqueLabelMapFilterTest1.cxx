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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"


#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkStatisticsUniqueLabelMapFilter.h"

#include "itkTestingMacros.h"

#include "itkFlatStructuringElement.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"

int itkStatisticsUniqueLabelMapFilterTest1(int argc, char * argv[])
{
  // ToDo: remove dilationOutput once the JIRA issue 3370 has been solved
  // Then, argc != 6
  if( argc != 7 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input feature output dilationOutput";
    std::cerr << " reverseOrdering attribute";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  const char * inputImage = argv[1];
  const char * featureImage = argv[2];
  const char * outputImage = argv[3];
  const char * dilationOutput = argv[4];
  const bool reverseOrdering = atoi( argv[5] );
  const unsigned int attribute = atoi( argv[6] );

  const unsigned int Dimension = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, Dimension >                 ImageType;
  typedef itk::StatisticsLabelObject< PixelType, Dimension > StatisticsLabelObjectType;
  typedef itk::LabelMap< StatisticsLabelObjectType >         LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputImage );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( featureImage );


  // Dilate each label object to form overlapping label objects.
  const unsigned int radiusValue = 5;

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType > LabelImageToLabelMapFilterType;
  LabelImageToLabelMapFilterType::Pointer labelMapConverter = LabelImageToLabelMapFilterType::New();
  labelMapConverter->SetInput( reader->GetOutput() );
  labelMapConverter->SetBackgroundValue( itk::NumericTraits< PixelType >::ZeroValue() );

  typedef itk::FlatStructuringElement< Dimension > StructuringElementType;
  StructuringElementType::RadiusType radius;
  radius.Fill( radiusValue );

  StructuringElementType structuringElement = StructuringElementType::Box( radius );

  typedef itk::GrayscaleDilateImageFilter< ImageType, ImageType, StructuringElementType > MorphologicalFilterType;
  MorphologicalFilterType::Pointer grayscaleDilateFilter = MorphologicalFilterType::New();
  grayscaleDilateFilter->SetInput( reader->GetOutput() );
  grayscaleDilateFilter->SetKernel( structuringElement );

  typedef itk::ObjectByObjectLabelMapFilter< LabelMapType > ObjectByObjectLabelMapFilterType;
  ObjectByObjectLabelMapFilterType::Pointer objectByObjectLabelMapFilter = ObjectByObjectLabelMapFilterType::New();
  objectByObjectLabelMapFilter->SetInput( labelMapConverter->GetOutput() );
  objectByObjectLabelMapFilter->SetBinaryInternalOutput( false );
  objectByObjectLabelMapFilter->SetFilter( grayscaleDilateFilter );

  typedef itk::StatisticsLabelMapFilter< LabelMapType, ImageType > StatisticsFilterType;
  StatisticsFilterType::Pointer statisticsFilter = StatisticsFilterType::New();
  statisticsFilter->SetInput1( objectByObjectLabelMapFilter->GetOutput()  );
  statisticsFilter->SetFeatureImage( reader2->GetOutput() );

  typedef itk::StatisticsUniqueLabelMapFilter< LabelMapType > LabelUniqueType;
  LabelUniqueType::Pointer unique = LabelUniqueType::New();

  //testing boolean macro for ReverseOrdering
  unique->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, unique->GetReverseOrdering() );

  unique->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, unique->GetReverseOrdering() );

  //testing get and set macros for ReverseOrdering
  // ToDo: decrease reverseOrdering argv index once the JIRA issue 3370 has been solved
  // Then, argv[4]
  unique->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , unique->GetReverseOrdering() );


  //testing get and set macros for Attribute
  // ToDo: decrease attribute argv index once the JIRA issue 3370 has been solved
  // Then, argv[5]
  unique->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, unique->GetAttribute() );

  unique->SetInput( statisticsFilter->GetOutput() );

  itk::SimpleFilterWatcher watcher(unique, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> LabelMapToImageFilterType;
  LabelMapToImageFilterType::Pointer labelMapToImageFilter = LabelMapToImageFilterType::New();
  labelMapToImageFilter->SetInput( unique->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( labelMapToImageFilter->GetOutput() );
  writer->SetFileName( outputImage );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  // WARNING: TEMPORARY: JIRA ISSUE 3370
  // Writing an additional output of just the dilated label
  writer->SetInput( grayscaleDilateFilter->GetOutput() );
  writer->SetFileName( dilationOutput );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
