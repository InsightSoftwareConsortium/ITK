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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkAutoCropLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkAutoCropLabelMapFilterTest1( int argc, char * argv [] )
{

  if( argc != 6 )
    {
    std::cerr << "usage: " << argv[0];
    std::cerr << " inputLabelImage outputLabelImage inputBackgroundValue sizeX sizeY" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  typedef unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::LabelObject< PixelType, Dimension > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >         LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType > ImageToLabelMapFilterType;
  ImageToLabelMapFilterType::Pointer imageToLabelMapFilter =
    ImageToLabelMapFilterType::New();
  imageToLabelMapFilter->SetInput( reader->GetOutput() );

  PixelType backgroundValue = atoi( argv[3] );

  imageToLabelMapFilter->SetBackgroundValue( backgroundValue );

  typedef itk::AutoCropLabelMapFilter< LabelMapType > AutoCropLabelMapFilterType;
  AutoCropLabelMapFilterType::Pointer autoCropFilter = AutoCropLabelMapFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( autoCropFilter, AutoCropLabelMapFilter,
    ChangeRegionLabelMapFilter );

  autoCropFilter->SetInput( imageToLabelMapFilter->GetOutput() );

  AutoCropLabelMapFilterType::SizeType size;
  size[0] = atoi( argv[4] );
  size[1] = atoi( argv[5] );
  autoCropFilter->SetCropBorder( size );
  TEST_SET_GET_VALUE( size, autoCropFilter->GetCropBorder() );

  itk::SimpleFilterWatcher watcher(autoCropFilter, "AutoCropLabelMapFilter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType>
    LabelMapToLabelImageFilterType;
  LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter =
    LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput( autoCropFilter->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( labelMapToLabelImageFilter->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
