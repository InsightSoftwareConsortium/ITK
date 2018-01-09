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
#include "itkPadLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkPadLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 5 )
    {
    std::cerr << "usage: " << argv[0] << " input output size0 size1" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef itk::Image< unsigned char, dim > ImageType;

  typedef itk::LabelObject< unsigned char, dim >    LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >          LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::PadLabelMapFilter< LabelMapType > PadLabelMapFilterType;
  PadLabelMapFilterType::Pointer padLabelMapFilter = PadLabelMapFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( padLabelMapFilter, PadLabelMapFilter, ChangeRegionLabelMapFilter );

  PadLabelMapFilterType::SizeType upperBoundaryPadSize = {{0}};
  padLabelMapFilter->SetPadSize( upperBoundaryPadSize );
  TEST_SET_GET_VALUE( upperBoundaryPadSize, padLabelMapFilter->GetUpperBoundaryPadSize() );

  PadLabelMapFilterType::SizeType lowerBoundaryPadSize = {{0}};
  padLabelMapFilter->SetPadSize( lowerBoundaryPadSize );
  TEST_SET_GET_VALUE( upperBoundaryPadSize, padLabelMapFilter->GetLowerBoundaryPadSize() );

  padLabelMapFilter->SetInput( i2l->GetOutput() );
  PadLabelMapFilterType::SizeType size;
  size[0] = atoi( argv[3] );
  size[1] = atoi( argv[4] );

  padLabelMapFilter->SetPadSize( size );
  TEST_SET_GET_VALUE( size, padLabelMapFilter->GetLowerBoundaryPadSize() );
  TEST_SET_GET_VALUE( size, padLabelMapFilter->GetUpperBoundaryPadSize() );

  itk::SimpleFilterWatcher watcher( padLabelMapFilter, "filter" );

  TRY_EXPECT_NO_EXCEPTION( padLabelMapFilter->Update() );

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();

  l2i->SetInput( padLabelMapFilter->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  l2i->SetInput( nullptr );
  TRY_EXPECT_EXCEPTION( l2i->Update() );

  return EXIT_SUCCESS;
}
