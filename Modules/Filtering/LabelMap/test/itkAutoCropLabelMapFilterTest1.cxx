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

  const unsigned int dim = 2;
  typedef uint8_t   PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::LabelObject< PixelType, dim > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >   LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  PixelType backgroundValue = atoi( argv[3] );

  i2l->SetBackgroundValue( backgroundValue );

  typedef itk::AutoCropLabelMapFilter< LabelMapType > ChangeType;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput( i2l->GetOutput() );

  ChangeType::SizeType size;
  size[0] = atoi( argv[4] );
  size[1] = atoi( argv[5] );
  change->SetCropBorder( size );
  TEST_SET_GET_VALUE( size, change->GetCropBorder() );

  itk::SimpleFilterWatcher watcher6(change, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( change->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  typedef ChangeType::IndexType             IndexType;
  typedef ChangeType::InputImageRegionType  InputImageRegionType;


  const InputImageRegionType & cropRegion = change->GetRegion();
  const IndexType & minIndex = cropRegion.GetIndex();
  const IndexType & maxIndex = cropRegion.GetUpperIndex();

  std::cout << "GetMinIndex() = " << minIndex << std::endl;
  std::cout << "GetMaxIndex() = " << maxIndex << std::endl;
  std::cout << "GetRegion() = " << cropRegion << std::endl;

  return EXIT_SUCCESS;
}
