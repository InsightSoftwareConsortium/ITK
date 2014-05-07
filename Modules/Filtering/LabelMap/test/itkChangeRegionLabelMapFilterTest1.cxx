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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkChangeRegionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkChangeRegionLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 7 )
    {
    std::cerr << "usage: " << argv[0] << " input output idx0 idx1 size0 size1" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef itk::Image< unsigned char, dim > ImageType;

  typedef itk::LabelObject< unsigned char, dim > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >       LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::ChangeRegionLabelMapFilter< LabelMapType > ChangeType;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput( i2l->GetOutput() );
  ChangeType::IndexType idx;
  idx[0] = atoi( argv[3] );
  idx[1] = atoi( argv[4] );
  ChangeType::SizeType size;
  size[0] = atoi( argv[5] );
  size[1] = atoi( argv[6] );
  ChangeType::RegionType region;
  region.SetSize( size );
  region.SetIndex( idx );
  change->SetRegion( region );
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

  change->SetInput( ITK_NULLPTR );
  TRY_EXPECT_EXCEPTION( change->Update() );

  return EXIT_SUCCESS;
}
