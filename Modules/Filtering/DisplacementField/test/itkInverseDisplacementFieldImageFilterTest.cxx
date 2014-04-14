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

#include "itkInverseDisplacementFieldImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkInverseDisplacementFieldImageFilterTest( int argc, char * argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int Dimension = 2;
  typedef   float VectorComponentType;

  typedef   itk::Vector< VectorComponentType, Dimension > VectorType;

  typedef itk::Image< VectorType,  Dimension > DisplacementFieldType;

  typedef itk::InverseDisplacementFieldImageFilter<
    DisplacementFieldType,
    DisplacementFieldType
    >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  FilterWatcher watcher(filter);

  // Creating an input displacement field
  DisplacementFieldType::Pointer field = DisplacementFieldType::New();

  DisplacementFieldType::SpacingType spacing;
  spacing.Fill( 1.0 );

  DisplacementFieldType::PointType origin;
  origin.Fill( 0.0 );

  DisplacementFieldType::RegionType region;
  DisplacementFieldType::SizeType   size;
  DisplacementFieldType::IndexType  start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize( size );
  region.SetIndex( start );

  field->SetOrigin( origin );
  field->SetSpacing( spacing );
  field->SetRegions( region );
  field->Allocate();

  VectorType pixelValue;

  itk::ImageRegionIteratorWithIndex< DisplacementFieldType > it( field, region );

  // Fill the field with some vectors
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    DisplacementFieldType::IndexType index = it.GetIndex();
    pixelValue[0] = index[0] * 2.0 - index[0];
    pixelValue[1] = index[1] * 2.0 - index[1];
    it.Set( pixelValue );
    ++it;
    }

  // Since the tested transform is upsampling by a factor of two, the
  // size of the inverse field should be twice the size of the input
  // field. All other geomtry parameters are the same.
  filter->SetOutputSpacing( spacing );

  // keep the origin
  filter->SetOutputOrigin( origin );

  // set the size
  DisplacementFieldType::SizeType invFieldSize;
  invFieldSize[0] = size[0] * 2;
  invFieldSize[1] = size[1] * 2;

  filter->SetSize( invFieldSize );

  filter->SetInput( field );

  filter->SetSubsamplingFactor( 16 );

  try
    {
    filter->UpdateLargestPossibleRegion();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  // Write an image for regression testing
  typedef itk::ImageFileWriter<  DisplacementFieldType  > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput (filter->GetOutput() );
  writer->SetFileName( argv[1] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown by writer" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Now, test for loop invariant (acts as filter validation)
  // f^-1(f(p1) + p1 ) - f(p1)  = 0
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    DisplacementFieldType::PointType p1;
    field->TransformIndexToPhysicalPoint(it.GetIndex(), p1);

    DisplacementFieldType::PixelType fp1 = it.Get();

    DisplacementFieldType::PointType p2;
    p2[0] = p1[0] + fp1[0];
    p2[1] = p1[1] + fp1[1];

    DisplacementFieldType::IndexType id2;
    filter->GetOutput()->TransformPhysicalPointToIndex(p2,id2);
    DisplacementFieldType::PixelType fp2 = filter->GetOutput()->GetPixel(id2);

    if(std::abs(fp2[0] + fp1[0]) > 0.001
       || std::abs(fp2[1] + fp1[1]) > 0.001)
      {
      std::cerr<<"Loop invariant not satisfied for index "<<it.GetIndex()<<" : f^-1(f(p1) + p1 ) + f(p1)  = 0"<<   std::endl;
      std::cerr<<"f(p1) = "<<fp1<<std::endl;
      std::cerr<<"f^-1(f(p1) + p1 ) = "<<fp2<<std::endl;
      std::cerr<<"diff: "<<fp1[0]+fp2[0]<<", "<<fp1[1]+fp2[1]<<std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    }

  return EXIT_SUCCESS;

}
