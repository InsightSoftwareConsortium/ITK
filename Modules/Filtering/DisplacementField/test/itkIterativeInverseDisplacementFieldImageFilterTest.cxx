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

#include "itkIterativeInverseDisplacementFieldImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkIterativeInverseDisplacementFieldImageFilterTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  using FilterType = itk::IterativeInverseDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>;

  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  // Creating an input displacement field
  DisplacementFieldType::Pointer field = DisplacementFieldType::New();

  DisplacementFieldType::SpacingType spacing;
  spacing.Fill(1.0);

  DisplacementFieldType::PointType origin;
  origin.Fill(0.0);

  DisplacementFieldType::RegionType region;
  DisplacementFieldType::SizeType   size;
  DisplacementFieldType::IndexType  start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize(size);
  region.SetIndex(start);


  field->SetOrigin(origin);
  field->SetSpacing(spacing);
  field->SetRegions(region);
  field->Allocate();

  VectorType pixelValue;

  itk::ImageRegionIteratorWithIndex<DisplacementFieldType> it(field, region);

  // Fill the field with some vectors
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    DisplacementFieldType::IndexType index = it.GetIndex();
    pixelValue[0] = index[0] * 2.0;
    pixelValue[1] = index[1] * 2.0;
    it.Set(pixelValue);
    ++it;
  }

  // Use the same geometry for the inverse field.
  // This is for simplicity here, in general a
  // different geometry should be used.
  // filter->SetOutputSpacing( spacing );


  // keep the origin
  // filter->SetOutputOrigin( origin );

  // set the size
  // filter->SetSize( size );


  filter->SetInput(field);

  try
  {
    filter->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Write an image for regression testing
  using WriterType = itk::ImageFileWriter<DisplacementFieldType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[1]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown by writer" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
