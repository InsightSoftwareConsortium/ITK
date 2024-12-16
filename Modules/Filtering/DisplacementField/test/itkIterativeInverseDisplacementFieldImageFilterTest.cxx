/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage numberOfIterations stopValue" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  using FilterType = itk::IterativeInverseDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, IterativeInverseDisplacementFieldImageFilter, ImageToImageFilter);


  const itk::SimpleFilterWatcher watcher(filter);

  auto numberOfIterations = static_cast<unsigned int>(std::stoi(argv[2]));
  filter->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, filter->GetNumberOfIterations());

  auto stopValue = std::stod(argv[3]);
  filter->SetStopValue(stopValue);
  ITK_TEST_SET_GET_VALUE(stopValue, filter->GetStopValue());


  // Creating an input displacement field
  auto field = DisplacementFieldType::New();

  DisplacementFieldType::SizeType size;
  size[0] = 128;
  size[1] = 128;

  DisplacementFieldType::IndexType start;
  start[0] = 0;
  start[1] = 0;

  const DisplacementFieldType::RegionType region{ start, size };
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

  auto writer = WriterType::New();

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
