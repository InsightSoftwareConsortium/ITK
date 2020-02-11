/*=========================================================================
 *
 *  Copyright NumFOCUS
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

// itkDeformationFieldJacobianDeterminantFilter.h -> itkDisplacementFieldJacobianDeterminantFilter.h
#include "itkDisplacementFieldJacobianDeterminantFilter.h"

#include "itkTestDriverIncludeRequiredIOFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
  }

  // For now, this program runs on 3D deformation fields
  using InputPixelType = itk::Vector<float, 3>;
  using OutputPixelType = float;

  using InputImageType = itk::Image<InputPixelType, 3>;
  using OutputImageType = itk::Image<OutputPixelType, 3>;

  using ReaderType = itk::ImageFileReader<InputImageType>;

  using FilterType = itk::DisplacementFieldJacobianDeterminantFilter<InputImageType>;

  // Set up deformation field reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Connect deformation-to-Jacobian filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  //  filter->SetUseImageSpacingOn();
  filter->Update();

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  // Write Jacobian determinant image.
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
