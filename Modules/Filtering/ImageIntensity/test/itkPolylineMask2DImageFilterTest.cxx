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

#include "itkPolylineMask2DImageFilter.h"
#include "itkPolyLineParametricPath.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkPolylineMask2DImageFilterTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Error: missing arguments" << '\n';
    std::cerr << "Usage: " << '\n';
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputFilename outputFilename " << '\n';
    return EXIT_FAILURE;
  }

  // Declare the types of the images
  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;
  using InputPolylineType = itk::PolyLineParametricPath<Dimension>;

  // Read input image
  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();

  std::cout << "Input filename = " << argv[1] << '\n';

  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught an unexpected exception. " << '\n';
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }

  const InputImageType::ConstPointer inputImage = reader->GetOutput();

  // Create polyline
  auto inputPolyline = InputPolylineType::New();

  // We expect as input an image of 256 x 256 pixels with spacing 1,1.

  // Initialize the polyline
  using VertexType = InputPolylineType::VertexType;

  // Add vertices to the polyline
  VertexType v0;
  v0[0] = 64.0;
  v0[1] = 128.0;
  inputPolyline->AddVertex(v0);

  VertexType v1;
  v1[0] = 128.0;
  v1[1] = 192.0;
  inputPolyline->AddVertex(v1);

  VertexType v2;
  v2[0] = 192.0;
  v2[1] = 128.0;
  inputPolyline->AddVertex(v2);

  VertexType v3;
  v3[0] = 128.0;
  v3[1] = 64.0;
  inputPolyline->AddVertex(v3);


  // Declare the type for the Mask image filter
  using InputFilterType = itk::PolylineMask2DImageFilter<InputImageType, InputPolylineType, OutputImageType>;


  // Create a mask  Filter
  auto filter = InputFilterType::New();

  // Connect the input image
  filter->SetInput1(inputImage);

  // Connect the Polyline
  filter->SetInput2(inputPolyline);

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();

  std::cout << "Output filename = " << argv[2] << '\n';

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught an unexpected exception. " << '\n';
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "Output image has been saved" << '\n';
  std::cout << '\n';

  // Now cause and exception
  // Put a vertex outside of the image
  VertexType ve;
  ve[0] = 1000.0;
  ve[1] = 128.0;
  inputPolyline->AddVertex(ve);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught an expected exception. " << '\n';
    std::cout << err << '\n';
    return EXIT_SUCCESS;
  }
  return EXIT_FAILURE;
}
