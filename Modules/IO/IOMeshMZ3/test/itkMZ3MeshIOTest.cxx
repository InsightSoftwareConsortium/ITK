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

#include "itkMZ3MeshIO.h"

#include "itkCommand.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"
#include "itkMesh.h"

int
itkMZ3MeshIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputMesh";
    std::cerr << " outputMesh";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputMeshFileName = argv[1];
  const char * outputMeshFileName = argv[2];

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh<PixelType, Dimension>;


  // ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, MZ3MeshIO, ImageToImageFilter);

  // // Create input image to avoid test dependencies.
  // ImageType::SizeType size;
  // size.Fill(128);
  // ImageType::Pointer image = ImageType::New();
  // image->SetRegions(size);
  // image->Allocate();
  // image->FillBuffer(1.1f);

  // ShowProgress::Pointer showProgress = ShowProgress::New();
  // filter->AddObserver(itk::ProgressEvent(), showProgress);
  // filter->SetInput(image);

  // using WriterType = itk::ImageFileWriter<ImageType>;
  // WriterType::Pointer writer = WriterType::New();
  // writer->SetFileName(outputImageFileName);
  // writer->SetInput(filter->GetOutput());
  // writer->SetUseCompression(true);

  // ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
