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
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

#include "itkBorderQuadEdgeMeshFilter.h"
#include "itkTestingMacros.h"

int
itkBorderQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename borderType (0: SQUARE; 1: DISK)" << std::endl;
    std::cerr << " borderPick (0: LONGEST; 1: LARGEST)" << std::endl;
    std::cerr << " outputFilename" << std::endl;
    return EXIT_FAILURE;
  }


  // ** TYPEDEF **
  using Coord = double;

  using MeshType = itk::QuadEdgeMesh<Coord, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;
  using BorderTransformType = itk::BorderQuadEdgeMeshFilter<MeshType, MeshType>;


  // ** READ THE FILE IN **
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  MeshType::Pointer mesh = reader->GetOutput();

  // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
  auto border_transform = BorderTransformType::New();
  border_transform->SetInput(mesh);
  // two following line for coverage
  border_transform->SetRadius(border_transform->GetRadius());
  border_transform->GetNameOfClass();

  int border = std::stoi(argv[2]);
  switch (border) // choose border type
  {
    case 0: // square shaped domain
      border_transform->SetTransformType(BorderTransformType::BorderTransformEnum::SQUARE_BORDER_TRANSFORM);
      break;
    case 1: // disk shaped domain
      border_transform->SetTransformType(BorderTransformType::BorderTransformEnum::DISK_BORDER_TRANSFORM);
      break;
    default: // handle .... user ....
      std::cerr << "2nd argument must be " << std::endl;
      std::cerr << "0 for SQUARE BORDER TRANSFORM or 1 for DISK BORDER TRANSFORM" << std::endl;
      return EXIT_FAILURE;
  }
  std::cout << "Transform type is: " << border_transform->GetTransformType();
  std::cout << std::endl;

  int pick = std::stoi(argv[3]);
  switch (pick)
  {
    case 0:
      border_transform->SetBorderPick(BorderTransformType::BorderPickEnum::LONGEST);
      break;
    case 1:
      border_transform->SetBorderPick(BorderTransformType::BorderPickEnum::LARGEST);
      break;
    default: // handle .... user ....
      std::cerr << "3rd argument must be " << std::endl;
      std::cerr << "0 for LONGEST BORDER or 1 for LARGEST BORDER" << std::endl;
      return EXIT_FAILURE;
  }
  std::cout << "Border picked is: " << border_transform->GetBorderPick();
  std::cout << std::endl;

  MeshType::Pointer output = border_transform->GetOutput();

  // ** WRITE OUTPUT **
  auto writer = WriterType::New();
  writer->SetInput(border_transform->GetOutput());
  writer->SetFileName(argv[4]);
  writer->Update();

  // ** PRINT **
  std::cout << "BorderTransform: \n" << border_transform;

  // Test streaming enumeration for BorderQuadEdgeMeshFilterEnums::BorderTransform elements
  const std::set<itk::BorderQuadEdgeMeshFilterEnums::BorderTransform> allBorderTransform{
    itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::SQUARE_BORDER_TRANSFORM,
    itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::DISK_BORDER_TRANSFORM
  };
  for (const auto & ee : allBorderTransform)
  {
    std::cout << "STREAMED ENUM VALUE BorderQuadEdgeMeshFilterEnums::BorderTransform: " << ee << std::endl;
  }

  // Test streaming enumeration for BorderQuadEdgeMeshFilterEnums::BorderPick elements
  const std::set<itk::BorderQuadEdgeMeshFilterEnums::BorderPick> allBorderPick{
    itk::BorderQuadEdgeMeshFilterEnums::BorderPick::LONGEST, itk::BorderQuadEdgeMeshFilterEnums::BorderPick::LARGEST
  };
  for (const auto & ee : allBorderPick)
  {
    std::cout << "STREAMED ENUM VALUE BorderQuadEdgeMeshFilterEnums::BorderPick: " << ee << std::endl;
  }
  // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
  return EXIT_SUCCESS;
}
