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

#include "itkQuadEdgeMesh.h"
#include "itkSTLMeshIOFactory.h"
#include "itkSTLMeshIO.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"

int
itkSTLMeshIOTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << "inputMesh outputMesh (0:ASCII/1:BINARY) " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using PixelType = float;

  using QEMeshType = itk::QuadEdgeMesh<PixelType, Dimension>;

  itk::STLMeshIOFactory::RegisterOneFactory();

  using ReaderType = itk::MeshFileReader<QEMeshType>;
  using WriterType = itk::MeshFileWriter<QEMeshType>;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  int fileMode = atoi(argv[3]);

  if (fileMode == 0)
  {
    writer->SetFileTypeAsASCII();
  }
  else if (fileMode == 1)
  {
    writer->SetFileTypeAsBINARY();
  }

  reader->Update();
  QEMeshType * mesh = reader->GetOutput();

  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  //
  //  Exercising additional methods
  //
  itk::STLMeshIO::Pointer meshIO = itk::STLMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(meshIO, STLMeshIO, MeshIOBase);

  mesh->Print(std::cout);
  reader->GetMeshIO()->Print(std::cout);
  writer->GetMeshIO()->Print(std::cout);

  //
  //  Report the System Endianness
  //
  std::cout << std::endl;
  if (itk::ByteSwapper<int>::SystemIsLittleEndian())
  {
    std::cout << "This system is Little Endian" << std::endl;
  }
  else
  {
    std::cout << "This system is Big Endian" << std::endl;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
