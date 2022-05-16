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
#include "itkConformalFlatteningMeshFilter.h"
#include "itkTestingMacros.h"

int
itkConformalFlatteningMeshFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "vtkInputFilename vtkOutputFilename mapToSphere[0:1] [polarCellId]" << std::endl;
    return EXIT_FAILURE;
  }

  using MeshType = itk::Mesh<double, 3>;

  using FilterType = itk::ConformalFlatteningMeshFilter<MeshType, MeshType>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  using CellIdentifier = MeshType::CellIdentifier;

  // Read mesh file
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  MeshType::Pointer mesh = reader->GetOutput();

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ConformalFlatteningMeshFilter, MeshToMeshFilter);


  filter->SetInput(mesh);

  CellIdentifier polarCellId = 0; // default set to the first cell

  if (argc > 4)
  {
    polarCellId = std::stoi(argv[4]);
  }

  filter->SetPolarCellIdentifier(polarCellId);

  int mapToSphere = std::stoi(argv[3]);

  if (mapToSphere == 1)
  {
    filter->MapToSphere();
  }
  else
  {
    filter->MapToPlane();
  }

  //  double scale = std::stod( argv[4] );
  //  filter->SetScale( scale );


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer newMesh = filter->GetOutput();

  // Write to file
  auto writer = WriterType::New();
  writer->SetInput(newMesh);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
