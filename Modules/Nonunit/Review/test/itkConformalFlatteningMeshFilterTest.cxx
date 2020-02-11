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

#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkConformalFlatteningMeshFilter.h"

int
itkConformalFlatteningMeshFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << argv[0] << "vtkInputFilename vtkOutputFilename mapToSphere[0:1] [polarCellId]\n";

    return EXIT_FAILURE;
  }

  using MeshType = itk::Mesh<double, 3>;

  using FilterType = itk::ConformalFlatteningMeshFilter<MeshType, MeshType>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  using CellIdentifier = MeshType::CellIdentifier;

  //
  // Read mesh file
  //

  std::cout << "Read " << argv[1] << std::endl;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  MeshType::Pointer mesh = reader->GetOutput();

  //
  // Test itkConformalFlatteningMeshFilter
  //

  FilterType::Pointer filter = FilterType::New();

  // Connect the input
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

  // Execute the filter

  std::cout << "Execute the filter" << std::endl;

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer newMesh = filter->GetOutput();

  //
  // Write to file
  //

  std::cout << "Write " << argv[2] << std::endl;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(newMesh);
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
