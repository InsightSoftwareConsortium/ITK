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
#include "itkQuadEdgeMesh.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"


#include "itkQuadEdgeMeshDecimationCriteria.h"
#include "itkSquaredEdgeLengthDecimationQuadEdgeMeshFilter.h"

int
itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc < 3)
  {
    std::cout << "Requires 3 argument: " << std::endl;
    std::cout << "1-Input file name " << std::endl;
    std::cout << "2-Number of Faces " << std::endl;
    std::cout << "3-Output file name " << std::endl;
    return EXIT_FAILURE;
  }

  // ** TYPEDEF **
  using CoordType = double;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<CoordType, Dimension>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;

  // ** READ THE FILE IN **
  ReaderType::Pointer reader = ReaderType::New();
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

  for (auto it = mesh->GetCells()->Begin(); it != mesh->GetCells()->End(); ++it)
  {
    mesh->SetCellData(it.Index(), 25);
  }
  itkAssertOrThrowMacro(mesh->GetNumberOfCells() == mesh->GetCellData()->Size(),
                        "Incorrect number of elements in the cell data array.");

  using CriterionType = itk::NumberOfFacesCriterion<MeshType>;

  using DecimationType = itk::SquaredEdgeLengthDecimationQuadEdgeMeshFilter<MeshType, MeshType, CriterionType>;

  long              N;
  std::stringstream ssout(argv[2]);
  ssout >> N;

  CriterionType::Pointer criterion = CriterionType::New();
  criterion->SetTopologicalChange(true);
  criterion->SetNumberOfElements(N);

  DecimationType::Pointer decimate = DecimationType::New();
  decimate->SetInput(mesh);
  decimate->SetCriterion(criterion);
  decimate->Update();

  itkAssertOrThrowMacro(decimate->GetOutput()->GetNumberOfCells() == decimate->GetOutput()->GetCellData()->Size(),
                        "Incorrect number of elements in the cell data array.");

  // ** WRITE OUTPUT **
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(decimate->GetOutput());
  writer->SetFileName(argv[3]);
  writer->Update();

  return EXIT_SUCCESS;
}
