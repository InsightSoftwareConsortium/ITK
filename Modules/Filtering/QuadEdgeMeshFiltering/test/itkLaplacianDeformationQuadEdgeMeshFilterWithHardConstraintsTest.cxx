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

#include "itkQuadEdgeMesh.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraints.h"
#include "VNLSparseLUSolverTraits.h"

int
itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraintsTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc != 4)
  {
    std::cout << "Requires 3 argument: " << std::endl;
    std::cout << "1-Input file name " << std::endl;
    std::cout << "2-Output file name " << std::endl;
    std::cout << "3-Use Mixed Area" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using CoordType = double;
  using MeshType = itk::QuadEdgeMesh<CoordType, Dimension>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using SolverType = VNLSparseLUSolverTraits<CoordType>;

  using FilterType = itk::LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints<MeshType, MeshType, SolverType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->SetOrder(2);

  if (std::stoi(argv[3]) == 1)
  {
    filter->SetAreaComputationType(FilterType::MIXEDAREA);
  }
  else
  {
    filter->SetAreaComputationType(FilterType::NONE);
  }

  using CoefficientType = itk::ConformalMatrixCoefficients<MeshType>;
  CoefficientType coeff;
  filter->SetCoefficientsMethod(&coeff);

  MeshType::VectorType nullVector(0.);

  std::map<MeshType::PointIdentifier, MeshType::VectorType> constraints;
  constraints[150] = nullVector;
  constraints[292] = nullVector;
  constraints[185] = nullVector;
  constraints[180] = nullVector;
  constraints[153] = nullVector;
  constraints[183] = nullVector;
  constraints[226] = nullVector;

  MeshType::VectorType d(0.);
  d[2] = -0.1;

  constraints[729] = d;
  constraints[938] = d;

  MeshType::VectorType e(0.);
  e[1] = 0.1;
  e[2] = -0.1;

  constraints[40] = e;
  constraints[371] = e;

  std::map<MeshType::PointIdentifier, MeshType::VectorType>::const_iterator it = constraints.begin();
  while (it != constraints.end())
  {
    filter->SetDisplacement(it->first, it->second);
    ++it;
  }

  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & except)
  {
    std::cerr << "Failure: " << except.what();
    return EXIT_FAILURE;
  }

  using WriterType = itk::MeshFileWriter<MeshType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  MeshType::Pointer inputMesh = reader->GetOutput();
  MeshType::Pointer outputMesh = filter->GetOutput();

  it = constraints.begin();

  MeshType::PointType  iPt, oPt;
  MeshType::VectorType displacement;

  while (it != constraints.end())
  {
    iPt = inputMesh->GetPoint(it->first);
    oPt = outputMesh->GetPoint(it->first);
    displacement = oPt - iPt;

    if ((displacement - it->second).GetNorm() > 1e-6)
    {
      std::cerr << "Id: " << it->first << " * displacement: " << displacement << " * reference: " << it->second
                << std::endl;
      return EXIT_FAILURE;
    }
    ++it;
  }

  iPt = inputMesh->GetPoint(0);
  oPt = outputMesh->GetPoint(0);
  displacement = oPt - iPt;

  if (displacement.GetNorm() < 1e-6)
  {
    std::cerr << "No displacement" << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
