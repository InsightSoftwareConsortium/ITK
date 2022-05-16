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
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints.h"
#include "VNLSparseLUSolverTraits.h"
#include "itkTestingMacros.h"

int
itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraintsTest(int argc, char * argv[])
{
  // ** ERROR MESSAGE AND HELP ** //
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName useMixedArea"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using CoordType = double;
  using MeshType = itk::QuadEdgeMesh<CoordType, Dimension>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using SolverType = VNLSparseLUSolverTraits<CoordType>;

  using FilterType = itk::LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints<MeshType, MeshType, SolverType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    filter, LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints, LaplacianDeformationQuadEdgeMeshFilter);


  filter->SetInput(reader->GetOutput());
  filter->SetOrder(1);

  typename FilterType::OutputCoordRepType lambda = 1.0;
  filter->SetLambda(lambda);
  ITK_TEST_SET_GET_VALUE(lambda, filter->GetLambda());

  if (std::stoi(argv[3]) == 1)
  {
    filter->SetAreaComputationType(FilterType::AreaEnum::MIXEDAREA);
  }
  else
  {
    filter->SetAreaComputationType(FilterType::AreaEnum::NONE);
  }


  using CoefficientType = itk::ConformalMatrixCoefficients<MeshType>;
  CoefficientType coeff;
  filter->SetCoefficientsMethod(&coeff);

  constexpr MeshType::VectorType nullVector{};

  std::map<MeshType::PointIdentifier, MeshType::VectorType> constraints;
  constraints[150] = nullVector;
  constraints[292] = nullVector;
  constraints[185] = nullVector;
  constraints[180] = nullVector;
  constraints[153] = nullVector;
  constraints[183] = nullVector;
  constraints[226] = nullVector;

  MeshType::VectorType d{};
  d[2] = -0.1;

  constraints[729] = d;
  constraints[938] = d;

  MeshType::VectorType e{};
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

  filter->SetLocalLambda(371, 0.1);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & except)
  {
    std::cerr << "Failure: " << except.what();
    return EXIT_FAILURE;
  }

  using WriterType = itk::MeshFileWriter<MeshType>;
  auto writer = WriterType::New();
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

    if (it->second.GetNorm() > 1e-6)
    {
      if (displacement.GetNorm() < 1e-6)
      {
        std::cerr << "Id: " << it->first << " * no displacement" << std::endl;
        return EXIT_FAILURE;
      }
    }
    ++it;
  }

  return EXIT_SUCCESS;
}
