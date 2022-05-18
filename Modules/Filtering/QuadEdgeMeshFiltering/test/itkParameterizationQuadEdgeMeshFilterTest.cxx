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

#include "VNLSparseLUSolverTraits.h"
#include "VNLIterativeSparseSolverTraits.h"

#include "itkParameterizationQuadEdgeMeshFilter.h"
#include "itkTestingMacros.h"

template <typename TSolver>
int
ParameterizationQuadEdgeMeshFilterTest(const char * inputFilename,
                                       unsigned int borderType,
                                       unsigned int coefficientType,
                                       const char * outputFilename)
{
  // ** TYPEDEF **
  using Coord = typename TSolver::ValueType;

  using MeshType = itk::QuadEdgeMesh<Coord, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;
  using WriterType = itk::MeshFileWriter<MeshType>;
  using BorderTransformType = itk::BorderQuadEdgeMeshFilter<MeshType, MeshType>;


  // ** READ THE FILE IN **
  auto reader = ReaderType::New();
  reader->SetFileName(inputFilename);

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

  typename MeshType::Pointer mesh = reader->GetOutput();

  // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
  auto border_transform = BorderTransformType::New();
  border_transform->SetInput(mesh);
  // two following line for coverage
  border_transform->SetRadius(border_transform->GetRadius());
  border_transform->GetNameOfClass();

  switch (borderType) // choose border type
  {
    case 0: // square shaped domain
      border_transform->SetTransformType(itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::SQUARE_BORDER_TRANSFORM);
      break;
    case 1: // disk shaped domain
      border_transform->SetTransformType(itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::DISK_BORDER_TRANSFORM);
      break;
    default: // handle .... user ....
      std::cerr << "2nd argument must be " << std::endl;
      std::cerr << "0 for SQUARE BORDER TRANSFORM or "
                << "1 for DISK BORDER TRANSFORM" << std::endl;
      return EXIT_FAILURE;
  }
  std::cout << "Transform type is: " << border_transform->GetTransformType();
  std::cout << std::endl;

  // ** CHOOSE AND SET BARYCENTRIC WEIGHTS **
  itk::OnesMatrixCoefficients<MeshType>                     coeff0;
  itk::InverseEuclideanDistanceMatrixCoefficients<MeshType> coeff1;
  itk::ConformalMatrixCoefficients<MeshType>                coeff2;
  itk::AuthalicMatrixCoefficients<MeshType>                 coeff3;
  itk::HarmonicMatrixCoefficients<MeshType>                 coeff4;

  using ParametrizationType = itk::ParameterizationQuadEdgeMeshFilter<MeshType, MeshType, TSolver>;
  auto param = ParametrizationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(param, ParameterizationQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);


  param->SetBorderTransform(border_transform);
  ITK_TEST_SET_GET_VALUE(border_transform, param->GetBorderTransform());

  param->SetInput(mesh);

  switch (coefficientType)
  {
    case 0:
      param->SetCoefficientsMethod(&coeff0);
      break;
    case 1:
      param->SetCoefficientsMethod(&coeff1);
      break;
    case 2:
      param->SetCoefficientsMethod(&coeff2);
      break;
    case 3:
      param->SetCoefficientsMethod(&coeff3);
      break;
    case 4:
      param->SetCoefficientsMethod(&coeff4);
      break;
    default:
      std::cerr << "3rd argument must be " << std::endl;
      std::cerr << "0, 1, 2, 3 or 4" << std::endl;
      std::cerr << "Here it is: " << coefficientType << std::endl;
      return EXIT_FAILURE;
  }

  // ** PROCESS **
  param->Update();
  typename MeshType::Pointer output = param->GetOutput();

  // ** WRITE OUTPUT **
  auto writer = WriterType::New();
  writer->SetInput(param->GetOutput());
  writer->SetFileName(outputFilename);
  writer->Update();

  // ** PRINT **
  std::cout << "BorderTransform: \n" << border_transform;
  std::cout << "Parametrization: \n" << param;

  // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
  return EXIT_SUCCESS;
}

int
itkParameterizationQuadEdgeMeshFilterTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename borderType (0: SQUARE; 1: DISK)" << std::endl;
    std::cerr << " coefficientType" << std::endl;
    std::cerr << "   * 0: OnesMatrixCoefficients" << std::endl;
    std::cerr << "   * 1: InverseEuclideanDistanceMatrixCoefficients" << std::endl;
    std::cerr << "   * 2: ConformalMatrixCoefficients" << std::endl;
    std::cerr << "   * 3: AuthalicMatrixCoefficients" << std::endl;
    std::cerr << "   * 4: HarmonicMatrixCoefficients" << std::endl;
    std::cerr << " solverType (0: iterative; 1: LU decomposition)" << std::endl;
    std::cerr << " outputFilename" << std::endl;

    return EXIT_FAILURE;
  }

  using TCoord = double;
  using IterativeSolverTraits = VNLIterativeSparseSolverTraits<TCoord>;
  using LUSolverTraits = VNLSparseLUSolverTraits<TCoord>;

  if (std::stoi(argv[4]) == 0)
  {
    return ParameterizationQuadEdgeMeshFilterTest<IterativeSolverTraits>(
      argv[1], std::stoi(argv[2]), std::stoi(argv[3]), argv[5]);
  }
  else if (std::stoi(argv[4]) == 1)
  {
    return ParameterizationQuadEdgeMeshFilterTest<LUSolverTraits>(
      argv[1], std::stoi(argv[2]), std::stoi(argv[3]), argv[5]);
  }
  else
  {
    return EXIT_FAILURE;
  }
}
