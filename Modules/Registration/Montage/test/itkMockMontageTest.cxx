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
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <type_traits>
#include <fstream>
#include <array>

constexpr unsigned xMontageSize = 10;
constexpr unsigned yMontageSize = 10;

constexpr unsigned Dimension = 2;
using PixelType = unsigned short;

using PointType = itk::Point<double, Dimension>;
using VectorType = itk::Vector<double, Dimension>;
using TransformType = itk::TranslationTransform<double, Dimension>;
using TableType = std::array<std::array<PointType, xMontageSize>, yMontageSize>;


double calculateError(const TableType& initalCoords, const TableType& actualCoords, const std::string& dir,
    std::ostream& out, unsigned xF, unsigned yF, unsigned xM, unsigned yM)
{
  double translationError = 0.0;
  std::string filenameFixed = dir + "/Image_" + std::to_string(xF + 1) + "_" + std::to_string(yF + 1) + ".tif";
  std::string filenameMoving = dir + "/Image_" + std::to_string(xM + 1) + "_" + std::to_string(yM + 1) + ".tif";
  std::cout << "Registering " << filenameMoving << " to " << filenameFixed << std::endl;
  out << std::to_string(xF + 1) + "_" + std::to_string(yF + 1) + " <- " + std::to_string(xM + 1) + "_" + std::to_string(yM + 1);

  using ImageType = itk::Image< PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader< ImageType >;
  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(filenameFixed);
  reader->Update();
  ImageType::Pointer fixedImage = reader->GetOutput();
  fixedImage->DisconnectPipeline();

  reader->SetFileName(filenameMoving);
  reader->Update();
  ImageType::Pointer  movingImage = reader->GetOutput();
  movingImage->DisconnectPipeline();

  //adjust origins (assume 0 origins in files)
  fixedImage->SetOrigin(initalCoords[yF][xF]);
  movingImage->SetOrigin(initalCoords[yM][xM]);

  //execute registration
  using PhaseCorrelationMethodType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage(fixedImage);
  phaseCorrelationMethod->SetMovingImage(movingImage);
  //phaseCorrelationMethod->DebugOn();

  using OperatorType = itk::PhaseCorrelationOperator< typename itk::NumericTraits< PixelType >::RealType, Dimension >;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator(pcmOperator);

  using OptimizerType = itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >;
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  phaseCorrelationMethod->SetOptimizer(pcmOptimizer);

  phaseCorrelationMethod->Update();

  static_assert(std::is_same<TransformType, typename PhaseCorrelationMethodType::TransformType>::value,
      "PhaseCorrelationMethod's TransformType is expected to be a TranslationTransform");
  const typename TransformType* regTr = phaseCorrelationMethod->GetOutput()->Get();

  //calculate error
  VectorType tr = regTr->GetOffset(); //translation measured by registration
  VectorType ta = (actualCoords[yM][xM] - initalCoords[yM][xM]) - (actualCoords[yF][xF] - initalCoords[yF][xF]); //translation (actual)
  for (unsigned d = 0; d < Dimension; d++)
    {
    out << '\t' << (tr[d] - ta[d]);
    translationError += std::abs(tr[d] - ta[d]);
    }
  out << std::endl;

  return translationError;
}

int itkMockMontageTest(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <directoryWtihInputData> <outputTSV>" << std::endl;
    return EXIT_FAILURE;
    }

  TableType stageCoords, actualCoords;

  //read coordinates from files
  std::ifstream fStage(std::string(argv[1]) + "/StageCoords.txt");
  std::ifstream fActual(std::string(argv[1]) + "/ActualCoords.txt");
  std::string temp;
  std::getline(fStage, temp); //throw away header
  std::getline(fActual, temp); //throw away header

  for (unsigned y = 0; y < yMontageSize; y++)
    {
    for (unsigned x = 0; x < xMontageSize; x++)
      {
      PointType p;
      for (unsigned d = 0; d < Dimension; d++)
        {
        fStage >> p[d];
        }
      stageCoords[y][x] = p;
      for (unsigned d = 0; d < Dimension; d++)
        {
        fActual >> p[d];
        }
      actualCoords[y][x] = p;
      }
    }

  //do the registrations and calculate registration errors
  std::ofstream registrationErrors(argv[2]);
  registrationErrors << "Fixed <- Moving";
  for (unsigned d = 0; d < Dimension; d++)
    {
    registrationErrors << '\t' << char('x' + d) << "Error";
    }
  registrationErrors << std::endl;

  double totalError = 0.0;
  for (unsigned y = 0; y < yMontageSize; y++)
    {
    for (unsigned x = 0; x < xMontageSize; x++)
      {
      if (x > 0)
        {
        totalError += calculateError(stageCoords, actualCoords, argv[1], registrationErrors, x - 1, y, x, y);
        }
      if (y > 0)
        {
        totalError += calculateError(stageCoords, actualCoords, argv[1], registrationErrors, x, y - 1, x, y);
        }
      }
    }

  double avgError = totalError / (2 * (xMontageSize - 1)*(yMontageSize - 1));
  std::cout << "Average per-registration translation error for all coordinates: " << avgError;
  if (avgError < 1.0)
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}//itkMockMontageTest<>
