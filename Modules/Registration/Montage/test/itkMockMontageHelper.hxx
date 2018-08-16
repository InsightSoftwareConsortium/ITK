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

#ifndef itkMockMontageHelper_hxx
#define itkMockMontageHelper_hxx

#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <type_traits>
#include <array>
#include <fstream>
#include <iomanip>

//do the registration and calculate error for two images
template<typename PixelType, unsigned Dimension, typename PositionTableType, typename FilenameTableType>
double calculateError(const PositionTableType& initalCoords, const PositionTableType& actualCoords,
    const FilenameTableType& filenames, int paddingMethod,
    std::ostream& out, unsigned xF, unsigned yF, unsigned xM, unsigned yM)
{
  double translationError = 0.0;
  std::cout << filenames[yF][xF] << " <- " << filenames[yM][xM] << std::endl;

  using ImageType = itk::Image< PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader< ImageType >;
  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(filenames[yF][xF]);
  reader->Update();
  typename ImageType::Pointer fixedImage = reader->GetOutput();
  fixedImage->DisconnectPipeline();

  reader->SetFileName(filenames[yM][xM]);
  reader->Update();
  typename ImageType::Pointer  movingImage = reader->GetOutput();
  movingImage->DisconnectPipeline();

  //adjust origins (assume 0 origins in files)
  fixedImage->SetOrigin(initalCoords[yF][xF]);
  movingImage->SetOrigin(initalCoords[yM][xM]);

  //execute registration
  using PhaseCorrelationMethodType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage(fixedImage);
  phaseCorrelationMethod->SetMovingImage(movingImage);
  typename PhaseCorrelationMethodType::SizeType pad;
  pad.Fill(8 * sizeof(PixelType));
  phaseCorrelationMethod->SetObligatoryPadding(pad);
  //phaseCorrelationMethod->DebugOn();

  using PMType = typename PhaseCorrelationMethodType::PaddingMethod;
  using PadMethodUnderlying = typename std::underlying_type<PMType>::type;
  static_assert(std::is_same<decltype(paddingMethod), PadMethodUnderlying>::value,
      "We expect type of paddingMethod to be equal to PadMethodUnderlying type");
  //cause compile error if this ever changes
  //to correct it, change type of paddingMethod parameter
  auto padMethod = static_cast<PMType>(paddingMethod);
  phaseCorrelationMethod->SetPaddingMethod(padMethod);

  using OperatorType = itk::PhaseCorrelationOperator< typename itk::NumericTraits< PixelType >::RealType, Dimension >;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator(pcmOperator);

  using OptimizerType = itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >;
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  phaseCorrelationMethod->SetOptimizer(pcmOptimizer);

  using PeakInterpolationType = typename itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >::PeakInterpolationMethod;
  using PeakFinderUnderlying = typename std::underlying_type<PeakInterpolationType>::type;

  unsigned count = 0;
  for (auto peakMethod = static_cast<PeakFinderUnderlying>(PeakInterpolationType::None);
      peakMethod <= static_cast<PeakFinderUnderlying>(PeakInterpolationType::Last);
      peakMethod++)
    {
    pcmOptimizer->SetPeakInterpolationMethod(static_cast<PeakInterpolationType>(peakMethod));
    phaseCorrelationMethod->Modified(); // optimizer is not an "input" to PCM
    //so its modification does not cause a pipeline update automatically

    out << std::to_string(xF) + "," + std::to_string(yF) + " <- " + std::to_string(xM) + "," + std::to_string(yM);
    out << '\t' << peakMethod;
    std::cout << "    PeakMethod" << peakMethod << ":";

    phaseCorrelationMethod->Update();

    using TransformType = itk::TranslationTransform<double, Dimension>;
    static_assert(std::is_same<TransformType, typename PhaseCorrelationMethodType::TransformType>::value,
        "PhaseCorrelationMethod's TransformType is expected to be a TranslationTransform");
    const TransformType* regTr = phaseCorrelationMethod->GetOutput()->Get();

    //calculate error
    using VectorType = itk::Vector<double, Dimension>;
    VectorType tr = regTr->GetOffset(); //translation measured by registration
    VectorType ta = (actualCoords[yF][xF] - initalCoords[yF][xF]) - (actualCoords[yM][xM] - initalCoords[yM][xM]); //translation (actual)
    for (unsigned d = 0; d < Dimension; d++)
      {
      out << '\t' << (tr[d] - ta[d]);
      std::cout << "  " << std::setw(8) << std::setprecision(3) << (tr[d] - ta[d]);
      translationError += std::abs(tr[d] - ta[d]);
      }
    out << std::endl;
    count++;
  }
  std::cout << std::endl;

  return translationError / count;
}//calculateError

 //do the registrations and calculate registration errors
template<typename PixelType, unsigned xMontageSize, unsigned yMontageSize, typename PositionTableType, typename FilenameTableType>
int mockMontageTest(const PositionTableType& stageCoords, const PositionTableType& actualCoords,
    const FilenameTableType& filenames, const std::string& outFilename, bool varyPaddingMethods)
{
  int result = EXIT_SUCCESS;
  constexpr unsigned Dimension = 2;
  using ImageType = itk::Image< PixelType, Dimension>;
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
  using PadMethodUnderlying = typename std::underlying_type<typename PCMType::PaddingMethod>::type;

  for (auto padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethod::Zero);
      padMethod <= static_cast<PadMethodUnderlying>(PCMType::PaddingMethod::Last);
      padMethod++)
    {
    std::ofstream registrationErrors(outFilename + std::to_string(padMethod) + ".tsv");
    std::cout << "Padding method " << padMethod << std::endl;
    registrationErrors << "Fixed <- Moving\tPeakInterpolationMethod";
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
          totalError += calculateError<PixelType, Dimension>(stageCoords, actualCoords, filenames,
              padMethod, registrationErrors, x - 1, y, x, y);
          }
        if (y > 0)
          {
          totalError += calculateError<PixelType, Dimension>(stageCoords, actualCoords, filenames,
              padMethod, registrationErrors, x, y - 1, x, y);
          }
        }
      }

    double avgError = totalError / (xMontageSize*(yMontageSize - 1) + (xMontageSize - 1)*yMontageSize);
    avgError /= Dimension; //report per-dimension error
    std::cout << "Average translation error for padding method " << padMethod << ": " << avgError << std::endl << std::endl;
    if (avgError >= 1.0)
      {
      result = EXIT_FAILURE;
      }
    if (!varyPaddingMethods)
      {
      break;
      }
    }
  return result;
}

#endif //itkMockMontageHelper_hxx
