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

#ifndef itkMockMontageHelper_hxx
#define itkMockMontageHelper_hxx

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkPhaseFrequencyCorrelationOptimizer.h"
#include "itkTileConfiguration.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"

#include <array>
#include <fstream>
#include <iomanip>
#include <type_traits>

// do the registration and calculate error for two images
template <typename PixelType, unsigned Dimension, typename PeakInterpolationMethodEnum>
double
calculateError(const itk::TileConfiguration<Dimension> &    stageTiles,
               const itk::TileConfiguration<Dimension> &    actualTiles,
               const std::string &                          inputPath,
               uint8_t                                      paddingMethod,
               unsigned                                     positionTolerance,
               std::map<PeakInterpolationMethodEnum, itk::Point<double, Dimension>> & regBias,
               std::ostream &                               out,
               size_t                                       fInd,
               size_t                                       mInd)
{
  double translationError = 0.0;
  std::cout << stageTiles.Tiles[fInd].FileName << " <- " << stageTiles.Tiles[mInd].FileName << std::endl;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(inputPath + stageTiles.Tiles[fInd].FileName);
  reader->Update();
  typename ImageType::Pointer fixedImage = reader->GetOutput();
  fixedImage->DisconnectPipeline();

  reader->SetFileName(inputPath + stageTiles.Tiles[mInd].FileName);
  reader->Update();
  typename ImageType::Pointer movingImage = reader->GetOutput();
  movingImage->DisconnectPipeline();

  // adjust origins (assume 0 origins in files)
  typename ImageType::SpacingType sp = fixedImage->GetSpacing();
  typename ImageType::PointType   origin = stageTiles.Tiles[fInd].Position;
  for (unsigned d = 0; d < Dimension; d++)
  {
    origin[d] *= sp[d];
  }
  fixedImage->SetOrigin(origin);
  sp = movingImage->GetSpacing();
  origin = stageTiles.Tiles[mInd].Position;
  for (unsigned d = 0; d < Dimension; d++)
  {
    origin[d] *= sp[d];
  }
  movingImage->SetOrigin(origin);

  // execute registration
  using PhaseCorrelationMethodType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage(fixedImage);
  phaseCorrelationMethod->SetMovingImage(movingImage);
  typename PhaseCorrelationMethodType::SizeType pad;
  pad.Fill(8 * sizeof(PixelType));
  phaseCorrelationMethod->SetObligatoryPadding(pad);
  // phaseCorrelationMethod->DebugOn();

  using PMType = typename PhaseCorrelationMethodType::PaddingMethodEnum;
  using PadMethodUnderlying = typename std::underlying_type<PMType>::type;
  static_assert(std::is_same<decltype(paddingMethod), PadMethodUnderlying>::value,
                "We expect type of paddingMethod to be equal to PadMethodUnderlying type");
  // cause compile error if this ever changes
  // to correct it, change type of paddingMethod parameter
  auto padMethod = static_cast<PMType>(paddingMethod);
  phaseCorrelationMethod->SetPaddingMethod(padMethod);

  using OperatorType = itk::PhaseCorrelationOperator<typename PhaseCorrelationMethodType::InternalPixelType, Dimension>;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator(pcmOperator);

  using OptimizerType = itk::PhaseCorrelationOptimizer<ImageType>;

  using RealOptimizerType = itk::MaxPhaseCorrelationOptimizer<PhaseCorrelationMethodType>;
  typename RealOptimizerType::Pointer realPCMOptimizer = RealOptimizerType::New();
  realPCMOptimizer->SetPixelDistanceTolerance(positionTolerance);

  using ComplexOptimizerType = itk::PhaseFrequencyCorrelationOptimizer<PhaseCorrelationMethodType>;
  typename ComplexOptimizerType::Pointer complexPCMOptimizer = ComplexOptimizerType::New();
  complexPCMOptimizer->SetPixelDistanceTolerance(positionTolerance);

  using PeakInterpolationType =
    typename OptimizerType::PeakInterpolationMethodEnum;

  for (auto peakMethod: itk::PhaseCorrelationOptimizerEnums::AllPeakInterpolationMethods)
  {
    itk::Point<double, Dimension> point;
    point.Fill(0.0);
    regBias[peakMethod] = point;
  }

  unsigned count = 0;
  for (auto peakMethod: itk::PhaseCorrelationOptimizerEnums::AllPeakInterpolationMethods)
  {
    std::cout << "\nTESTING WITH PEAK INTERPOLATION METHOD: " << peakMethod << std::endl;
    if (realPCMOptimizer->SupportsPeakInterpolationMethod(peakMethod))
    {
      realPCMOptimizer->SetPeakInterpolationMethod(peakMethod);
      phaseCorrelationMethod->SetOptimizer(realPCMOptimizer);
    }
    else if(complexPCMOptimizer->SupportsPeakInterpolationMethod(peakMethod))
    {
      complexPCMOptimizer->SetPeakInterpolationMethod(peakMethod);
      phaseCorrelationMethod->SetOptimizer(complexPCMOptimizer);
    }
    else
    {
      std::cerr << "PeakInterpolationMethod: " << peakMethod << " not supported!" << std::endl;
      return EXIT_FAILURE;
    }

    phaseCorrelationMethod->Modified(); // optimizer is not an "input" to PCM
    // so its modification does not cause a pipeline update automatically

    out << stageTiles.LinearIndexToNDIndex(fInd) << " <- " << stageTiles.LinearIndexToNDIndex(mInd);
    out << '\t' << peakMethod;
    std::cout << "    PeakMethod" << peakMethod << ":";

    phaseCorrelationMethod->Update();

    using TransformType = itk::TranslationTransform<double, Dimension>;
    static_assert(std::is_same<TransformType, typename PhaseCorrelationMethodType::TransformType>::value,
                  "PhaseCorrelationMethod's TransformType is expected to be a TranslationTransform");
    const TransformType * regTr = phaseCorrelationMethod->GetOutput()->Get();

    // calculate error
    using VectorType = itk::Vector<double, Dimension>;
    VectorType tr = regTr->GetOffset(); // translation measured by registration
    for (unsigned d = 0; d < Dimension; d++)
    {
      tr[d] /= sp[d];
    }
    VectorType ta = (actualTiles.Tiles[fInd].Position - stageTiles.Tiles[fInd].Position) -
                    (actualTiles.Tiles[mInd].Position - stageTiles.Tiles[mInd].Position); // translation (actual)
    regBias[peakMethod] += tr - ta;
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
} // calculateError

// do the registrations and calculate registration errors
template <typename PixelType, unsigned Dimension>
int
pairwiseTests(const itk::TileConfiguration<Dimension> & stageTiles,
              const itk::TileConfiguration<Dimension> & actualTiles,
              const std::string &                       inputPath,
              const std::string &                       outFilename,
              bool                                      varyPaddingMethods,
              unsigned                                  positionTolerance)
{
  int result = EXIT_SUCCESS;
  using ImageType = itk::Image<PixelType, Dimension>;
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ImageType, ImageType, float>;
  using PadMethodUnderlying = typename std::underlying_type<typename PCMType::PaddingMethodEnum>::type;
  using TileConfig = itk::TileConfiguration<Dimension>;

  for (auto padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Zero);
       padMethod <= static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Last);
       padMethod++)
  {
    if (!varyPaddingMethods) // go straight to the last, best method
    {
      padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Last);
    }
    std::ofstream registrationErrors(outFilename + std::to_string(padMethod) + ".tsv");
    std::cout << "Padding method " << padMethod << std::endl;
    registrationErrors << "Fixed <- Moving\tPeakInterpolationMethod";
    for (unsigned d = 0; d < Dimension; d++)
    {
      registrationErrors << '\t' << char('x' + d) << "Error";
    }
    registrationErrors << std::endl;

    using PhaseCorrelationOptimizerType = itk::PhaseCorrelationOptimizer<ImageType>;
    using PeakInterpolationMethodEnum = typename PhaseCorrelationOptimizerType::PeakInterpolationMethodEnum;

    const size_t                               linearSize = stageTiles.LinearSize();
    typename TileConfig::TileIndexType         ind;
    std::map<PeakInterpolationMethodEnum, itk::Point<double, Dimension>> accumulatedBias; // one per PeakInterpolationType
    size_t                                     count = 0;
    double                                     totalError = 0.0;
    for (size_t t = 0; t < linearSize; t++)
    {
      ind = stageTiles.LinearIndexToNDIndex(t);
      for (unsigned d = 0; d < Dimension; d++)
      {
        if (ind[d] > 0)
        {
          ++count;
          typename TileConfig::TileIndexType neighborInd = ind;
          --neighborInd[d];
          size_t fixedLinearIndex = stageTiles.nDIndexToLinearIndex(neighborInd);
          totalError += calculateError<PixelType, Dimension, PeakInterpolationMethodEnum>(stageTiles,
                                                             actualTiles,
                                                             inputPath,
                                                             padMethod,
                                                             positionTolerance,
                                                             accumulatedBias,
                                                             registrationErrors,
                                                             fixedLinearIndex,
                                                             t);
        }
      }
    }

    for (auto m: itk::PhaseCorrelationOptimizerEnums::AllPeakInterpolationMethods)
    {
      std::cout << "PeakInterpolation " << m << " has average translation bias:";
      for (unsigned d = 0; d < Dimension; d++)
      {
        std::cout << " " << accumulatedBias[m][d] / count;
      }
      std::cout << std::endl;
    }
    // double avgError = totalError / (xMontageSize * (yMontageSize - 1) + (xMontageSize - 1) * yMontageSize);
    double avgError = totalError / count;
    avgError /= Dimension; // report per-dimension error
    std::cout << "Average translation error for padding method " << padMethod << ": " << avgError << std::endl
              << std::endl;
    if (avgError >= 1.0)
    {
      result = EXIT_FAILURE;
    }
  }
  return result;
}

#endif // itkMockMontageHelper_hxx
