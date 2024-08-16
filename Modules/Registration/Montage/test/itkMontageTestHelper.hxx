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

#ifndef itkMontageTestHelper_hxx
#define itkMontageTestHelper_hxx

#include "itkAffineTransform.h"
#include "itkImageFileWriter.h"
#include "itkPhaseCorrelationOptimizer.h"
#include "itkTileConfiguration.h"
#include "itkRGBToLuminanceImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

#include <fstream>
#include <iomanip>
#include <type_traits>

template <typename TransformType>
void
WriteTransform(const TransformType * transform, std::string filename)
{
  using AffineType = itk::AffineTransform<double, 3>;
  using TransformWriterType = itk::TransformFileWriterTemplate<double>;
  TransformWriterType::Pointer tWriter = TransformWriterType::New();
  tWriter->SetFileName(filename);

  if (TransformType::SpaceDimension >= 2 || TransformType::SpaceDimension <= 3)
  { // convert into affine which Slicer can read
    AffineType::Pointer         aTr = AffineType::New();
    AffineType::TranslationType t;
    t.Fill(0);
    for (unsigned i = 0; i < TransformType::SpaceDimension; i++)
    {
      t[i] = transform->GetOffset()[i];
    }
    aTr->SetTranslation(t);
    tWriter->SetInput(aTr);
  }
  else
  {
    tWriter->SetInput(transform);
  }
  tWriter->Update();
}

template <typename TImage>
typename TImage::Pointer
ReadImage(const char * filename)
{
  using ReaderType = itk::ImageFileReader<TImage>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  return reader->GetOutput();
}

// use SFINAE to select whether to do simple assignment or RGB to Luminance conversion
template <typename RGBImage, typename ScalarImage>
typename std::enable_if<std::is_same<RGBImage, ScalarImage>::value, void>::type
assignRGBtoScalar(typename RGBImage::Pointer rgbImage, typename ScalarImage::Pointer & scalarImage)
{
  scalarImage = rgbImage;
}
template <typename RGBImage, typename ScalarImage>
typename std::enable_if<!std::is_same<RGBImage, ScalarImage>::value, void>::type
assignRGBtoScalar(typename RGBImage::Pointer rgbImage, typename ScalarImage::Pointer & scalarImage)
{
  using CastType = itk::RGBToLuminanceImageFilter<RGBImage, ScalarImage>;
  typename CastType::Pointer caster = CastType::New();
  caster->SetInput(rgbImage);
  caster->Update();
  scalarImage = caster->GetOutput();
  scalarImage->DisconnectPipeline();
}


// do the registrations and calculate registration errors
// negative peakMethodToUse means to try them all
// streamSubdivisions of 1 disables streaming (higher memory useage, less cluttered debug output)
template <typename PixelType, typename AccumulatePixelType, unsigned Dimension>
int
montageTest(const itk::TileConfiguration<Dimension> & stageTiles,
            const itk::TileConfiguration<Dimension> & actualTiles,
            const std::string &                       inputPath,
            const std::string &                       outFilename,
            bool                                      varyPaddingMethods,
            int                                       peakMethodToUse,
            bool                                      loadIntoMemory,
            unsigned                                  streamSubdivisions,
            bool                                      writeTransformFiles,
            bool                                      allowDrift,
            unsigned                                  positionTolerance,
            bool                                      writeImage)
{
  int result = EXIT_SUCCESS;
  using ScalarPixelType = typename itk::NumericTraits<PixelType>::ValueType;
  using TileConfig = itk::TileConfiguration<Dimension>;
  using PointType = itk::Point<itk::SpacePrecisionType, Dimension>;
  using VectorType = itk::Vector<itk::SpacePrecisionType, Dimension>;
  using TransformType = itk::TranslationTransform<itk::SpacePrecisionType, Dimension>;
  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ScalarImageType, ScalarImageType>;
  using PadMethodUnderlying = typename std::underlying_type<typename PCMType::PaddingMethodEnum>::type;
  typename ScalarImageType::SpacingType sp;
  itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New());
  const size_t                       linearSize = stageTiles.LinearSize();
  typename TileConfig::TileIndexType origin1;
  for (unsigned d = 0; d < Dimension; d++)
  {
    origin1[d] = stageTiles.AxisSizes[d] > 1 ? 1 : 0; // support montages of size 1 along a dimension
  }
  size_t    origin1linear = stageTiles.nDIndexToLinearIndex(origin1);
  PointType originAdjustment = PointType(stageTiles.Tiles[origin1linear].Position - stageTiles.Tiles[0].Position);

  using PeakInterpolationType = itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod;
  using PeakMethodUnderlying = typename std::underlying_type<PeakInterpolationType>::type;
  using MontageType = itk::TileMontage<ScalarImageType>;
  using ResamplerType = itk::TileMergeImageFilter<OriginalImageType, AccumulatePixelType>;

  std::vector<typename OriginalImageType::Pointer> oImages(linearSize);
  std::vector<typename ScalarImageType::Pointer>   sImages(linearSize);
  typename TileConfig::TileIndexType               ind;
  if (loadIntoMemory)
  {
    for (size_t t = 0; t < linearSize; t++)
    {
      std::string                         filename = inputPath + stageTiles.Tiles[t].FileName;
      typename OriginalImageType::Pointer image = ReadImage<OriginalImageType>(filename.c_str());
      PointType                           origin = stageTiles.Tiles[t].Position;
      sp = image->GetSpacing();
      for (unsigned d = 0; d < Dimension; d++)
      {
        origin[d] *= sp[d];
      }
      image->SetOrigin(origin);
      oImages[t] = image;
      assignRGBtoScalar<OriginalImageType, ScalarImageType>(image, sImages[t]);

      // show image loading progress
      ind = stageTiles.LinearIndexToNDIndex(t);
      char digit = '0';
      for (unsigned d = 0; d < Dimension; d++)
      {
        if (ind[d] < stageTiles.AxisSizes[d] - 1)
        {
          break;
        }
        ++digit;
      }
      std::cout << digit << std::flush;
    }
  }
  else
  {
    // load the first tile and take spacing from it
    std::string filename = inputPath + stageTiles.Tiles[0].FileName;
    using ReaderType = itk::ImageFileReader<OriginalImageType>;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(filename);
    reader->UpdateOutputInformation();
    sp = reader->GetOutput()->GetSpacing();
    for (unsigned d = 0; d < Dimension; d++)
    {
      originAdjustment[d] *= sp[d];
    }
  }
  std::cout << std::endl;

  for (auto padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Zero);
       padMethod <= static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Last);
       padMethod++)
  {
    if (!varyPaddingMethods) // go straight to the last, best method
    {
      padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethodEnum::Last);
    }
    auto          paddingMethod = static_cast<typename PCMType::PaddingMethodEnum>(padMethod);
    std::ofstream registrationErrors(outFilename + std::to_string(padMethod) + ".tsv");
    std::cout << paddingMethod << std::endl;
    registrationErrors << "PeakInterpolationMethod";
    for (unsigned d = 0; d < Dimension; d++)
    {
      registrationErrors << '\t' << char('x' + d) << "Tile";
    }
    for (unsigned d = 0; d < Dimension; d++)
    {
      registrationErrors << '\t' << char('x' + d) << "Error";
    }
    registrationErrors << std::endl;


    typename MontageType::Pointer montage = MontageType::New();
    montage->SetPaddingMethod(paddingMethod);
    montage->SetPositionTolerance(positionTolerance);
    montage->SetMontageSize(stageTiles.AxisSizes);
    if (!loadIntoMemory)
    {
      montage->SetOriginAdjustment(originAdjustment);
      montage->SetForcedSpacing(sp);
      // Force full coarse-grained parallelism. It helps with decoding JPEG images, but leads to high memory use.
      // montage->SetNumberOfWorkUnits(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads());
    }

    for (size_t t = 0; t < linearSize; t++)
    {
      std::string filename = inputPath + stageTiles.Tiles[t].FileName;
      if (loadIntoMemory)
      {
        montage->SetInputTile(t, sImages[t]);
      }
      else
      {
        montage->SetInputTile(t, filename);
      }
    }

    // std::initializer_list<itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod> interpolationMethods =
    // itk::PhaseCorrelationOptimizerEnums::AllPeakInterpolationMethods;
    std::initializer_list<itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod> interpolationMethods = {
      itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::None,
      itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Parabolic,
      itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Cosine,
      itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::WeightedMeanPhase,
    };
    if (peakMethodToUse >= 0)
    {
      auto peakMethodNumber = static_cast<PeakMethodUnderlying>(peakMethodToUse);
      std::initializer_list<itk::PhaseCorrelationOptimizerEnums::PeakInterpolationMethod> interpMethods = {
        static_cast<PeakInterpolationType>(peakMethodNumber)
      };
      interpolationMethods = interpMethods;
    }
    for (auto peakMethod : interpolationMethods)
    {
      auto peakMethodNumber = static_cast<PeakMethodUnderlying>(peakMethod);
      montage->SetPeakInterpolationMethod(peakMethod);
      std::cout << peakMethod << std::endl;
      itk::SimpleFilterWatcher fw(montage, "montage");
      // montage->SetDebug( true ); // enable more debugging output from global tile optimization
      montage->Update();

      std::cout << std::fixed;

      std::vector<VectorType> regPos(linearSize); // translations measured by registration
      // translations using average translation to neighbors and neighbors' ground truth
      std::vector<typename TileConfig::PointType> avgPos(linearSize);
      for (size_t t = 0; t < linearSize; t++)
      {
        ind = stageTiles.LinearIndexToNDIndex(t);
        const TransformType * regTr = montage->GetOutputTransform(ind);
        if (writeTransformFiles)
        {
          std::ostringstream ostrm;
          ostrm << outFilename << static_cast<int>(padMethod) << "_" << static_cast<int>(peakMethodNumber) << "_Tr_"
                << t << ".tfm";
          WriteTransform(regTr, ostrm.str());
        }
        regPos[t] = regTr->GetOffset();
        for (unsigned d = 0; d < Dimension; d++)
        {
          regPos[t][d] /= sp[d]; // convert into index units
        }
        avgPos[t].Fill(0.0); // initialize to zeroes
      }

      // make averages
      for (size_t t = 0; t < linearSize; t++)
      {
        ind = stageTiles.LinearIndexToNDIndex(t);
        VectorType avg;
        avg.Fill(0);
        unsigned count = 0;
        for (unsigned d = 0; d < Dimension; d++)
        {
          if (ind[d] > 0)
          {
            ++count;
            typename TileConfig::TileIndexType neighborInd = ind;
            --neighborInd[d];
            size_t nInd = stageTiles.nDIndexToLinearIndex(neighborInd);
            avg += regPos[t] - regPos[nInd] - (actualTiles.Tiles[nInd].Position - stageTiles.Tiles[nInd].Position);
          }

          if (ind[d] < stageTiles.AxisSizes[d] - 1)
          {
            ++count;
            typename TileConfig::TileIndexType neighborInd = ind;
            ++neighborInd[d];
            size_t nInd = stageTiles.nDIndexToLinearIndex(neighborInd);
            avg += regPos[t] - regPos[nInd] - (actualTiles.Tiles[nInd].Position - stageTiles.Tiles[nInd].Position);
          }
        }

        for (unsigned d = 0; d < Dimension; d++) // iterate over dimension because Vector and Point don't mix well
        {
          avgPos[t][d] = avg[d] / count;
        }
      }

      double totalError = 0.0;
      for (size_t t = 0; t < linearSize; t++)
      {
        ind = stageTiles.LinearIndexToNDIndex(t);
        std::cout << ind << ": " << regPos[t];
        registrationErrors << peakMethod;
        for (unsigned d = 0; d < Dimension; d++)
        {
          registrationErrors << '\t' << ind[d];
        }

        // calculate error
        const VectorType & tr = regPos[t]; // translation measured by registration
        VectorType         ta = stageTiles.Tiles[t].Position - actualTiles.Tiles[t].Position; // translation (actual)
        // account for tile zero maybe not being at coordinates 0
        ta += actualTiles.Tiles[0].Position - stageTiles.Tiles[0].Position;
        double singleError = 0.0;
        double alternativeError = 0.0; // to account for accumulation of individual errors
        for (unsigned d = 0; d < Dimension; d++)
        {
          registrationErrors << '\t' << (tr[d] - ta[d]);
          std::cout << "  " << std::setw(8) << std::setprecision(3) << (tr[d] - ta[d]);
          singleError += itk::Math::abs(tr[d] - ta[d]);
          alternativeError += itk::Math::abs(avgPos[t][d] - ta[d]);
        }

        if (alternativeError >= 5.0 && alternativeError < singleError)
        {
          result = EXIT_FAILURE;
          registrationErrors << "\tseverly wrong\t" << alternativeError;
          std::cout << "  severly wrong: " << alternativeError;
        }
        if (allowDrift)
        {
          totalError += std::min(singleError, alternativeError);
        }
        else
        {
          totalError += singleError;
        }
        registrationErrors << std::endl;
        std::cout << std::endl;
      }
      // allow error of whole pixel for each tile, ignoring tile 0
      // also allow accumulation of one pixel for each registration - this effectively double the tolerance
      double avgError = 0.5 * totalError / (linearSize - 1);
      avgError /= Dimension; // report per-dimension error
      registrationErrors << "Average translation error for " << paddingMethod << " and " << peakMethod << ": "
                         << avgError << std::endl;
      std::cout << "\nAverage translation error for " << std::endl;
      std::cout << "\t" << paddingMethod << std::endl;
      std::cout << "\t\t and" << std::endl;
      std::cout << "\t" << peakMethod << std::endl;
      std::cout << "\t\t " << avgError << "\n" << std::endl;
      if (avgError >= 1.2)
      {
        result = EXIT_FAILURE;
      }

      if (writeImage) // write generated mosaic
      {
        typename ResamplerType::Pointer resampleF = ResamplerType::New();
        itk::SimpleFilterWatcher        fw2(resampleF, "resampler");
        resampleF->SetMontageSize(stageTiles.AxisSizes);
        if (!loadIntoMemory)
        {
          resampleF->SetOriginAdjustment(originAdjustment);
          resampleF->SetForcedSpacing(sp);
        }

        for (size_t t = 0; t < linearSize; t++)
        {
          std::string filename = inputPath + stageTiles.Tiles[t].FileName;
          if (loadIntoMemory)
          {
            resampleF->SetInputTile(t, oImages[t]);
          }
          else
          {
            resampleF->SetInputTile(t, filename);
          }
          ind = stageTiles.LinearIndexToNDIndex(t);

          resampleF->SetTileTransform(ind, montage->GetOutputTransform(ind));
        }

        // resampleF->Update(); // updating here prevents streaming
        using WriterType = itk::ImageFileWriter<OriginalImageType>;
        typename WriterType::Pointer w = WriterType::New();
        w->SetInput(resampleF->GetOutput());
        // resampleF->DebugOn(); //generate an image of contributing regions
        // MetaImage format supports streaming
        std::ostringstream ostrm;
        ostrm << outFilename << static_cast<int>(padMethod) << "_" << static_cast<int>(peakMethodNumber) << ".mha";
        w->SetFileName(ostrm.str());
        // w->UseCompressionOn();
        w->SetNumberOfStreamDivisions(streamSubdivisions);
        w->Update();
      }
      if (peakMethodToUse >= 0) // peak method was specified
      {
        break; // do not try them all
      }
    }

    std::cout << std::endl;
  }
  return result;
}

#endif // itkMontageTestHelper_hxx
