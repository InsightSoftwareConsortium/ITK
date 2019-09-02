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

#ifndef itkMontageTestHelper_hxx
#define itkMontageTestHelper_hxx

#include "itkAffineTransform.h"
#include "itkImageFileWriter.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkParseTileConfiguration.h"
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
template <typename PixelType, typename AccumulatePixelType>
int
montageTest(const itk::TileLayout2D & stageTiles,
            const itk::TileLayout2D & actualTiles,
            const std::string &       inputPath,
            const std::string &       outFilename,
            bool                      varyPaddingMethods,
            int                       peakMethodToUse,
            bool                      loadIntoMemory,
            unsigned                  streamSubdivisions,
            bool                      writeTransformFiles,
            bool                      allowDrift,
            unsigned                  positionTolerance,
            bool                      writeImage)
{
  int result = EXIT_SUCCESS;
  using ScalarPixelType = typename itk::NumericTraits<PixelType>::ValueType;
  constexpr unsigned Dimension = 2;
  using PointType = itk::Point<double, Dimension>;
  using VectorType = itk::Vector<double, Dimension>;
  using TransformType = itk::TranslationTransform<double, Dimension>;
  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ScalarImageType, ScalarImageType>;
  using PadMethodUnderlying = typename std::underlying_type<typename PCMType::PaddingMethod>::type;
  typename ScalarImageType::SpacingType sp;
  itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New());
  const size_t   yMontageSize = stageTiles.size();
  const size_t   xMontageSize = stageTiles[0].size();
  const unsigned origin1x = xMontageSize > 1 ? 1 : 0; // support 1xN montages
  const unsigned origin1y = yMontageSize > 1 ? 1 : 0; // support Nx1 montages
  PointType      originAdjustment = stageTiles[origin1y][origin1x].Position;

  using PeakInterpolationType = typename itk::MaxPhaseCorrelationOptimizer<PCMType>::PeakInterpolationMethod;
  using PeakFinderUnderlying = typename std::underlying_type<PeakInterpolationType>::type;
  using MontageType = itk::TileMontage<ScalarImageType>;
  using ResamplerType = itk::TileMergeImageFilter<OriginalImageType, AccumulatePixelType>;

  std::vector<std::vector<typename OriginalImageType::Pointer>> oImages(yMontageSize);
  std::vector<std::vector<typename ScalarImageType::Pointer>>   sImages(yMontageSize);
  typename MontageType::TileIndexType                           ind;
  if (loadIntoMemory)
  {
    for (unsigned y = 0; y < yMontageSize; y++)
    {
      oImages[y].resize(xMontageSize);
      sImages[y].resize(xMontageSize);
      for (unsigned x = 0; x < xMontageSize; x++)
      {
        std::string                         filename = inputPath + stageTiles[y][x].FileName;
        typename OriginalImageType::Pointer image = ReadImage<OriginalImageType>(filename.c_str());
        PointType                           origin = stageTiles[y][x].Position;
        sp = image->GetSpacing();
        for (unsigned d = 0; d < Dimension; d++)
        {
          origin[d] *= sp[d];
        }
        image->SetOrigin(origin);
        oImages[y][x] = image;
        assignRGBtoScalar<OriginalImageType, ScalarImageType>(image, sImages[y][x]);
        std::cout << '.' << std::flush;
      }
      std::cout << '|' << std::flush;
    }
  }
  else
  {
    // load the first tile and take spacing from it
    std::string filename = inputPath + stageTiles[0][0].FileName;
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

  for (auto padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethod::Zero);
       padMethod <= static_cast<PadMethodUnderlying>(PCMType::PaddingMethod::Last);
       padMethod++)
  {
    if (!varyPaddingMethods) // go straight to the last, best method
    {
      padMethod = static_cast<PadMethodUnderlying>(PCMType::PaddingMethod::Last);
    }
    std::ofstream registrationErrors(outFilename + std::to_string(padMethod) + ".tsv");
    std::cout << "Padding method " << padMethod << std::endl;
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
    auto                          paddingMethod = static_cast<typename PCMType::PaddingMethod>(padMethod);
    montage->SetPaddingMethod(paddingMethod);
    montage->SetPositionTolerance(positionTolerance);
    montage->SetMontageSize({ xMontageSize, yMontageSize });
    if (!loadIntoMemory)
    {
      montage->SetOriginAdjustment(originAdjustment);
      montage->SetForcedSpacing(sp);
      // Set full coarse-grained parallelism. It helps with decoding JPEG images.
      montage->SetNumberOfWorkUnits(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads());
    }

    for (unsigned y = 0; y < yMontageSize; y++)
    {
      ind[1] = y;
      for (unsigned x = 0; x < xMontageSize; x++)
      {
        ind[0] = x;
        std::string filename = inputPath + stageTiles[y][x].FileName;
        if (loadIntoMemory)
        {
          montage->SetInputTile(ind, sImages[y][x]);
        }
        else
        {
          montage->SetInputTile(ind, filename);
        }
      }
    }

    for (auto peakMethod = static_cast<PeakFinderUnderlying>(PeakInterpolationType::None);
         peakMethod <= static_cast<PeakFinderUnderlying>(PeakInterpolationType::Last);
         peakMethod++)
    {
      if (peakMethodToUse >= 0)
      {
        peakMethod = static_cast<PeakFinderUnderlying>(peakMethodToUse);
      }
      montage->SetPeakInterpolationMethod(static_cast<PeakInterpolationType>(peakMethod));
      std::cout << "    PeakMethod " << peakMethod << std::endl;
      itk::SimpleFilterWatcher fw(montage, "montage");
      // montage->SetDebug( true ); // enable more debugging output from global tile optimization
      montage->Update();

      std::cout << std::fixed;

      std::vector<std::vector<VectorType>> regPos(yMontageSize); // translations measured by registration
      // translations using average translation to neighbors and neighbors' ground truth
      std::vector<std::vector<itk::Tile2D::PointType>> avgPos(yMontageSize);
      for (unsigned y = 0; y < yMontageSize; y++)
      {
        ind[1] = y;
        regPos[y].resize(xMontageSize);
        avgPos[y].resize(xMontageSize);
        for (unsigned x = 0; x < xMontageSize; x++)
        {
          ind[0] = x;
          const TransformType * regTr = montage->GetOutputTransform(ind);
          if (writeTransformFiles)
          {
            WriteTransform(regTr,
                           outFilename + std::to_string(padMethod) + "_" + std::to_string(peakMethod) + "_Tr_" +
                             std::to_string(x) + "_" + std::to_string(y) + ".tfm");
          }
          regPos[y][x] = regTr->GetOffset();
          for (unsigned d = 0; d < Dimension; d++)
          {
            regPos[y][x][d] /= sp[d]; // convert into index units
          }
          avgPos[y][x].Fill(0.0); // initialize to zeroes
        }
      }

      // make averages
      for (unsigned y = 0; y < yMontageSize; y++)
      {
        for (unsigned x = 0; x < xMontageSize; x++)
        {
          for (unsigned d = 0; d < Dimension; d++) // iterate over dimension because Vector and Point don't mix well
          {
            unsigned count = 0;
            if (x > 0)
            {
              ++count;
              avgPos[y][x][d] += regPos[y][x][d] - regPos[y][x - 1][d] -
                                 (actualTiles[y][x - 1].Position[d] - stageTiles[y][x - 1].Position[d]);
            }
            if (x < xMontageSize - 1)
            {
              ++count;
              avgPos[y][x][d] += regPos[y][x][d] - regPos[y][x + 1][d] -
                                 (actualTiles[y][x + 1].Position[d] - stageTiles[y][x + 1].Position[d]);
            }
            if (y > 0)
            {
              ++count;
              avgPos[y][x][d] += regPos[y][x][d] - regPos[y - 1][x][d] -
                                 (actualTiles[y - 1][x].Position[d] - stageTiles[y - 1][x].Position[d]);
            }
            if (y < yMontageSize - 1)
            {
              ++count;
              avgPos[y][x][d] += regPos[y][x][d] - regPos[y + 1][x][d] -
                                 (actualTiles[y + 1][x].Position[d] - stageTiles[y + 1][x].Position[d]);
            }
            avgPos[y][x][d] /= count;
          }
        }
      }

      double totalError = 0.0;
      for (unsigned y = 0; y < yMontageSize; y++)
      {
        for (unsigned x = 0; x < xMontageSize; x++)
        {
          std::cout << "(" << x << ", " << y << "): " << regPos[y][x];
          registrationErrors << peakMethod << '\t' << x << '\t' << y;

          // calculate error
          const VectorType & tr = regPos[y][x]; // translation measured by registration
          VectorType         ta = stageTiles[y][x].Position - actualTiles[y][x].Position; // translation (actual)
          ta += actualTiles[0][0].Position -
                stageTiles[0][0].Position; // account for tile zero maybe not being at coordinates 0
          double singleError = 0.0;
          double alternativeError = 0.0; // to account for accumulation of individual errors
          for (unsigned d = 0; d < Dimension; d++)
          {
            registrationErrors << '\t' << (tr[d] - ta[d]);
            std::cout << "  " << std::setw(8) << std::setprecision(3) << (tr[d] - ta[d]);
            singleError += std::abs(tr[d] - ta[d]);
            alternativeError += std::abs(avgPos[y][x][d] - ta[d]);
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
      }
      // allow error of whole pixel for each tile, ignoring tile 0
      // also allow accumulation of one pixel for each registration - this effectively double the tolerance
      double avgError = 0.5 * totalError / (xMontageSize * yMontageSize - 1);
      avgError /= Dimension; // report per-dimension error
      registrationErrors << "\nAverage translation error for padding method " << padMethod
                         << " and peak interpolation method " << peakMethod << ": " << avgError << std::endl;
      std::cout << "\nAverage translation error for padding method " << padMethod << " and peak interpolation method "
                << peakMethod << ": " << avgError << std::endl;
      if (avgError >= 1.2)
      {
        result = EXIT_FAILURE;
      }

      if (writeImage) // write generated mosaic
      {
        typename ResamplerType::Pointer resampleF = ResamplerType::New();
        itk::SimpleFilterWatcher        fw2(resampleF, "resampler");
        resampleF->SetMontageSize({ xMontageSize, yMontageSize });
        if (!loadIntoMemory)
        {
          resampleF->SetOriginAdjustment(originAdjustment);
          resampleF->SetForcedSpacing(sp);
        }
        for (unsigned y = 0; y < yMontageSize; y++)
        {
          ind[1] = y;
          for (unsigned x = 0; x < xMontageSize; x++)
          {
            ind[0] = x;
            std::string filename = inputPath + stageTiles[y][x].FileName;
            if (loadIntoMemory)
            {
              resampleF->SetInputTile(ind, oImages[y][x]);
            }
            else
            {
              resampleF->SetInputTile(ind, filename);
            }
            resampleF->SetTileTransform(ind, montage->GetOutputTransform(ind));
          }
        }

        // resampleF->Update(); // updating here prevents streaming
        using WriterType = itk::ImageFileWriter<OriginalImageType>;
        typename WriterType::Pointer w = WriterType::New();
        w->SetInput(resampleF->GetOutput());
        // resampleF->DebugOn(); //generate an image of contributing regions
        // MetaImage format supports streaming
        w->SetFileName(outFilename + std::to_string(padMethod) + "_" + std::to_string(peakMethod) + ".mha");
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
