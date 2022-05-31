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

#ifndef itkInMemoryMontageTestHelper_hxx
#define itkInMemoryMontageTestHelper_hxx

#include "itkAffineTransform.h"
#include "itkImageFileWriter.h"
#include "itkPhaseCorrelationOptimizer.h"
#include "itkObject.h"
#include "itkTileConfiguration.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

#include <fstream>
#include <iomanip>
#include <type_traits>

template <typename PixelType, typename AccumulatePixelType>
class ITK_TEMPLATE_EXPORT InMemoryMontageTest : public itk::Object
{
public:
  InMemoryMontageTest() = default;
  ~InMemoryMontageTest() override = default;

  ITK_DISALLOW_COPY_AND_MOVE(InMemoryMontageTest);

  /** Standard class type aliases. */
  using Self = InMemoryMontageTest;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  enum class TestVariation : unsigned int
  {
    UOrigin_USpacing_UTransform = 0,
    UOrigin_DSpacing_UTransform,
    UOrigin_USpacing_DTransform,
    UOrigin_DSpacing_DTransform,
    DOrigin_USpacing_UTransform,
    DOrigin_DSpacing_UTransform,
    DOrigin_USpacing_DTransform,
    DOrigin_DSpacing_DTransform,
  };

  /* -----------------------------------------------------------------------------------------------
   * This method tests stitching images with different combinations of origins, spacings, and
   * transforms.
   *
   * Legend:
   * UO - Unit Origin.  An origin of (0, 0, 0).
   * US - Unit Spacing.  A spacing of (1, 1, 1).
   *                                                (1, 0, 0)
   * UT - Unit Transform.  An identity transform of (0, 1, 0)
   *                                                (0, 0, 1)
   *
   * DO - Dynamic Origin.  An origin that does not equal the unit origin, such as (12, 0, 51)
   * DS - Dynamic Spacing.  Spacing that is not identical to unit spacing, such as (0.5, 0.25, 0.5)
   * DT - Dynamic Transform.  A transform that is not identical to a unit transform.
   *
   * ~~~~~~~~~~~
   *
   * Each run of the test uses either a unit or dynamic origin, spacing, and transform.  In total,
   * there are 8 combinations...
   *
   * Combinations:
   * UO | US | UT - Fail
   * UO | DS | UT - Fail
   * UO | US | DT
   * UO | DS | DT
   *
   * DO | US | UT
   * DO | DS | UT
   * DO | US | DT
   * DO | DS | DT
   * ----------------------------------------------------------------------------------------------- */
  int
  execute(const itk::TileConfiguration<2> & stageTiles,
          const std::string &               inputPath,
          std::string &                     outFilename,
          TestVariation                     variation,
          unsigned                          streamSubdivisions)
  {
    // MetaImage format supports streaming
    outFilename = outFilename + std::to_string(static_cast<unsigned int>(variation)) + ".mha";

    switch (variation)
    {
      case TestVariation::UOrigin_USpacing_UTransform:
        return inMemoryMontageTest0(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::UOrigin_DSpacing_UTransform:
        return inMemoryMontageTest1(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::UOrigin_USpacing_DTransform:
        return inMemoryMontageTest2(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::UOrigin_DSpacing_DTransform:
        return inMemoryMontageTest3(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::DOrigin_USpacing_UTransform:
        return inMemoryMontageTest4(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::DOrigin_DSpacing_UTransform:
        return inMemoryMontageTest5(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::DOrigin_USpacing_DTransform:
        return inMemoryMontageTest6(stageTiles, inputPath, outFilename, streamSubdivisions);
      case TestVariation::DOrigin_DSpacing_DTransform:
        return inMemoryMontageTest7(stageTiles, inputPath, outFilename, streamSubdivisions);
      default:
        return EXIT_FAILURE;
    }
  }

private:
  static constexpr unsigned Dimension = 2;
  using ScalarPixelType = typename itk::NumericTraits<PixelType>::ValueType;
  using PointType = itk::Point<double, Dimension>;
  using VectorType = itk::Vector<double, Dimension>;
  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using SpacingType = typename ScalarImageType::SpacingType;
  using SpacingRow = std::vector<SpacingType>;
  using Spacing2D = std::vector<SpacingRow>;
  using MontageType = itk::TileMontage<ScalarImageType>;
  using TransformPtr = typename MontageType::TransformPointer;
  using TransformPtrRow = std::vector<TransformPtr>;
  using Transform2D = std::vector<TransformPtrRow>;
  using OriginPoint = typename ScalarImageType::PointType;
  using OriginRow = std::vector<OriginPoint>;
  using Origin2D = std::vector<OriginRow>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<ScalarImageType, ScalarImageType>;
  using TransformType = itk::TranslationTransform<double, Dimension>;
  using TileConfig = itk::TileConfiguration<Dimension>;

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, unit spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest0(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D    UO = createUnitOrigin2D(yMontageSize, xMontageSize);
    Spacing2D   US = createSpacing2D(yMontageSize, xMontageSize, 1);
    Transform2D UT = createUnitTransform2D(yMontageSize, xMontageSize);

    return stitchMontage(stageTiles, inputPath, outFilename, UO, US, UT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, dynamic spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest1(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D    UO = createUnitOrigin2D(yMontageSize, xMontageSize);
    Spacing2D   DS = createSpacing2D(yMontageSize, xMontageSize, 0.5);
    Transform2D UT = createUnitTransform2D(yMontageSize, xMontageSize);

    return stitchMontage(stageTiles, inputPath, outFilename, UO, DS, UT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, unit spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest2(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D    UO = createUnitOrigin2D(yMontageSize, xMontageSize);
    Spacing2D   US = createSpacing2D(yMontageSize, xMontageSize, 1);
    Transform2D DT = createTransform2DFromStageTiles(stageTiles);

    return stitchMontage(stageTiles, inputPath, outFilename, UO, US, DT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, dynamic spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest3(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D    UO = createUnitOrigin2D(yMontageSize, xMontageSize);
    Spacing2D   DS = createSpacing2D(yMontageSize, xMontageSize, 0.5);
    Transform2D DT = createTransform2DFromStageTiles(stageTiles);

    // Halve all the DT values to account for the spacing of 0.5
    for (TransformPtrRow & transform_row : DT)
    {
      for (typename MontageType::TransformPointer & transform : transform_row)
      {
        auto offset = transform->GetOffset();
        for (unsigned i = 0; i < Dimension; i++)
        {
          offset[i] = offset[i] / 2;
        }
        transform->SetOffset(offset);
      }
    }

    return stitchMontage(stageTiles, inputPath, outFilename, UO, DS, DT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, unit spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest4(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D    DO = createOrigin2DFromStageTiles(stageTiles);
    Spacing2D   US = createSpacing2D(yMontageSize, xMontageSize, 1);
    Transform2D UT = createUnitTransform2D(yMontageSize, xMontageSize);

    return stitchMontage(stageTiles, inputPath, outFilename, DO, US, UT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, dynamic spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest5(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    Origin2D DO = createOrigin2DFromStageTiles(stageTiles);

    // Halve all the DO values to account for the spacing of 0.5
    for (OriginRow & origin_row : DO)
    {
      for (OriginPoint & origin : origin_row)
      {
        for (unsigned i = 0; i < Dimension; i++)
        {
          origin[i] = origin[i] / 2;
        }
      }
    }

    Spacing2D   DS = createSpacing2D(yMontageSize, xMontageSize, 0.5);
    Transform2D UT = createUnitTransform2D(yMontageSize, xMontageSize);

    return stitchMontage(stageTiles, inputPath, outFilename, DO, DS, UT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, unit spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest6(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    // Create DO values so that the tiles start out [col * 100] pixels away from the correct position
    Origin2D DO(stageTiles.AxisSizes[1]);
    size_t   t = 0;
    for (unsigned col = 0; col < yMontageSize; col++)
    {
      DO[col].resize(stageTiles.AxisSizes[0]);
      for (unsigned row = 0; row < xMontageSize; row++)
      {
        PointType pos = stageTiles.Tiles[t].Position;
        for (unsigned i = 0; i < Dimension; i++)
        {
          // Get correct origin value, then add [col * 100] to it
          pos[i] = pos[i] + (col * 100);
        }
        DO[col][row] = pos;

        t++;
      }
    }

    Spacing2D US = createSpacing2D(yMontageSize, xMontageSize, 1);

    // Create DT values so that the tiles are translated back to their correct positions
    Transform2D DT(yMontageSize);
    t = 0;
    for (unsigned col = 0; col < yMontageSize; col++)
    {
      DT[col].resize(xMontageSize);
      for (unsigned row = 0; row < xMontageSize; row++)
      {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();

        auto offset = transform->GetOffset();
        for (unsigned i = 0; i < Dimension; i++)
        {
          offset[i] = (col * 100);
        }
        transform->SetOffset(offset);
        DT[col][row] = transform;

        t++;
      }
    }

    return stitchMontage(stageTiles, inputPath, outFilename, DO, US, DT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, dynamic spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest7(const TileConfig &  stageTiles,
                       const std::string & inputPath,
                       const std::string & outFilename,
                       unsigned            streamSubdivisions)
  {
    unsigned yMontageSize = stageTiles.AxisSizes[1];
    unsigned xMontageSize = stageTiles.AxisSizes[0];

    // Create DO values so that the tiles start out [col * 100] pixels away from the correct position
    Origin2D DO(stageTiles.AxisSizes[1]);
    size_t   t = 0;
    for (unsigned col = 0; col < yMontageSize; col++)
    {
      DO[col].resize(stageTiles.AxisSizes[0]);
      for (unsigned row = 0; row < xMontageSize; row++)
      {
        PointType pos = stageTiles.Tiles[t].Position;
        for (unsigned i = 0; i < Dimension; i++)
        {
          // Get correct origin value, divide by 2 to account for the 0.5 spacing, then add [col * 100]
          pos[i] = (pos[i] / 2) + (col * 100);
        }
        DO[col][row] = pos;

        t++;
      }
    }

    Spacing2D DS = createSpacing2D(yMontageSize, xMontageSize, 0.5);

    // Create DT values so that the tiles are translated back to their correct positions
    Transform2D DT(yMontageSize);
    t = 0;
    for (unsigned col = 0; col < yMontageSize; col++)
    {
      DT[col].resize(xMontageSize);
      for (unsigned row = 0; row < xMontageSize; row++)
      {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();

        auto offset = transform->GetOffset();
        for (unsigned i = 0; i < Dimension; i++)
        {
          offset[i] = (col * 100);
        }
        transform->SetOffset(offset);
        DT[col][row] = transform;

        t++;
      }
    }

    return stitchMontage(stageTiles, inputPath, outFilename, DO, DS, DT, streamSubdivisions);
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that executes a given montage test with a given origin2D, spacing2D, and transform2D
   * ----------------------------------------------------------------------------------------------- */
  int
  stitchMontage(const TileConfig &  stageTiles,
                const std::string & inputPath,
                const std::string & outFilename,
                Origin2D            origin2D,
                Spacing2D           spacing2D,
                Transform2D         transform2D,
                unsigned            streamSubdivisions)
  {
    itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New());
    unsigned yMontageSize = origin2D.size();
    unsigned xMontageSize = origin2D[0].size();

    // write generated mosaic
    typename MontageType::TileIndexType ind;
    using Resampler = itk::TileMergeImageFilter<OriginalImageType, AccumulatePixelType>;
    typename Resampler::Pointer resampleF = Resampler::New();
    itk::SimpleFilterWatcher    fw2(resampleF, "resampler");
    resampleF->SetMontageSize({ xMontageSize, yMontageSize });
    unsigned t = 0;
    for (unsigned y = 0; y < yMontageSize; y++)
    {
      ind[1] = y;
      for (unsigned x = 0; x < xMontageSize; x++)
      {
        ind[0] = x;
        std::string filename = inputPath + stageTiles.Tiles[t].FileName;

        typename OriginalImageType::Pointer     image = ReadImage<typename Resampler::ImageType>(filename.c_str());
        typename OriginalImageType::PointType   origin = origin2D[y][x];
        typename OriginalImageType::SpacingType spacing = spacing2D[y][x];
        image->SetOrigin(origin);
        image->SetSpacing(spacing);
        resampleF->SetInputTile(ind, image);

        typename MontageType::TransformConstPointer transform = transform2D[y][x];
        resampleF->SetTileTransform(ind, transform);
        t++;
      }
    }

    resampleF->Update();

    using WriterType = itk::ImageFileWriter<OriginalImageType>;
    typename WriterType::Pointer w = WriterType::New();
    w->SetInput(resampleF->GetOutput());
    // resampleF->DebugOn(); //generate an image of contributing regions
    w->SetFileName(outFilename);
    // w->UseCompressionOn();
    w->SetNumberOfStreamDivisions(streamSubdivisions);
    w->Update();

    return EXIT_SUCCESS;
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

  Origin2D
  createUnitOrigin2D(unsigned numOfRows, unsigned numOfCols)
  {
    Origin2D UO;

    OriginPoint UO_point;
    for (unsigned i = 0; i < Dimension; i++)
    {
      UO_point[i] = 0;
    }

    for (unsigned i = 0; i < numOfRows; i++)
    {
      OriginRow row;
      for (unsigned j = 0; j < numOfCols; j++)
      {
        row.push_back(UO_point);
      }
      UO.push_back(row);
    }

    return UO;
  }

  Origin2D
  createOrigin2DFromStageTiles(TileConfig stageTiles)
  {
    Origin2D UO(stageTiles.AxisSizes[1]);

    for (size_t y = 0; y < stageTiles.AxisSizes[1]; y++)
    {
      UO[y].resize(stageTiles.AxisSizes[0]);
      for (size_t x = 0; x < stageTiles.AxisSizes[0]; x++)
      {
        UO[y][x] = stageTiles.Tiles[stageTiles.nDIndexToLinearIndex({ x, y })].Position;
      }
    }

    return UO;
  }

  Spacing2D
  createSpacing2D(unsigned numOfRows, unsigned numOfCols, double value)
  {
    Spacing2D US;

    SpacingType spacing;
    for (unsigned i = 0; i < Dimension; i++)
    {
      spacing[i] = value;
    }

    for (unsigned i = 0; i < numOfRows; i++)
    {
      SpacingRow row;
      for (unsigned j = 0; j < numOfCols; j++)
      {
        row.push_back(spacing);
      }
      US.push_back(row);
    }

    return US;
  }

  Transform2D
  createUnitTransform2D(unsigned numOfRows, unsigned numOfCols)
  {
    Transform2D UT;

    for (unsigned i = 0; i < numOfRows; i++)
    {
      TransformPtrRow transform_row;
      for (unsigned j = 0; j < numOfCols; j++)
      {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();
        transform_row.push_back(transform);
      }
      UT.push_back(transform_row);
    }

    return UT;
  }

  Transform2D
  createTransform2DFromStageTiles(TileConfig stageTiles)
  {
    Transform2D UT(stageTiles.AxisSizes[1]);

    // initialize
    for (size_t y = 0; y < stageTiles.AxisSizes[1]; y++)
    {
      UT[y].resize(stageTiles.AxisSizes[0]);
    }

    // fill with translations
    typename TileConfig::TileIndexType ind;
    for (size_t t = 0; t < stageTiles.LinearSize(); t++)
    {
      typename MontageType::TransformPointer transform = MontageType::TransformType::New();
      ind = stageTiles.LinearIndexToNDIndex(t);
      auto offset = transform->GetOffset();
      for (unsigned d = 0; d < MontageType::TransformType::SpaceDimension; d++)
      {
        offset[d] = -stageTiles.Tiles[t].Position[d];
      }
      transform->SetOffset(offset);
      UT[ind[1]][ind[0]] = transform;
    }

    return UT;
  }
};

#endif // itkInMemoryMontageTestHelper_hxx
