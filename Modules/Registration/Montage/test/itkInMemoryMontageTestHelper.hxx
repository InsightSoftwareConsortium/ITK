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

#ifndef itkInMemoryMontageTestHelper_hxx
#define itkInMemoryMontageTestHelper_hxx

#include "itkAffineTransform.h"
#include "itkImageFileWriter.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkObject.h"
#include "itkParseTileConfiguration.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

#include <fstream>
#include <iomanip>
#include <type_traits>

template< typename PixelType, typename AccumulatePixelType >
class ITK_TEMPLATE_EXPORT InMemoryMontageTest : public itk::Object
{
public:
  InMemoryMontageTest() = default;
  ~InMemoryMontageTest() = default;

  ITK_DISALLOW_COPY_AND_ASSIGN( InMemoryMontageTest );

  /** Standard class type aliases. */
  using Self = InMemoryMontageTest;
  using Pointer = itk::SmartPointer< Self >;
  using ConstPointer = itk::SmartPointer< const Self >;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self )

    enum class TestVariation : unsigned int {
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
  execute( const itk::TileLayout2D& stageTiles, const std::string& inputPath, std::string& outFilename,
           TestVariation variation, unsigned streamSubdivisions )
  {
    // MetaImage format supports streaming
    outFilename = outFilename + std::to_string( static_cast< unsigned int >( variation ) ) + ".mha";

    switch ( variation )
      {
      case TestVariation::UOrigin_USpacing_UTransform:
        return inMemoryMontageTest0( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::UOrigin_DSpacing_UTransform:
        return inMemoryMontageTest1( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::UOrigin_USpacing_DTransform:
        return inMemoryMontageTest2( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::UOrigin_DSpacing_DTransform:
        return inMemoryMontageTest3( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::DOrigin_USpacing_UTransform:
        return inMemoryMontageTest4( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::DOrigin_DSpacing_UTransform:
        return inMemoryMontageTest5( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::DOrigin_USpacing_DTransform:
        return inMemoryMontageTest6( stageTiles, inputPath, outFilename, streamSubdivisions );
      case TestVariation::DOrigin_DSpacing_DTransform:
        return inMemoryMontageTest7( stageTiles, inputPath, outFilename, streamSubdivisions );
      default:
        return EXIT_FAILURE;
      }
  }

private:
  static constexpr unsigned Dimension = 2;
  using ScalarPixelType = typename itk::NumericTraits< PixelType >::ValueType;
  using PointType = itk::Point< double, Dimension >;
  using VectorType = itk::Vector< double, Dimension >;
  using ScalarImageType = itk::Image< ScalarPixelType, Dimension >;
  using SpacingType = typename ScalarImageType::SpacingType;
  using SpacingRow = std::vector< SpacingType >;
  using Spacing2D = std::vector< SpacingRow >;
  using MontageType = itk::TileMontage< ScalarImageType >;
  using TransformPtr = typename MontageType::TransformPointer;
  using TransformPtrRow = std::vector< TransformPtr >;
  using Transform2D = std::vector< TransformPtrRow >;
  using OriginPoint = typename ScalarImageType::PointType;
  using OriginRow = std::vector< OriginPoint >;
  using Origin2D = std::vector< OriginRow >;
  using OriginalImageType = itk::Image< PixelType, Dimension >; // possibly RGB instead of scalar
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod< ScalarImageType, ScalarImageType >;
  using PadMethodUnderlying = typename std::underlying_type< typename PCMType::PaddingMethod >::type;
  using TransformType = itk::TranslationTransform< double, Dimension >;
  using StageTiles = itk::TileLayout2D;

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, unit spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest0( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D UO = createUnitOrigin2D( yMontageSize, xMontageSize );
    Spacing2D US = createSpacing2D( yMontageSize, xMontageSize, 1 );
    Transform2D UT = createUnitTransform2D( yMontageSize, xMontageSize );

    return stitchMontage( stageTiles, inputPath, outFilename, UO, US, UT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, dynamic spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest1( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D UO = createUnitOrigin2D( yMontageSize, xMontageSize );
    Spacing2D DS = createSpacing2D( yMontageSize, xMontageSize, 0.5 );
    Transform2D UT = createUnitTransform2D( yMontageSize, xMontageSize );

    return stitchMontage( stageTiles, inputPath, outFilename, UO, DS, UT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, unit spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest2( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D UO = createUnitOrigin2D( yMontageSize, xMontageSize );
    Spacing2D US = createSpacing2D( yMontageSize, xMontageSize, 1 );
    Transform2D DT = createTransform2DFromStageTiles( stageTiles );

    return stitchMontage( stageTiles, inputPath, outFilename, UO, US, DT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a unit origin, dynamic spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest3( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D UO = createUnitOrigin2D( yMontageSize, xMontageSize );
    Spacing2D DS = createSpacing2D( yMontageSize, xMontageSize, 0.5 );
    Transform2D DT = createTransform2DFromStageTiles( stageTiles );

    // Halve all the DT values to account for the spacing of 0.5
    for ( TransformPtrRow& transform_row : DT )
      {
      for ( typename MontageType::TransformPointer& transform : transform_row )
        {
        auto offset = transform->GetOffset();
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          offset[i] = offset[i] / 2;
          }
        transform->SetOffset( offset );
        }
      }

    return stitchMontage( stageTiles, inputPath, outFilename, UO, DS, DT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, unit spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest4( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D DO = createOrigin2DFromStageTiles( stageTiles );
    Spacing2D US = createSpacing2D( yMontageSize, xMontageSize, 1 );
    Transform2D UT = createUnitTransform2D( yMontageSize, xMontageSize );

    return stitchMontage( stageTiles, inputPath, outFilename, DO, US, UT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, dynamic spacing, and unit transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest5( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    Origin2D DO = createOrigin2DFromStageTiles( stageTiles );

    // Halve all the DO values to account for the spacing of 0.5
    for ( OriginRow& origin_row : DO )
      {
      for ( OriginPoint& origin : origin_row )
        {
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          origin[i] = origin[i] / 2;
          }
        }
      }

    Spacing2D DS = createSpacing2D( yMontageSize, xMontageSize, 0.5 );
    Transform2D UT = createUnitTransform2D( yMontageSize, xMontageSize );

    return stitchMontage( stageTiles, inputPath, outFilename, DO, DS, UT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, unit spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest6( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    // Create DO values so that the tiles start out [col * 100] pixels away from the correct position
    Origin2D DO;
    for ( itk::TileRow2D tileRow : stageTiles )
      {
      OriginRow row;
      for ( unsigned col = 0; col < tileRow.size(); col++ )
        {
        itk::Tile2D tile = tileRow[col];
        itk::Point< double, Dimension > pos = tile.Position;
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          // Get correct origin value, then add [col * 100] to it
          pos[i] = pos[i] + ( col * 100 );
          }
        row.push_back( pos );
        }
      DO.push_back( row );
      }

    Spacing2D US = createSpacing2D( yMontageSize, xMontageSize, 1 );

    // Create DT values so that the tiles are translated back to their correct positions
    Transform2D DT;
    for ( unsigned row = 0; row < yMontageSize; row++ )
      {
      TransformPtrRow transform_row;
      for ( unsigned col = 0; col < xMontageSize; col++ )
        {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();
        auto offset = transform->GetOffset();
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          offset[i] = ( col * 100 );
          }
        transform->SetOffset( offset );
        transform_row.push_back( transform );
        }
      DT.push_back( transform_row );
      }

    return stitchMontage( stageTiles, inputPath, outFilename, DO, US, DT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that creates a dynamic origin, dynamic spacing, and dynamic transform and calls the test
   * ----------------------------------------------------------------------------------------------- */
  int
  inMemoryMontageTest7( const itk::TileLayout2D& stageTiles, const std::string& inputPath,
                        const std::string& outFilename, unsigned streamSubdivisions )
  {
    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();

    // Create DO values so that the tiles start out [col * 100] pixels away from the correct position
    Origin2D DO;
    for ( itk::TileRow2D tileRow : stageTiles )
      {
      OriginRow row;
      for ( unsigned col = 0; col < tileRow.size(); col++ )
        {
        itk::Tile2D tile = tileRow[col];
        itk::Point< double, Dimension > pos = tile.Position;
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          // Get correct origin value, divide by 2 to account for the 0.5 spacing, then add [col * 100]
          pos[i] = ( pos[i] / 2 ) + ( col * 100 );
          }
        row.push_back( pos );
        }
      DO.push_back( row );
      }

    Spacing2D DS = createSpacing2D( yMontageSize, xMontageSize, 0.5 );

    // Create DT values so that the tiles are translated back to their correct positions
    Transform2D DT;
    for ( unsigned row = 0; row < yMontageSize; row++ )
      {
      TransformPtrRow transform_row;
      for ( unsigned col = 0; col < xMontageSize; col++ )
        {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();
        auto offset = transform->GetOffset();
        for ( unsigned i = 0; i < Dimension; i++ )
          {
          offset[i] = ( col * 100 );
          }
        transform->SetOffset( offset );
        transform_row.push_back( transform );
        }
      DT.push_back( transform_row );
      }

    return stitchMontage( stageTiles, inputPath, outFilename, DO, DS, DT, streamSubdivisions );
  }

  /* -----------------------------------------------------------------------------------------------
   * Helper Method that executes a given montage test with a given origin2D, spacing2D, and transform2D
   * ----------------------------------------------------------------------------------------------- */
  int
  stitchMontage( const itk::TileLayout2D& stageTiles, const std::string& inputPath, const std::string& outFilename,
                 Origin2D origin2D, Spacing2D spacing2D, Transform2D transform2D, unsigned streamSubdivisions )
  {
    itk::ObjectFactoryBase::RegisterFactory( itk::TxtTransformIOFactory::New() );
    unsigned yMontageSize = origin2D.size();
    unsigned xMontageSize = origin2D[0].size();

    // write generated mosaic
    typename MontageType::TileIndexType ind;
    using Resampler = itk::TileMergeImageFilter< OriginalImageType, AccumulatePixelType >;
    typename Resampler::Pointer resampleF = Resampler::New();
    itk::SimpleFilterWatcher    fw2( resampleF, "resampler" );
    resampleF->SetMontageSize( { xMontageSize, yMontageSize } );
    for ( unsigned y = 0; y < yMontageSize; y++ )
      {
      ind[1] = y;
      for ( unsigned x = 0; x < xMontageSize; x++ )
        {
        ind[0] = x;
        std::string filename = inputPath + stageTiles[y][x].FileName;

        typename OriginalImageType::Pointer image = ReadImage< typename Resampler::ImageType >( filename.c_str() );
        typename OriginalImageType::PointType   origin = origin2D[y][x];
        typename OriginalImageType::SpacingType spacing = spacing2D[y][x];
        image->SetOrigin( origin );
        image->SetSpacing( spacing );
        resampleF->SetInputTile( ind, image );

        typename MontageType::TransformConstPointer transform = transform2D[y][x];
        resampleF->SetTileTransform( ind, transform );
        }
      }

    resampleF->Update();

    using WriterType = itk::ImageFileWriter< OriginalImageType >;
    typename WriterType::Pointer w = WriterType::New();
    w->SetInput( resampleF->GetOutput() );
    // resampleF->DebugOn(); //generate an image of contributing regions
    w->SetFileName( outFilename );
    // w->UseCompressionOn();
    w->SetNumberOfStreamDivisions( streamSubdivisions );
    w->Update();

    return EXIT_SUCCESS;
  }

  template< typename TImage >
  typename TImage::Pointer
  ReadImage( const char* filename )
  {
    using ReaderType = itk::ImageFileReader< TImage >;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( filename );
    reader->Update();
    return reader->GetOutput();
  }

  Origin2D
  createUnitOrigin2D( unsigned numOfRows, unsigned numOfCols )
  {
    Origin2D UO;

    OriginPoint UO_point;
    for ( unsigned i = 0; i < Dimension; i++ )
      {
      UO_point[i] = 0;
      }

    for ( unsigned i = 0; i < numOfRows; i++ )
      {
      OriginRow row;
      for ( unsigned j = 0; j < numOfCols; j++ )
        {
        row.push_back( UO_point );
        }
      UO.push_back( row );
      }

    return UO;
  }

  Origin2D
  createOrigin2DFromStageTiles( itk::TileLayout2D stageTiles )
  {
    Origin2D UO;

    for ( itk::TileRow2D tileRow : stageTiles )
      {
      OriginRow row;
      for ( itk::Tile2D tile : tileRow )
        {
        row.push_back( tile.Position );
        }
      UO.push_back( row );
      }

    return UO;
  }

  Spacing2D
  createSpacing2D( unsigned numOfRows, unsigned numOfCols, double value )
  {
    Spacing2D US;

    SpacingType spacing;
    for ( unsigned i = 0; i < Dimension; i++ )
      {
      spacing[i] = value;
      }

    for ( unsigned i = 0; i < numOfRows; i++ )
      {
      SpacingRow row;
      for ( unsigned j = 0; j < numOfCols; j++ )
        {
        row.push_back( spacing );
        }
      US.push_back( row );
      }

    return US;
  }

  Transform2D
  createUnitTransform2D( unsigned numOfRows, unsigned numOfCols )
  {
    Transform2D UT;

    for ( unsigned i = 0; i < numOfRows; i++ )
      {
      TransformPtrRow transform_row;
      for ( unsigned j = 0; j < numOfCols; j++ )
        {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();
        transform_row.push_back( transform );
        }
      UT.push_back( transform_row );
      }

    return UT;
  }

  Transform2D
  createTransform2DFromStageTiles( itk::TileLayout2D stageTiles )
  {
    Transform2D UT;

    for ( itk::TileRow2D tileRow : stageTiles )
      {
      TransformPtrRow transform_row;
      for ( itk::Tile2D tile : tileRow )
        {
        typename MontageType::TransformPointer transform = MontageType::TransformType::New();

        auto offset = transform->GetOffset();
        for ( unsigned i = 0; i < MontageType::TransformType::SpaceDimension; i++ )
          {
          offset[i] = -tile.Position[i];
          }

        transform->SetOffset( offset );

        transform_row.push_back( transform );
        }
      UT.push_back( transform_row );
      }

    return UT;
  }
};

#endif // itkInMemoryMontageTestHelper_hxx
