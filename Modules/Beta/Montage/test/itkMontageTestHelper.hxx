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
#include "itkSimpleFilterWatcher.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

#include <fstream>
#include <iomanip>
#include <type_traits>

template< typename TransformType >
void
WriteTransform( const TransformType* transform, std::string filename )
{
  using AffineType = itk::AffineTransform< double, 3 >;
  using TransformWriterType = itk::TransformFileWriterTemplate< double >;
  TransformWriterType::Pointer tWriter = TransformWriterType::New();
  tWriter->SetFileName( filename );

  if ( TransformType::SpaceDimension >= 2 || TransformType::SpaceDimension <= 3 )
    { // convert into affine which Slicer can read
    AffineType::Pointer aTr = AffineType::New();
    AffineType::TranslationType t;
    t.Fill( 0 );
    for ( unsigned i = 0; i < TransformType::SpaceDimension; i++ )
      {
      t[i] = transform->GetOffset()[i];
      }
    aTr->SetTranslation( t );
    tWriter->SetInput( aTr );
    }
  else
    {
    tWriter->SetInput( transform );
    }
  tWriter->Update();
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

// do the registrations and calculate registration errors
// negative peakMethodToUse means to try them all
// streamSubdivisions of 1 disables streaming (higher memory useage, less cluttered debug output)
template< typename PixelType, typename AccumulatePixelType >
int
montageTest( const itk::TileLayout2D& stageTiles, const itk::TileLayout2D& actualTiles,
             const std::string& inputPath, const std::string& outFilename, bool varyPaddingMethods,
             int peakMethodToUse, bool loadIntoMemory, unsigned streamSubdivisions )
{
  int result = EXIT_SUCCESS;
  using ScalarPixelType = typename itk::NumericTraits< PixelType >::ValueType;
  constexpr unsigned Dimension = 2;
  using PointType = itk::Point< double, Dimension >;
  using VectorType = itk::Vector< double, Dimension >;
  using TransformType = itk::TranslationTransform< double, Dimension >;
  using ScalarImageType = itk::Image< ScalarPixelType, Dimension >;
  using OriginalImageType = itk::Image< PixelType, Dimension >; // possibly RGB instead of scalar
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod< ScalarImageType, ScalarImageType >;
  using PadMethodUnderlying = typename std::underlying_type< typename PCMType::PaddingMethod >::type;
  typename ScalarImageType::SpacingType sp;
  sp.Fill( 1.0 ); // OMC test assumes unit spacing, tiles test has explicit unit spacing
  itk::ObjectFactoryBase::RegisterFactory( itk::TxtTransformIOFactory::New() );
  unsigned yMontageSize = stageTiles.size();
  unsigned xMontageSize = stageTiles[0].size();

  using PeakInterpolationType = typename itk::MaxPhaseCorrelationOptimizer< PCMType >::PeakInterpolationMethod;
  using PeakFinderUnderlying = typename std::underlying_type< PeakInterpolationType >::type;

  for ( auto padMethod = static_cast< PadMethodUnderlying >( PCMType::PaddingMethod::Zero );
        padMethod <= static_cast< PadMethodUnderlying >( PCMType::PaddingMethod::Last );
        padMethod++ )
    {
    if ( !varyPaddingMethods ) // go straight to the last, best method
      {
      padMethod = static_cast< PadMethodUnderlying >( PCMType::PaddingMethod::Last );
      }
    std::ofstream registrationErrors( outFilename + std::to_string( padMethod ) + ".tsv" );
    std::cout << "Padding method " << padMethod << std::endl;
    registrationErrors << "PeakInterpolationMethod";
    for ( unsigned d = 0; d < Dimension; d++ )
      {
      registrationErrors << '\t' << char( 'x' + d ) << "Tile";
      }
    for ( unsigned d = 0; d < Dimension; d++ )
      {
      registrationErrors << '\t' << char( 'x' + d ) << "Error";
      }
    registrationErrors << std::endl;


    using MontageType = itk::TileMontage< ScalarImageType >;
    typename MontageType::Pointer montage = MontageType::New();
    auto paddingMethod = static_cast< typename PCMType::PaddingMethod >( padMethod );
    montage->GetModifiablePCM()->SetPaddingMethod( paddingMethod );
    montage->SetMontageSize( { xMontageSize, yMontageSize } );
    montage->SetOriginAdjustment( stageTiles[1][1].Position );
    montage->SetForcedSpacing( sp );

    typename MontageType::TileIndexType ind;
    for ( unsigned y = 0; y < yMontageSize; y++ )
      {
      ind[1] = y;
      for ( unsigned x = 0; x < xMontageSize; x++ )
        {
        ind[0] = x;
        std::string filename = inputPath + stageTiles[y][x].FileName;
        if (loadIntoMemory)
          {
          montage->SetInputTile( ind, ReadImage< typename MontageType::ImageType >( filename.c_str() ) );
          }
        else
          {
          montage->SetInputTile( ind, filename );
          }
        }
      }

    for ( auto peakMethod = static_cast< PeakFinderUnderlying >( PeakInterpolationType::None );
          peakMethod <= static_cast< PeakFinderUnderlying >( PeakInterpolationType::Last );
          peakMethod++ )
      {
      if ( peakMethodToUse >= 0 )
        {
        peakMethod = static_cast< PeakFinderUnderlying >( peakMethodToUse );
        }
      montage->GetModifiablePCMOptimizer()->SetPeakInterpolationMethod(
        static_cast< PeakInterpolationType >( peakMethod ) );
      montage->Modified(); // optimizer is not an "input" to PCM
      // so its modification does not cause a pipeline update automatically

      std::cout << "    PeakMethod " << peakMethod << std::endl;
      itk::SimpleFilterWatcher fw( montage, "montage" );
      montage->Update();

      std::cout << std::fixed;

      double totalError = 0.0;
      for ( unsigned y = 0; y < yMontageSize; y++ )
        {
        ind[1] = y;
        for ( unsigned x = 0; x < xMontageSize; x++ )
          {
          ind[0] = x;
          const TransformType* regTr = montage->GetOutputTransform( ind );

          registrationErrors << peakMethod << '\t' << x << '\t' << y;

          // calculate error
          VectorType tr = regTr->GetOffset(); // translation measured by registration
          VectorType ta = stageTiles[y][x].Position - actualTiles[y][x].Position; // translation (actual)
          PointType  p0;
          p0.Fill( 0 );
          ta += actualTiles[0][0].Position - p0; // account for tile zero maybe not being at coordinates 0
          double singleError = 0.0;
          for ( unsigned d = 0; d < Dimension; d++ )
            {
            registrationErrors << '\t' << ( tr[d] - ta[d] );
            std::cout << "  " << std::setw( 8 ) << std::setprecision( 3 ) << ( tr[d] - ta[d] );
            singleError += std::abs( tr[d] - ta[d] );
            }
          totalError += singleError;
          registrationErrors << std::endl;
          WriteTransform( regTr, outFilename + std::to_string( padMethod ) + "_" + std::to_string( peakMethod )
                          + "_Tr_" + std::to_string( x ) + "_" + std::to_string( y ) + ".tfm" );
          }
        }
      double avgError = totalError / ( xMontageSize * ( yMontageSize - 1 ) + ( xMontageSize - 1 ) * yMontageSize );
      avgError /= Dimension; // report per-dimension error
      std::cout << "\nAverage translation error for padding method " << padMethod
                << " and peak interpolation method " << peakMethod << ": " << avgError << std::endl;
      if ( avgError >= 1.0 )
        {
        result = EXIT_FAILURE;
        }
      // write generated mosaic
      using Resampler = itk::TileMergeImageFilter< OriginalImageType, AccumulatePixelType >;
      typename Resampler::Pointer resampleF = Resampler::New();
      itk::SimpleFilterWatcher fw2( resampleF, "resampler" );
      resampleF->SetMontageSize( { xMontageSize, yMontageSize } );
      resampleF->SetOriginAdjustment( stageTiles[1][1].Position );
      resampleF->SetForcedSpacing( sp );
      for ( unsigned y = 0; y < yMontageSize; y++ )
        {
        ind[1] = y;
        for ( unsigned x = 0; x < xMontageSize; x++ )
          {
          ind[0] = x;
          std::string filename = inputPath + stageTiles[y][x].FileName;
          if ( loadIntoMemory )
            {
            resampleF->SetInputTile( ind, ReadImage< typename Resampler::ImageType >( filename.c_str() ) );
            }
          else
            {
            resampleF->SetInputTile( ind, filename );
            }
          resampleF->SetTileTransform( ind, montage->GetOutputTransform( ind ) );
          }
        }

      // resampleF->Update();
      using WriterType = itk::ImageFileWriter< OriginalImageType >;
      typename WriterType::Pointer w = WriterType::New();
      w->SetInput( resampleF->GetOutput() );
      // resampleF->DebugOn(); //generate an image of contributing regions
      // MetaImage format supports streaming
      w->SetFileName( outFilename + std::to_string( padMethod ) + "_" + std::to_string( peakMethod ) + ".mha" );
      // w->UseCompressionOn();
      w->SetNumberOfStreamDivisions( streamSubdivisions );
      w->Update();
      if ( peakMethodToUse >= 0 ) // peak method was specified
        {
        break; // do not try them all
        }
      }

    std::cout << std::endl;
    }
  return result;
}

#endif // itkMontageTestHelper_hxx
