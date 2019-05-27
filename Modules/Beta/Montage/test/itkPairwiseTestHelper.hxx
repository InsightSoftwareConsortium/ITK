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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkParseTileConfiguration.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"

#include <array>
#include <fstream>
#include <iomanip>
#include <type_traits>

// do the registration and calculate error for two images
template< typename PixelType >
double
calculateError( const itk::TileLayout2D& stageTiles, const itk::TileLayout2D& actualTiles,
                const std::string& inputPath, int paddingMethod, std::ostream& out,
                unsigned xF, unsigned yF, unsigned xM, unsigned yM)
{
  double translationError = 0.0;
  std::cout << stageTiles[yF][xF].FileName << " <- " << stageTiles[yM][xM].FileName << std::endl;

  constexpr unsigned Dimension = 2;
  using ImageType = itk::Image< PixelType, Dimension >;
  using ReaderType = itk::ImageFileReader< ImageType >;
  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( inputPath + stageTiles[yF][xF].FileName );
  reader->Update();
  typename ImageType::Pointer fixedImage = reader->GetOutput();
  fixedImage->DisconnectPipeline();

  reader->SetFileName( inputPath + stageTiles[yM][xM].FileName );
  reader->Update();
  typename ImageType::Pointer movingImage = reader->GetOutput();
  movingImage->DisconnectPipeline();

  // adjust origins (assume 0 origins in files)
  typename ImageType::SpacingType sp = fixedImage->GetSpacing();
  typename ImageType::PointType origin = stageTiles[yF][xF].Position;
  for ( unsigned d = 0; d < Dimension; d++ )
    {
    origin[d] *= sp[d];
    }
  fixedImage->SetOrigin( origin );
  sp = movingImage->GetSpacing();
  origin = stageTiles[yM][xM].Position;
  for ( unsigned d = 0; d < Dimension; d++ )
    {
    origin[d] *= sp[d];
    }
  movingImage->SetOrigin( origin );

  // execute registration
  using PhaseCorrelationMethodType = itk::PhaseCorrelationImageRegistrationMethod< ImageType, ImageType >;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage( fixedImage );
  phaseCorrelationMethod->SetMovingImage( movingImage );
  typename PhaseCorrelationMethodType::SizeType pad;
  pad.Fill( 8 * sizeof( PixelType ) );
  phaseCorrelationMethod->SetObligatoryPadding( pad );
  // phaseCorrelationMethod->DebugOn();

  using PMType = typename PhaseCorrelationMethodType::PaddingMethod;
  using PadMethodUnderlying = typename std::underlying_type< PMType >::type;
  static_assert( std::is_same< decltype( paddingMethod ), PadMethodUnderlying >::value,
      "We expect type of paddingMethod to be equal to PadMethodUnderlying type" );
  // cause compile error if this ever changes
  // to correct it, change type of paddingMethod parameter
  auto padMethod = static_cast< PMType >( paddingMethod );
  phaseCorrelationMethod->SetPaddingMethod( padMethod );

  using OperatorType = itk::PhaseCorrelationOperator< typename itk::NumericTraits< PixelType >::RealType, Dimension >;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator( pcmOperator );

  using OptimizerType = itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >;
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  phaseCorrelationMethod->SetOptimizer( pcmOptimizer );

  using PeakInterpolationType =
    typename itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >::PeakInterpolationMethod;
  using PeakFinderUnderlying = typename std::underlying_type< PeakInterpolationType >::type;

  unsigned count = 0;
  for ( auto peakMethod = static_cast< PeakFinderUnderlying >( PeakInterpolationType::None );
        peakMethod <= static_cast< PeakFinderUnderlying >( PeakInterpolationType::Last );
        peakMethod++ )
    {
    pcmOptimizer->SetPeakInterpolationMethod( static_cast< PeakInterpolationType >( peakMethod ) );
    phaseCorrelationMethod->Modified(); // optimizer is not an "input" to PCM
    // so its modification does not cause a pipeline update automatically

    out << std::to_string( xF ) + "," + std::to_string( yF ) + " <- " + std::to_string( xM ) + "," + std::to_string( yM );
    out << '\t' << peakMethod;
    std::cout << "    PeakMethod" << peakMethod << ":";

    phaseCorrelationMethod->Update();

    using TransformType = itk::TranslationTransform< double, Dimension >;
    static_assert( std::is_same< TransformType, typename PhaseCorrelationMethodType::TransformType >::value,
      "PhaseCorrelationMethod's TransformType is expected to be a TranslationTransform" );
    const TransformType* regTr = phaseCorrelationMethod->GetOutput()->Get();

    // calculate error
    using VectorType = itk::Vector< double, Dimension >;
    VectorType tr = regTr->GetOffset(); // translation measured by registration
    for ( unsigned d = 0; d < Dimension; d++ )
      {
      tr[d] /= sp[d];
      }
    VectorType ta = ( actualTiles[yF][xF].Position - stageTiles[yF][xF].Position ) -
                    ( actualTiles[yM][xM].Position - stageTiles[yM][xM].Position ); // translation (actual)
    for ( unsigned d = 0; d < Dimension; d++ )
      {
      out << '\t' << ( tr[d] - ta[d] );
      std::cout << "  " << std::setw( 8 ) << std::setprecision( 3 ) << ( tr[d] - ta[d] );
      translationError += std::abs( tr[d] - ta[d] );
      }
    out << std::endl;
    count++;
    }
  std::cout << std::endl;

  return translationError / count;
} // calculateError

// do the registrations and calculate registration errors
template< typename PixelType >
int
pairwiseTests( const itk::TileLayout2D& stageTiles, const itk::TileLayout2D& actualTiles,
               const std::string& inputPath, const std::string& outFilename, bool varyPaddingMethods )
{
  int result = EXIT_SUCCESS;
  constexpr unsigned Dimension = 2;
  using ImageType = itk::Image< PixelType, Dimension >;
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod< ImageType, ImageType >;
  using PadMethodUnderlying = typename std::underlying_type< typename PCMType::PaddingMethod >::type;

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
    registrationErrors << "Fixed <- Moving\tPeakInterpolationMethod";
    for ( unsigned d = 0; d < Dimension; d++ )
      {
      registrationErrors << '\t' << char( 'x' + d ) << "Error";
      }
    registrationErrors << std::endl;

    unsigned yMontageSize = stageTiles.size();
    unsigned xMontageSize = stageTiles[0].size();
    double totalError = 0.0;
    for ( unsigned y = 0; y < yMontageSize; y++ )
      {
      for ( unsigned x = 0; x < xMontageSize; x++ )
        {
        if ( x > 0 )
          {
          totalError += calculateError< PixelType >(
              stageTiles, actualTiles, inputPath, padMethod, registrationErrors, x - 1, y, x, y );
          }
        if ( y > 0 )
          {
          totalError += calculateError< PixelType >(
              stageTiles, actualTiles, inputPath, padMethod, registrationErrors, x, y - 1, x, y );
          }
        }
      }

    double avgError = totalError / ( xMontageSize * ( yMontageSize - 1 ) + ( xMontageSize - 1 ) * yMontageSize );
    avgError /= Dimension; // report per-dimension error
    std::cout << "Average translation error for padding method "
      << padMethod << ": " << avgError << std::endl << std::endl;
    if ( avgError >= 1.0 )
      {
      result = EXIT_FAILURE;
      }
    }
  return result;
}

#endif // itkMockMontageHelper_hxx
