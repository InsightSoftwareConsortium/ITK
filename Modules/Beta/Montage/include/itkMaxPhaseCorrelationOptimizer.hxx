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
#ifndef itkMaxPhaseCorrelationOptimizer_hxx
#define itkMaxPhaseCorrelationOptimizer_hxx

#include "itkMaxPhaseCorrelationOptimizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

#include <cmath>
#include <type_traits>

/*
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 */

#ifndef NDEBUG
#include "itkImageFileWriter.h"

namespace
{
template< typename TImage >
void WriteDebug(const TImage* out, const char *filename)
{
  using WriterType = itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
    {
    w->Update();
    }
  catch (itk::ExceptionObject & error)
    {
    std::cerr << error << std::endl;
    }
}
}
#else
namespace
{
template< typename TImage >
void WriteDebug(TImage*, const char *) {}
}
#endif

namespace itk
{
template < typename TRegistrationMethod >
MaxPhaseCorrelationOptimizer<TRegistrationMethod>
::MaxPhaseCorrelationOptimizer() : Superclass()
{
  m_MaxCalculator = MaxCalculatorType::New();
}


template< typename TRegistrationMethod >
void
MaxPhaseCorrelationOptimizer< TRegistrationMethod >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "MaxCalculator: " << m_MaxCalculator << std::endl;
  auto pim = static_cast< typename std::underlying_type< PeakInterpolationMethod >::type >( m_PeakInterpolationMethod );
  os << indent << "PeakInterpolationMethod: " << pim << std::endl;
  os << indent << "ZeroSuppression: " << m_ZeroSuppression << std::endl;
  os << indent << "BiasTowardsExpected: " << m_BiasTowardsExpected << std::endl;
}

template< typename TRegistrationMethod >
void
MaxPhaseCorrelationOptimizer< TRegistrationMethod >
::SetPeakInterpolationMethod( const PeakInterpolationMethod peakInterpolationMethod )
{
  if ( this->m_PeakInterpolationMethod != peakInterpolationMethod )
    {
    this->m_PeakInterpolationMethod = peakInterpolationMethod;
    this->Modified();
    }
}

template< typename TRegistrationMethod >
void
MaxPhaseCorrelationOptimizer< TRegistrationMethod >
::ComputeOffset()
{
  ImageConstPointer input = static_cast< ImageType* >( this->GetInput( 0 ) );
  ImageConstPointer fixed = static_cast< ImageType* >( this->GetInput( 1 ) );
  ImageConstPointer moving = static_cast< ImageType* >( this->GetInput( 2 ) );

  OffsetType offset;
  offset.Fill( 0 );

  if ( !input )
    {
    return;
    }

  const typename ImageType::RegionType lpr = input->GetLargestPossibleRegion();
  const typename ImageType::SizeType size = lpr.GetSize();
  const typename ImageType::IndexType oIndex = lpr.GetIndex();

  const typename ImageType::SpacingType spacing = input->GetSpacing();
  const typename ImageType::PointType fixedOrigin = fixed->GetOrigin();
  const typename ImageType::PointType movingOrigin = moving->GetOrigin();

  // create the image which be biased towards the expected solution and be zero-suppressed
  typename ImageType::Pointer iAdjusted = ImageType::New();
  iAdjusted->CopyInformation( input );
  iAdjusted->SetRegions( input->GetBufferedRegion() );
  iAdjusted->Allocate( false );

  typename ImageType::IndexType adjustedSize;
  typename ImageType::IndexType directExpectedIndex;
  typename ImageType::IndexType mirrorExpectedIndex;
  double distancePenaltyFactor = 0.0;
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    adjustedSize[d] = size[d] + oIndex[d];
    distancePenaltyFactor += adjustedSize[d] * adjustedSize[d]; // make it proportional to image size
    directExpectedIndex[d] = ( movingOrigin[d] - fixedOrigin[d] ) / spacing[d] + oIndex[d];
    mirrorExpectedIndex[d] = ( movingOrigin[d] - fixedOrigin[d] ) / spacing[d] + adjustedSize[d];
    }

  distancePenaltyFactor = -m_BiasTowardsExpected / distancePenaltyFactor;

  MultiThreaderBase* mt = this->GetMultiThreader();
  mt->ParallelizeImageRegion<ImageDimension>( lpr,
    [&](const typename ImageType::RegionType & region)
    {
      ImageRegionConstIterator< ImageType > iIt(input, region);
      ImageRegionIteratorWithIndex< ImageType > oIt(iAdjusted, region);
      for (; !oIt.IsAtEnd(); ++iIt, ++oIt)
        {
        typename ImageType::IndexType ind = oIt.GetIndex();
        IndexValueType dist = 0;
        for (unsigned d = 0; d < ImageDimension; d++)
          {
          IndexValueType distDirect = ( directExpectedIndex[d] - ind[d] ) * ( directExpectedIndex[d] - ind[d] );
          IndexValueType distMirror = ( mirrorExpectedIndex[d] - ind[d] ) * ( mirrorExpectedIndex[d] - ind[d] );
          if ( distDirect <= distMirror )
            {
            dist += distDirect;
            }
          else
            {
            dist += distMirror;
            }
          }
          
        typename ImageType::PixelType pixel = 1000*iIt.Get() * std::exp( distancePenaltyFactor * dist ); //TODO: remove 1000 factor
        oIt.Set( pixel );
        }
    },
    nullptr );

  WriteDebug( iAdjusted.GetPointer(), "iAdjusted.nrrd" );

  // supress trivial zero solution
  FixedArray< double, ImageDimension > dimFactor; // each dimension might have different size
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    double dimFactor = 100.0 / size[d]; // turn absolute size into percentages
    }
  constexpr IndexValueType znSize = 4; // zero neighborhood size
  mt->ParallelizeImageRegion<ImageDimension>( lpr,
    [&]( const typename ImageType::RegionType& region )
    {
      ImageRegionIteratorWithIndex< ImageType > oIt(iAdjusted, region);
      for (; !oIt.IsAtEnd(); ++oIt)
        {
        bool pixelValid = false;
        typename ImageType::PixelType pixel;
        typename ImageType::IndexType ind = oIt.GetIndex();
        IndexValueType dist = 0;
        for ( unsigned d = 0; d < ImageDimension; d++ )
          {
          dist += ind[d] - oIndex[d];
          }
        if ( dist < znSize ) // neighborhood of [0,0,...,0]
          {
          pixel = oIt.Get();
          pixel *= ( dist + 3 ) / ( m_ZeroSuppression + dist + 3 );
          pixelValid = true;
          }

        for ( unsigned d = 0; d < ImageDimension; d++ ) // lines/sheets of zero indices
          {
          if ( ind[d] == oIndex[d] ) // one of the indices is "zero"
            {
            if ( !pixelValid )
              {
              pixel = oIt.Get();
              pixelValid = true;
              }
            IndexValueType distD = ind[d] - oIndex[d];
            if ( distD > IndexValueType( size[d] / 2 ) ) // wrap around
              {
              distD = size[d] - distD;
              }
            double distF = distD * dimFactor[d];
            // avoid the initial steep drop of 1/(1+x) by shifting it by 3% of image size
            pixel *= ( distF + 3 ) / ( m_ZeroSuppression + distF + 3 );
            }
          }

        if ( pixelValid ) // either neighborhood or lines/sheets has updated the pixel
          {
          oIt.Set( pixel );
          }
        }
    },
    nullptr );

  WriteDebug( iAdjusted.GetPointer(), "iAdjustedZS.nrrd" );

  m_MaxCalculator->SetImage( iAdjusted );
  m_MaxCalculator->SetN( std::ceil( this->m_Offsets.size() / 2 ) *
                         ( static_cast< unsigned >( std::pow( 3, ImageDimension ) ) - 1 ) );

  try
    {
    m_MaxCalculator->ComputeMaxima();
    }
  catch ( ExceptionObject& err )
    {
    itkDebugMacro( "exception caught during execution of max calculator - passing " );
    throw err;
    }

  typename MaxCalculatorType::ValueVector maxs = m_MaxCalculator->GetMaxima();
  typename MaxCalculatorType::IndexVector indices = m_MaxCalculator->GetIndicesOfMaxima();
  itkAssertOrThrowMacro( maxs.size() == indices.size(),
      "Maxima and their indices must have the same number of elements" );
  std::greater< PixelType > compGreater;
  auto zeroBound = std::upper_bound( maxs.begin(), maxs.end(), 0.0, compGreater );
  if ( zeroBound != maxs.end() ) // there are some non-positive values in here
    {
    unsigned i = zeroBound - maxs.begin();
    maxs.resize( i );
    indices.resize( i );
    }

  // eliminate indices belonging to the same blurry peak
  // condition used is city-block distance of one
  unsigned i = 1;
  while ( i < indices.size() )
    {
    unsigned k = 0;
    while ( k < i )
      {
      // calculate maximum distance along any dimension
      SizeValueType dist = 0;
      for ( unsigned d = 0; d < ImageDimension; d++ )
        {
        SizeValueType d1 = std::abs( indices[i][d] - indices[k][d] );
        if ( d1 > size[d] / 2 ) // wrap around
          {
          d1 = size[d] - d1;
          }
        dist = std::max( dist, d1 );
        }
      if ( dist < 2 ) // for city-block this is equivalent to:  dist == 1
        {
        break;
        }
      ++k;
      }

    if ( k < i ) // k is nearby
      {
      maxs[k] += maxs[i]; // join amplitudes
      maxs.erase( maxs.begin() + i );
      indices.erase( indices.begin() + i );
      }
    else // examine next index
      {
      ++i;
      }
    }

  // now we need to re-sort the values
  {
    std::vector< unsigned > sIndices;
    sIndices.reserve( maxs.size() );
    for ( i = 0; i < maxs.size(); i++ )
      {
      sIndices.push_back( i );
      }
    std::sort( sIndices.begin(), sIndices.end(), [maxs]( unsigned a, unsigned b ) { return maxs[a] > maxs[b]; } );

    // now apply sorted order
    typename MaxCalculatorType::ValueVector tMaxs( maxs.size() );
    typename MaxCalculatorType::IndexVector tIndices( maxs.size() );
    for ( i = 0; i < maxs.size(); i++ )
      {
      tMaxs[i] = maxs[sIndices[i]];
      tIndices[i] = indices[sIndices[i]];
      }
    maxs.swap( tMaxs );
    indices.swap( tIndices );
  }

  if ( this->m_Offsets.size() > maxs.size() )
    {
    this->SetOffsetCount( maxs.size() );
    }
  else
    {
    maxs.resize( this->m_Offsets.size() );
    indices.resize( this->m_Offsets.size() );
    }

  for ( unsigned m = 0; m < maxs.size(); m++ )
    {
    using ContinuousIndexType = ContinuousIndex< OffsetScalarType, ImageDimension >;
    ContinuousIndexType maxIndex = indices[m];

    if ( m_PeakInterpolationMethod != PeakInterpolationMethod::None ) // interpolate the peak
      {
      typename ImageType::PixelType y0, y1 = maxs[m], y2;
      typename ImageType::IndexType tempIndex = indices[m];

      for ( i = 0; i < ImageDimension; i++ )
        {
        tempIndex[i] = maxIndex[i] - 1;
        if ( !lpr.IsInside( tempIndex ) )
          {
          tempIndex[i] = maxIndex[i];
          continue;
          }
        y0 = iAdjusted->GetPixel( tempIndex );
        tempIndex[i] = maxIndex[i] + 1;
        if ( !lpr.IsInside( tempIndex ) )
          {
          tempIndex[i] = maxIndex[i];
          continue;
          }
        y2 = iAdjusted->GetPixel( tempIndex );
        tempIndex[i] = maxIndex[i];

        OffsetScalarType omega, theta;
        switch ( m_PeakInterpolationMethod )
          {
          case PeakInterpolationMethod::Parabolic:
            maxIndex[i] += ( y0 - y2 ) / ( 2 * ( y0 - 2 * y1 + y2 ) );
            break;
          case PeakInterpolationMethod::Cosine:
            omega = std::acos( ( y0 + y2 ) / ( 2 * y1 ) );
            theta = std::atan( ( y0 - y2 ) / ( 2 * y1 * std::sin( omega ) ) );
            maxIndex[i] -= ::itk::Math::one_over_pi * theta / omega;
            break;
          default:
            itkAssertInDebugAndIgnoreInReleaseMacro( "Unknown interpolation method" );
            break;
          } // switch PeakInterpolationMethod
        } // for ImageDimension
      } // if Interpolation != None

    for ( i = 0; i < ImageDimension; ++i )
      {
      OffsetScalarType directOffset = ( movingOrigin[i] - fixedOrigin[i] )
        - 1 * spacing[i] * ( maxIndex[i] - oIndex[i] );
      OffsetScalarType mirrorOffset = ( movingOrigin[i] - fixedOrigin[i] )
        - 1 * spacing[i] * ( maxIndex[i] - adjustedSize[i] );
      if ( std::abs( directOffset ) <= std::abs( mirrorOffset ) )
        {
        offset[i] = directOffset;
        }
      else
        {
        offset[i] = mirrorOffset;
        }
      }

    this->m_Offsets[m] = offset;
    }
}

} // end namespace itk

#endif
