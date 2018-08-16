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
#include <type_traits>

/*
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 */

namespace itk
{

template < typename TRegistrationMethod >
MaxPhaseCorrelationOptimizer<TRegistrationMethod>
::MaxPhaseCorrelationOptimizer() : Superclass()
{
  m_MaxCalculator = MaxCalculatorType::New();
  m_PeakInterpolationMethod = PeakInterpolationMethod::Parabolic;
}


template < typename TRegistrationMethod >
void
MaxPhaseCorrelationOptimizer<TRegistrationMethod>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "MaxCalculator: " << m_MaxCalculator << std::endl;
  auto pim = static_cast<typename std::underlying_type<PeakInterpolationMethod>::type>(m_PeakInterpolationMethod);
  os << indent << "PeakInterpolationMethod: " << pim << std::endl;
}

template<typename TRegistrationMethod>
void
MaxPhaseCorrelationOptimizer<TRegistrationMethod>
::SetPeakInterpolationMethod(const PeakInterpolationMethod peakInterpolationMethod)
{
  if ( this->m_PeakInterpolationMethod != peakInterpolationMethod )
    {
    this->m_PeakInterpolationMethod = peakInterpolationMethod;
    this->Modified();
    }
}

template < typename TRegistrationMethod >
void
MaxPhaseCorrelationOptimizer<TRegistrationMethod>
::ComputeOffset()
{
  ImageConstPointer input = static_cast< ImageType * >(this->GetInput(0));
  ImageConstPointer fixed = static_cast< ImageType * >(this->GetInput(1));
  ImageConstPointer moving = static_cast< ImageType * >(this->GetInput(2));

  OffsetType offset;
  offset.Fill( 0 );

  if (!input)
    {
    return;
    }

  m_MaxCalculator->SetImage( input );

  try
    {
    m_MaxCalculator->ComputeMaximum();
    }
  catch( ExceptionObject& err )
    {
    itkDebugMacro( "exception caught during execution of max calculator - passing " );
    throw err;
    }

  using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
  ContinuousIndexType maxIndex = m_MaxCalculator->GetIndexOfMaximum();

  if (m_PeakInterpolationMethod != PeakInterpolationMethod::None) //interpolate the peak
    {
    typename ImageType::PixelType y0, y1 = m_MaxCalculator->GetMaximum(), y2;
    typename ImageType::IndexType tempIndex = m_MaxCalculator->GetIndexOfMaximum();
    typename ImageType::RegionType region = input->GetLargestPossibleRegion();

    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      tempIndex[i] = maxIndex[i] - 1;
      if( ! region.IsInside( tempIndex ) )
        {
        tempIndex[i] = maxIndex[i];
        continue;
        }
      y0 = input->GetPixel( tempIndex );
      tempIndex[i] = maxIndex[i] + 1;
      if( ! region.IsInside( tempIndex ) )
        {
        tempIndex[i] = maxIndex[i];
        continue;
        }
      y2 = input->GetPixel( tempIndex );
      tempIndex[i] = maxIndex[i];

      OffsetScalarType omega, theta;
      switch (m_PeakInterpolationMethod)
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
            itkAssertInDebugAndIgnoreInReleaseMacro("Unknown interpolation method");
            break;
        } //switch PeakInterpolationMethod
      } //for ImageDimension
    } //if Interpolation != None

  const typename ImageType::RegionType lpr = input->GetLargestPossibleRegion();
  const typename ImageType::IndexType oIndex = lpr.GetIndex();
  const typename ImageType::SizeType size = lpr.GetSize();
  const typename ImageType::SpacingType spacing = input->GetSpacing();
  const typename ImageType::PointType fixedOrigin = fixed->GetOrigin();
  const typename ImageType::PointType movingOrigin = moving->GetOrigin();

  for( unsigned int i = 0; i < ImageDimension; ++i )
    {
    IndexValueType adjustedSize = IndexValueType(size[i] + oIndex[i]);
    OffsetScalarType directOffset = (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
    OffsetScalarType mirrorOffset = (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize);
    if (std::abs(directOffset) <= std::abs(mirrorOffset))
      {
      offset[i] = directOffset;
      }
    else
      {
      offset[i] = mirrorOffset;
      }
    }

  this->SetOffset( offset );
}

} //end namespace itk

#endif
