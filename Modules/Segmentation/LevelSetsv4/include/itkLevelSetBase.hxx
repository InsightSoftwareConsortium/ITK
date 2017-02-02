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

#ifndef itkLevelSetBase_hxx
#define itkLevelSetBase_hxx

#include "itkLevelSetBase.h"
#include "itkProcessObject.h"

#define UNDEFINED_REGION NumericTraits< RegionType >::OneValue()

namespace itk
{

template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::LevelSetBase() :
  m_MaximumNumberOfRegions(0),
  m_NumberOfRegions(0),
  m_RequestedNumberOfRegions(0),
  m_BufferedRegion(0),
  m_RequestedRegion(0)
{}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
bool
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::IsInside( const InputType& iP ) const
{
  return ( this->Evaluate( iP ) <= NumericTraits< OutputType >::ZeroValue() );
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::Initialize()
{
  Superclass::Initialize();
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::EvaluateGradientNorm( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( !ioData.GradientNorm.m_Computed )
    {
    if( !ioData.Gradient.m_Computed )
      {
      this->EvaluateGradient( iP, ioData );
      }

    ioData.GradientNorm.m_Computed = true;
    ioData.GradientNorm.m_Value = ioData.Gradient.m_Value.GetNorm();
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
typename
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::OutputRealType
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::EvaluateGradientNorm( const InputType& iP ) const
{
  GradientType grad = this->EvaluateGradient( iP );
  return grad.GetNorm();
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
typename
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::OutputRealType
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::EvaluateMeanCurvature( const InputType& iP ) const
{
  OutputRealType oValue = NumericTraits< OutputRealType >::ZeroValue();

  HessianType   hessian = this->EvaluateHessian( iP );
  GradientType  grad = this->EvaluateGradient( iP );

  for( unsigned int i = 0; i < Dimension; i++ )
    {
    for( unsigned int j = 0; j < Dimension; j++ )
      {
      if( j != i )
        {
        oValue -= grad[i] * grad[j] * hessian[i][j];
        oValue += hessian[j][j] * grad[i] * grad[i];
        }
      }
    }

  OutputRealType gradNorm = grad.GetNorm();

  if( gradNorm > itk::Math::eps )
    {
    oValue /= ( gradNorm * gradNorm * gradNorm );
    }
  else
    {
    oValue /= ( NumericTraits< OutputRealType >::OneValue() + gradNorm );
    }

  return oValue;
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::EvaluateMeanCurvature( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( !ioData.MeanCurvature.m_Computed )
    {
    if( !ioData.Hessian.m_Computed )
      {
      EvaluateHessian( iP, ioData );
      }

    if( !ioData.Gradient.m_Computed )
      {
      EvaluateGradient( iP, ioData );
      }

    if( !ioData.GradientNorm.m_Computed )
      {
      EvaluateGradientNorm( iP, ioData );
      }

    ioData.MeanCurvature.m_Computed = true;
    ioData.MeanCurvature.m_Value = NumericTraits< OutputRealType >::ZeroValue();

    for( unsigned int i = 0; i < Dimension; i++ )
      {
      for( unsigned int j = 0; j < Dimension; j++ )
        {
        if( j != i )
          {
          ioData.MeanCurvature.m_Value -= ioData.Gradient.m_Value[i]
              * ioData.Gradient.m_Value[j] * ioData.Hessian.m_Value[i][j];
          ioData.MeanCurvature.m_Value += ioData.Hessian.m_Value[j][j]
              * ioData.Gradient.m_Value[i] * ioData.Gradient.m_Value[i];
          }
        }
      }

    OutputRealType temp = ioData.GradientNorm.m_Value;

    if( temp > itk::Math::eps )
      {
      ioData.MeanCurvature.m_Value /= ( temp * temp * temp );
      }
    else
      {
      ioData.MeanCurvature.m_Value /= ( NumericTraits< OutputRealType >::OneValue() + temp );
      }
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::UpdateOutputInformation()
{
  if( this->GetSource() )
    {
    this->GetSource()->UpdateOutputInformation();
    }

  // Now we should know what our largest possible region is. If our
  // requested region was not set yet, (or has been set to something
  // invalid - with no data in it ) then set it to the largest
  // possible region.
  if ( m_RequestedNumberOfRegions == 0 )
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedNumberOfRegions  = NumericTraits< RegionType >::OneValue();
  m_RequestedRegion           = NumericTraits< RegionType >::ZeroValue();
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::CopyInformation(const DataObject *data)
{
  const LevelSetBase *levelSet = dynamic_cast< const LevelSetBase * >( data );

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( LevelSetBase * ).name() );
    }

  m_MaximumNumberOfRegions = levelSet->GetMaximumNumberOfRegions();

  m_NumberOfRegions = levelSet->m_NumberOfRegions;
  m_RequestedNumberOfRegions = levelSet->m_RequestedNumberOfRegions;
  m_BufferedRegion  = levelSet->m_BufferedRegion;
  m_RequestedRegion = levelSet->m_RequestedRegion;
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::Graft(const DataObject *data)
{
  // Copy Meta Data
  this->CopyInformation(data);

  const Self * levelSet = dynamic_cast< const Self * >( data );

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
bool
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  if ( m_RequestedRegion != m_BufferedRegion
       || m_RequestedNumberOfRegions != m_NumberOfRegions )
    {
    return true;
    }

  return false;
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
bool
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::VerifyRequestedRegion()
{
  bool retval = true;

  // Are we asking for more regions than we can get?
  if ( m_RequestedNumberOfRegions > m_MaximumNumberOfRegions )
    {
    itkExceptionMacro(<< "Cannot break object into "
                      << m_RequestedNumberOfRegions << ". The limit is "
                      << m_MaximumNumberOfRegions);
    }

  if ( m_RequestedRegion >= m_RequestedNumberOfRegions )
    {
    itkExceptionMacro(<< "Invalid update region " << m_RequestedRegion
                      << ". Must be between 0 and "
                      << m_RequestedNumberOfRegions - 1);
    }

  return retval;
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::SetRequestedRegion(const DataObject *data)
{
  const Self *levelSet = dynamic_cast< const Self * >( data );

  if ( levelSet )
    {
    // only copy the RequestedRegion if the parameter is another PointSet
    m_RequestedRegion = levelSet->m_RequestedRegion;
    m_RequestedNumberOfRegions = levelSet->m_RequestedNumberOfRegions;
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::SetRequestedRegion(const RegionType & region)
{
  if ( m_RequestedRegion != region )
    {
    m_RequestedRegion = region;
    }
}

// ----------------------------------------------------------------------------
template< typename TInput, unsigned int VDimension, typename TOutput, typename TDomain >
void
LevelSetBase< TInput, VDimension, TOutput, TDomain >
::SetBufferedRegion(const RegionType & region)
{
  if ( m_BufferedRegion != region )
    {
    m_BufferedRegion = region;
    this->Modified();
    }
}

} // end namespace itk

#endif // itkLevelSetBase_hxx
