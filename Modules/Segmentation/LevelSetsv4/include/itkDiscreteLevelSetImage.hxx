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

#ifndef itkDiscreteLevelSetImage_hxx
#define itkDiscreteLevelSetImage_hxx

#include "itkDiscreteLevelSetImage.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
DiscreteLevelSetImage< TOutput, VDimension >
::DiscreteLevelSetImage()
{
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
DiscreteLevelSetImage< TOutput, VDimension >
::~DiscreteLevelSetImage()
{
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::GradientType
DiscreteLevelSetImage< TOutput, VDimension >::EvaluateGradient( const InputType& inputIndex ) const
{
  InputType inputIndexA = inputIndex;
  InputType inputIndexB = inputIndex;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] += 1;
    inputIndexB[dim] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );

    // division by 0 only if image is a single pixel
    const OutputRealType scale = this->m_NeighborhoodScales[dim] / (inputIndexA[dim] - inputIndexB[dim]);

    dx[dim] = ( valueA - valueB ) * scale;

    inputIndexA[dim] = inputIndexB[dim] = inputIndex[dim];

    }

  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::GradientType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateForwardGradient( const InputType& inputIndex ) const
{
  const OutputRealType centerValue = static_cast< OutputRealType >( this->Evaluate( inputIndex ) );

  InputType inputIndexA = inputIndex;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] += 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - centerValue ) * scale;

    inputIndexA[dim] = inputIndex[dim];
    }

  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::GradientType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateBackwardGradient( const InputType& inputIndex ) const
{
  const OutputRealType centerValue = static_cast< OutputRealType >( this->Evaluate( inputIndex ) );

  InputType inputIndexA = inputIndex;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( centerValue - valueA ) * scale;

    inputIndexA[dim] = inputIndex[dim];
    }
  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::HessianType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateHessian( const InputType& inputIndex ) const
{
  HessianType oHessian;

  const OutputRealType centerValue = static_cast< OutputRealType >( this->Evaluate( inputIndex ) );

  InputType inputIndexA = inputIndex;
  InputType inputIndexB = inputIndex;

  InputType inputIndexAa;
  InputType inputIndexBa;
  InputType inputIndexCa;
  InputType inputIndexDa;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    inputIndexA[dim1] += 1;
    inputIndexB[dim1] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim1] = inputIndex[dim1];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim1] = inputIndex[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );

    oHessian[dim1][dim1] = ( valueA + valueB - 2.0 * centerValue )
        * itk::Math::sqr( this->m_NeighborhoodScales[dim1] );

    inputIndexAa = inputIndexB;
    inputIndexBa = inputIndexB;

    inputIndexCa = inputIndexA;
    inputIndexDa = inputIndexA;

    for( unsigned int dim2 = dim1 + 1; dim2 < Dimension; dim2++ )
      {
      inputIndexAa[dim2] -= 1;
      inputIndexBa[dim2] += 1;

      inputIndexCa[dim2] -= 1;
      inputIndexDa[dim2] += 1;

      if( !this->IsInsideDomain( inputIndexAa ) )
        {
        inputIndexAa[dim2] = inputIndexB[dim2];
        }

      if( !this->IsInsideDomain( inputIndexBa ) )
        {
        inputIndexBa[dim2] = inputIndexB[dim2];
        }

      if( !this->IsInsideDomain( inputIndexCa ) )
        {
        inputIndexCa[dim2] = inputIndexA[dim2];
        }

      if( !this->IsInsideDomain( inputIndexDa ) )
        {
        inputIndexDa[dim2] = inputIndexA[dim2];
        }

      const OutputRealType valueAa = static_cast< OutputRealType >( this->Evaluate( inputIndexAa ) );
      const OutputRealType valueBa = static_cast< OutputRealType >( this->Evaluate( inputIndexBa ) );
      const OutputRealType valueCa = static_cast< OutputRealType >( this->Evaluate( inputIndexCa ) );
      const OutputRealType valueDa = static_cast< OutputRealType >( this->Evaluate( inputIndexDa ) );

      oHessian[dim1][dim2] = oHessian[dim2][dim1] =
          0.25 * ( valueAa - valueBa - valueCa + valueDa )
          * this->m_NeighborhoodScales[dim1] * this->m_NeighborhoodScales[dim2];

      inputIndexAa[dim2] = inputIndexB[dim2];
      inputIndexBa[dim2] = inputIndexB[dim2];

      inputIndexCa[dim2] = inputIndexA[dim2];
      inputIndexDa[dim2] = inputIndexA[dim2];
      }

    inputIndexA[dim1] = inputIndex[dim1];
    inputIndexB[dim1] = inputIndex[dim1];
    }

  return oHessian;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::OutputRealType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateLaplacian( const InputType& inputIndex ) const
{
  OutputRealType oLaplacian = NumericTraits< OutputRealType >::ZeroValue();

  const OutputRealType centerValue = static_cast< OutputRealType >( this->Evaluate( inputIndex ) );

  InputType inputIndexA = inputIndex;
  InputType inputIndexB = inputIndex;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    inputIndexA[dim1] += 1;
    inputIndexB[dim1] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim1] = inputIndex[dim1];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim1] = inputIndex[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );

    oLaplacian += ( valueA + valueB - 2.0 * centerValue )
        * itk::Math::sqr(this->m_NeighborhoodScales[dim1]);

    inputIndexA[dim1] = inputIndex[dim1];
    inputIndexB[dim1] = inputIndex[dim1];
    }

  return oLaplacian;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::Evaluate( const InputType& inputIndex, LevelSetDataType& data ) const
{
  // If it has not already been computed before
  if( data.Value.m_Computed )
    {
    return;
    }

  data.Value.m_Value = this->Evaluate( inputIndex );
  data.Value.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateGradient( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( data.Gradient.m_Computed )
    {
    return;
    }

  // If it has not already been computed before

  // compute the gradient

  InputType inputIndexA = inputIndex;
  InputType inputIndexB = inputIndex;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] += 1;
    inputIndexB[dim] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim] / (inputIndexA[dim] - inputIndexB[dim]);

    data.Gradient.m_Value[dim] = ( valueA - valueB ) * scale;

    inputIndexA[dim] = inputIndexB[dim] = inputIndex[dim];
    }

  data.Gradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateHessian( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( data.Hessian.m_Computed )
    {
    return;
    }

  if( !data.Value.m_Computed )
    {
    data.Value.m_Computed = true;
    data.Value.m_Value = this->Evaluate( inputIndex );
    }

  // compute the hessian
  OutputRealType centerValue = static_cast< OutputRealType >( data.Value.m_Value );

  InputType inputIndexA = inputIndex;
  InputType inputIndexB = inputIndex;

  InputType inputIndexAa;
  InputType inputIndexBa;
  InputType inputIndexCa;
  InputType inputIndexDa;

  bool backward = data.BackwardGradient.m_Computed;
  bool forward = data.ForwardGradient.m_Computed;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    inputIndexA[dim1] += 1;
    inputIndexB[dim1] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim1] = inputIndex[dim1];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim1] = inputIndex[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );

    data.Hessian.m_Value[dim1][dim1] =
        ( valueA + valueB - 2.0 * centerValue ) * itk::Math::sqr( this->m_NeighborhoodScales[dim1] );

    if( !backward )
      {
      data.BackwardGradient.m_Computed = true;
      data.BackwardGradient.m_Value[dim1] =
          ( centerValue - valueB ) * this->m_NeighborhoodScales[dim1];
      }

    if( !forward )
      {
      data.ForwardGradient.m_Computed = true;
      data.ForwardGradient.m_Value[dim1]  =
          ( valueA - centerValue ) * this->m_NeighborhoodScales[dim1];
      }

    inputIndexAa = inputIndexB;
    inputIndexBa = inputIndexB;

    inputIndexCa = inputIndexA;
    inputIndexDa = inputIndexA;

    for( unsigned int dim2 = dim1 + 1; dim2 < Dimension; dim2++ )
      {
      inputIndexAa[dim2] -= 1;
      inputIndexBa[dim2] += 1;

      inputIndexCa[dim2] -= 1;
      inputIndexDa[dim2] += 1;

      if( !this->IsInsideDomain( inputIndexAa ) )
        {
        inputIndexAa[dim2] = inputIndexB[dim2];
        }

      if( !this->IsInsideDomain( inputIndexBa ) )
        {
        inputIndexBa[dim2] = inputIndexB[dim2];
        }

      if( !this->IsInsideDomain( inputIndexCa ) )
        {
        inputIndexCa[dim2] = inputIndexA[dim2];
        }

      if( !this->IsInsideDomain( inputIndexDa ) )
        {
        inputIndexDa[dim2] = inputIndexA[dim2];
        }

      const OutputRealType valueAa = static_cast< OutputRealType >( this->Evaluate( inputIndexAa ) );
      const OutputRealType valueBa = static_cast< OutputRealType >( this->Evaluate( inputIndexBa ) );
      const OutputRealType valueCa = static_cast< OutputRealType >( this->Evaluate( inputIndexCa ) );
      const OutputRealType valueDa = static_cast< OutputRealType >( this->Evaluate( inputIndexDa ) );

      data.Hessian.m_Value[dim1][dim2] =
          data.Hessian.m_Value[dim2][dim1] =
          0.25 * ( valueAa - valueBa - valueCa + valueDa )
          * this->m_NeighborhoodScales[dim1] * this->m_NeighborhoodScales[dim2];

      inputIndexAa[dim2] = inputIndexB[dim2];
      inputIndexBa[dim2] = inputIndexB[dim2];

      inputIndexCa[dim2] = inputIndexA[dim2];
      inputIndexDa[dim2] = inputIndexA[dim2];
      }

    inputIndexA[dim1] = inputIndex[dim1];
    inputIndexB[dim1] = inputIndex[dim1];
    }

    data.Hessian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::OutputRealType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateMeanCurvature( const InputType& inputIndex ) const
{
  OutputRealType oValue = NumericTraits< OutputRealType >::ZeroValue();

  HessianType   hessian = this->EvaluateHessian( inputIndex );
  GradientType  grad = this->EvaluateGradient( inputIndex );

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
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateLaplacian( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( data.Laplacian.m_Computed )
    {
    return;
    }

  if( !data.Value.m_Computed )
    {
    data.Value.m_Value = this->Evaluate( inputIndex );
    data.Value.m_Computed = true;
    }

  const OutputRealType centerValue = static_cast< OutputRealType >( data.Value.m_Value );

  InputType inputIndexA =inputIndex;
  InputType inputIndexB = inputIndex;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    inputIndexA[dim1] += 1;
    inputIndexB[dim1] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim1] = inputIndex[dim1];
      }

    if( !this->IsInsideDomain( inputIndexB ) )
      {
      inputIndexB[dim1] = inputIndex[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( inputIndexB ) );

    data.Laplacian.m_Value +=
        ( valueA + valueB - 2.0 * centerValue ) * itk::Math::sqr( this->m_NeighborhoodScales[dim1] );

    inputIndexA[dim1] = inputIndex[dim1];
    inputIndexB[dim1] = inputIndex[dim1];
    }

  data.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateMeanCurvature( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( !data.MeanCurvature.m_Computed )
    {
    if( !data.Hessian.m_Computed )
      {
      this->EvaluateHessian( inputIndex, data );
      }

    if( !data.Gradient.m_Computed )
      {
      this->EvaluateGradient( inputIndex, data );
      }

    if( !data.GradientNorm.m_Computed )
      {
      this->EvaluateGradientNorm( inputIndex, data );
      }

    data.MeanCurvature.m_Computed = true;
    data.MeanCurvature.m_Value = NumericTraits< OutputRealType >::ZeroValue();

    for( unsigned int i = 0; i < Dimension; i++ )
      {
      for( unsigned int j = 0; j < Dimension; j++ )
        {
        if( j != i )
          {
          data.MeanCurvature.m_Value -= data.Gradient.m_Value[i]
              * data.Gradient.m_Value[j] * data.Hessian.m_Value[i][j];
          data.MeanCurvature.m_Value += data.Hessian.m_Value[j][j]
              * data.Gradient.m_Value[i] * data.Gradient.m_Value[i];
          }
        }
      }

    OutputRealType temp = data.GradientNorm.m_Value;

    if( temp > itk::Math::eps )
      {
      data.MeanCurvature.m_Value /= ( temp * temp * temp );
      }
    else
      {
      data.MeanCurvature.m_Value /= ( NumericTraits< OutputRealType >::OneValue() + temp );
      }
    }
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateForwardGradient( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( data.ForwardGradient.m_Computed )
    {
    return;
    }

  // compute the gradient
  if( !data.Value.m_Computed )
    {
    data.Value.m_Computed = true;
    data.Value.m_Value = this->Evaluate( inputIndex );
    }

  const OutputRealType centerValue = static_cast< OutputRealType >( data.Value.m_Value );

  InputType inputIndexA = inputIndex;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] += 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - centerValue ) * scale;

    inputIndexA[dim] = inputIndex[dim];
    }
  data.ForwardGradient.m_Value = dx;

  data.ForwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateBackwardGradient( const InputType& inputIndex, LevelSetDataType& data ) const
{
  if( data.BackwardGradient.m_Computed )
    {
    return;
    }

  // compute the gradient
  if( !data.Value.m_Computed )
    {
    data.Value.m_Value = this->Evaluate( inputIndex );
    data.Value.m_Computed = true;
    }

  const OutputRealType centerValue =
    static_cast< OutputRealType >( data.Value.m_Value );

  InputType inputIndexA = inputIndex;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    inputIndexA[dim] -= 1;

    if( !this->IsInsideDomain( inputIndexA ) )
      {
      inputIndexA[dim] = inputIndex[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( inputIndexA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( centerValue - valueA ) * scale;

    inputIndexA[dim] = inputIndex[dim];
    }
  data.BackwardGradient.m_Value = dx;

  data.BackwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::Initialize()
{
  Superclass::Initialize();
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::CopyInformation(const DataObject *data)
{
  Superclass::CopyInformation( data );

  const Self *LevelSet = dynamic_cast< const Self * >( data );
  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::DiscreteLevelSetImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *LevelSet = dynamic_cast< const Self* >( data );

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::DiscreteLevelSetImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_NeighborhoodScales = LevelSet->m_NeighborhoodScales;
}

}
#endif // itkDiscreteLevelSetImage_hxx
