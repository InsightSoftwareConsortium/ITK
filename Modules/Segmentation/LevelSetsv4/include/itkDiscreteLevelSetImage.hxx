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

#ifndef __itkDiscreteLevelSetImage_hxx
#define __itkDiscreteLevelSetImage_hxx

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
DiscreteLevelSetImage< TOutput, VDimension >::EvaluateGradient( const InputType& iP ) const
{
  InputType pA = iP;
  InputType pB = iP;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;
    pB[dim] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    // division by 0 only if image is a single pixel
    const OutputRealType scale = this->m_NeighborhoodScales[dim] / (pA[dim] - pB[dim]);

    dx[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];

    }

  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::GradientType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateForwardGradient( const InputType& iP ) const
{
  const OutputRealType center_value = static_cast< OutputRealType >( this->Evaluate( iP ) );

  InputType pA = iP;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - center_value ) * scale;

    pA[dim] = iP[dim];
    }

  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::GradientType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateBackwardGradient( const InputType& iP ) const
{
  const OutputRealType center_value = static_cast< OutputRealType >( this->Evaluate( iP ) );

  InputType pA = iP;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( center_value - valueA ) * scale;

    pA[dim] = iP[dim];
    }
  return dx;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::HessianType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateHessian( const InputType& iP ) const
{
  HessianType oHessian;

  const OutputRealType center_value = static_cast< OutputRealType >( this->Evaluate( iP ) );

  InputType pA = iP;
  InputType pB = iP;

  InputType pAa;
  InputType pBa;
  InputType pCa;
  InputType pDa;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    oHessian[dim1][dim1] = ( valueA + valueB - 2.0 * center_value )
        * vnl_math_sqr( this->m_NeighborhoodScales[dim1] );

    pAa = pB;
    pBa = pB;

    pCa = pA;
    pDa = pA;

    for( unsigned int dim2 = dim1 + 1; dim2 < Dimension; dim2++ )
      {
      pAa[dim2] -= 1;
      pBa[dim2] += 1;

      pCa[dim2] -= 1;
      pDa[dim2] += 1;

      if( !this->IsInsideDomain( pAa ) )
        {
        pAa[dim2] = pB[dim2];
        }

      if( !this->IsInsideDomain( pBa ) )
        {
        pBa[dim2] = pB[dim2];
        }

      if( !this->IsInsideDomain( pCa ) )
        {
        pCa[dim2] = pA[dim2];
        }

      if( !this->IsInsideDomain( pDa ) )
        {
        pDa[dim2] = pA[dim2];
        }

      const OutputRealType valueAa = static_cast< OutputRealType >( this->Evaluate( pAa ) );
      const OutputRealType valueBa = static_cast< OutputRealType >( this->Evaluate( pBa ) );
      const OutputRealType valueCa = static_cast< OutputRealType >( this->Evaluate( pCa ) );
      const OutputRealType valueDa = static_cast< OutputRealType >( this->Evaluate( pDa ) );

      oHessian[dim1][dim2] = oHessian[dim2][dim1] =
          0.25 * ( valueAa - valueBa - valueCa + valueDa )
          * this->m_NeighborhoodScales[dim1] * this->m_NeighborhoodScales[dim2];

      pAa[dim2] = pB[dim2];
      pBa[dim2] = pB[dim2];

      pCa[dim2] = pA[dim2];
      pDa[dim2] = pA[dim2];
      }

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

  return oHessian;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::OutputRealType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateLaplacian( const InputType& iP ) const
{
  OutputRealType oLaplacian = NumericTraits< OutputRealType >::Zero;

  const OutputRealType center_value = static_cast< OutputRealType >( this->Evaluate( iP ) );

  InputType pA = iP;
  InputType pB = iP;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    oLaplacian += ( valueA + valueB - 2.0 * center_value )
        * vnl_math_sqr(this->m_NeighborhoodScales[dim1]);

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

  return oLaplacian;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  // If it has not already been computed before
  if( ioData.Value.m_Computed )
    {
    return;
    }

  ioData.Value.m_Value = this->Evaluate( iP );
  ioData.Value.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.Gradient.m_Computed )
    {
    return;
    }

  // If it has not already been computed before

  // compute the gradient

  InputType pA = iP;
  InputType pB = iP;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;
    pB[dim] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim] / (pA[dim] - pB[dim]);

    ioData.Gradient.m_Value[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];
    }

  ioData.Gradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.Hessian.m_Computed )
    {
    return;
    }

  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Computed = true;
    ioData.Value.m_Value = this->Evaluate( iP );
    }

  // compute the hessian
  OutputRealType center_value = static_cast< OutputRealType >( ioData.Value.m_Value );

  InputType pA = iP;
  InputType pB = iP;

  InputType pAa;
  InputType pBa;
  InputType pCa;
  InputType pDa;

  bool backward = ioData.BackwardGradient.m_Computed;
  bool forward = ioData.ForwardGradient.m_Computed;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    ioData.Hessian.m_Value[dim1][dim1] =
        ( valueA + valueB - 2.0 * center_value ) * vnl_math_sqr( this->m_NeighborhoodScales[dim1] );

    if( !backward )
      {
      ioData.BackwardGradient.m_Computed = true;
      ioData.BackwardGradient.m_Value[dim1] =
          ( center_value - valueB ) * this->m_NeighborhoodScales[dim1];
      }

    if( !forward )
      {
      ioData.ForwardGradient.m_Computed = true;
      ioData.ForwardGradient.m_Value[dim1]  =
          ( valueA - center_value ) * this->m_NeighborhoodScales[dim1];
      }

    pAa = pB;
    pBa = pB;

    pCa = pA;
    pDa = pA;

    for( unsigned int dim2 = dim1 + 1; dim2 < Dimension; dim2++ )
      {
      pAa[dim2] -= 1;
      pBa[dim2] += 1;

      pCa[dim2] -= 1;
      pDa[dim2] += 1;

      if( !this->IsInsideDomain( pAa ) )
        {
        pAa[dim2] = pB[dim2];
        }

      if( !this->IsInsideDomain( pBa ) )
        {
        pBa[dim2] = pB[dim2];
        }

      if( !this->IsInsideDomain( pCa ) )
        {
        pCa[dim2] = pA[dim2];
        }

      if( !this->IsInsideDomain( pDa ) )
        {
        pDa[dim2] = pA[dim2];
        }

      const OutputRealType valueAa = static_cast< OutputRealType >( this->Evaluate( pAa ) );
      const OutputRealType valueBa = static_cast< OutputRealType >( this->Evaluate( pBa ) );
      const OutputRealType valueCa = static_cast< OutputRealType >( this->Evaluate( pCa ) );
      const OutputRealType valueDa = static_cast< OutputRealType >( this->Evaluate( pDa ) );

      ioData.Hessian.m_Value[dim1][dim2] =
          ioData.Hessian.m_Value[dim2][dim1] =
          0.25 * ( valueAa - valueBa - valueCa + valueDa )
          * this->m_NeighborhoodScales[dim1] * this->m_NeighborhoodScales[dim2];

      pAa[dim2] = pB[dim2];
      pBa[dim2] = pB[dim2];

      pCa[dim2] = pA[dim2];
      pDa[dim2] = pA[dim2];
      }

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

    ioData.Hessian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
typename DiscreteLevelSetImage< TOutput, VDimension >::OutputRealType
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateMeanCurvature( const InputType& iP ) const
{
  OutputRealType oValue = NumericTraits< OutputRealType >::Zero;

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

  if( gradNorm > vnl_math::eps )
    {
    oValue /= ( gradNorm * gradNorm * gradNorm );
    }
  else
    {
    oValue /= ( NumericTraits< OutputRealType >::One + gradNorm );
    }

  return oValue;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.Laplacian.m_Computed )
    {
    return;
    }

  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Value = this->Evaluate( iP );
    ioData.Value.m_Computed = true;
    }

  const OutputRealType center_value = static_cast< OutputRealType >( ioData.Value.m_Value );

  InputType pA =iP;
  InputType pB = iP;

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !this->IsInsideDomain( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    ioData.Laplacian.m_Value +=
        ( valueA + valueB - 2.0 * center_value ) * vnl_math_sqr( this->m_NeighborhoodScales[dim1] );

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

  ioData.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateMeanCurvature( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( !ioData.MeanCurvature.m_Computed )
    {
    if( !ioData.Hessian.m_Computed )
      {
      this->EvaluateHessian( iP, ioData );
      }

    if( !ioData.Gradient.m_Computed )
      {
      this->EvaluateGradient( iP, ioData );
      }

    if( !ioData.GradientNorm.m_Computed )
      {
      this->EvaluateGradientNorm( iP, ioData );
      }

    ioData.MeanCurvature.m_Computed = true;
    ioData.MeanCurvature.m_Value = NumericTraits< OutputRealType >::Zero;

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

    if( temp > vnl_math::eps )
      {
      ioData.MeanCurvature.m_Value /= ( temp * temp * temp );
      }
    else
      {
      ioData.MeanCurvature.m_Value /= ( NumericTraits< OutputRealType >::One + temp );
      }
    }
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateForwardGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.ForwardGradient.m_Computed )
    {
    return;
    }

  // compute the gradient
  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Computed = true;
    ioData.Value.m_Value = this->Evaluate( iP );
    }

  const OutputRealType center_value = static_cast< OutputRealType >( ioData.Value.m_Value );

  InputType pA = iP;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - center_value ) * scale;

    pA[dim] = iP[dim];
    }
  ioData.ForwardGradient.m_Value = dx;

  ioData.ForwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
void
DiscreteLevelSetImage< TOutput, VDimension >
::EvaluateBackwardGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.BackwardGradient.m_Computed )
    {
    return;
    }

  // compute the gradient
  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Value = this->Evaluate( iP );
    ioData.Value.m_Computed = true;
    }

  const OutputRealType center_value =
    static_cast< OutputRealType >( ioData.Value.m_Value );

  InputType pA = iP;

  GradientType dx;

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] -= 1;

    if( !this->IsInsideDomain( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( center_value - valueA ) * scale;

    pA[dim] = iP[dim];
    }
  ioData.BackwardGradient.m_Value = dx;

  ioData.BackwardGradient.m_Computed = true;
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

  const Self *LevelSet = NULL;

  try
    {
    LevelSet = dynamic_cast< const Self * >( data );
    }
  catch ( ... )
    {
    // LevelSet could not be cast back down
    itkExceptionMacro( << "itk::DiscreteLevelSetImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

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
  const Self *LevelSet = NULL;

  try
    {
    LevelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // image could not be cast back down
    itkExceptionMacro( << "itk::DiscreteLevelSetImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                         << typeid( Self * ).name() );
    }

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
#endif // __itkDiscreteLevelSetImage_hxx
