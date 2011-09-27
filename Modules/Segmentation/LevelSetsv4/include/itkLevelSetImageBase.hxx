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

#ifndef __itkLevelSetImageBase_hxx
#define __itkLevelSetImageBase_hxx

#include "itkLevelSetImageBase.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< class TImage >
LevelSetImageBase< TImage >
::LevelSetImageBase() : m_Image( NULL )
{
  this->m_NeighborhoodScales.Fill( NumericTraits< OutputRealType >::One );
}

// ----------------------------------------------------------------------------
template< class TImage >
LevelSetImageBase< TImage >
::~LevelSetImageBase()
{
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
::SetImage( ImageType* iImage )
{
  this->m_Image = iImage;
  typename ImageType::SpacingType spacing = m_Image->GetSpacing();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    m_NeighborhoodScales[dim] =
      NumericTraits< OutputRealType >::One / static_cast< OutputRealType >( spacing[dim ] );
    }
  this->Modified();
}

// ----------------------------------------------------------------------------
template< class TImage >
typename LevelSetImageBase< TImage >::OutputType
LevelSetImageBase< TImage >::Evaluate( const InputType& iP ) const
{
  return this->m_Image->GetPixel( iP );
}

// ----------------------------------------------------------------------------
template< class TImage >
typename LevelSetImageBase< TImage >::GradientType
LevelSetImageBase< TImage >::EvaluateGradient( const InputType& iP ) const
{
  InputType pA = iP;
  InputType pB = iP;

  GradientType dx;

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;
    pB[dim] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim] = iP[dim];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    // division by 0 only if image is a single pixel
    const OutputRealType scale = m_NeighborhoodScales[dim] / (pA[dim] - pB[dim]);

    dx[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];

    }

  return dx;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< class TImage >
typename LevelSetImageBase< TImage >::HessianType
LevelSetImageBase< TImage >
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

    const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    oHessian[dim1][dim1] = ( valueA + valueB - 2.0 * center_value )
        * vnl_math_sqr( m_NeighborhoodScales[dim1] );

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

      if( !largestRegion.IsInside( pAa ) )
        {
        pAa[dim2] = pB[dim2];
        }

      if( !largestRegion.IsInside( pBa ) )
        {
        pBa[dim2] = pB[dim2];
        }

      if( !largestRegion.IsInside( pCa ) )
        {
        pCa[dim2] = pA[dim2];
        }

      if( !largestRegion.IsInside( pDa ) )
        {
        pDa[dim2] = pA[dim2];
        }

      const OutputRealType valueAa = static_cast< OutputRealType >( this->Evaluate( pAa ) );
      const OutputRealType valueBa = static_cast< OutputRealType >( this->Evaluate( pBa ) );
      const OutputRealType valueCa = static_cast< OutputRealType >( this->Evaluate( pCa ) );
      const OutputRealType valueDa = static_cast< OutputRealType >( this->Evaluate( pDa ) );

      oHessian[dim1][dim2] = oHessian[dim2][dim1] =
          0.25 * ( valueAa - valueBa - valueCa + valueDa )
          * m_NeighborhoodScales[dim1] * m_NeighborhoodScales[dim2];

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
template< class TImage >
typename LevelSetImageBase< TImage >::OutputRealType
LevelSetImageBase< TImage >
::EvaluateLaplacian( const InputType& iP ) const
{
  OutputRealType oLaplacian = NumericTraits< OutputRealType >::Zero;

  const OutputRealType center_value = static_cast< OutputRealType >( this->Evaluate( iP ) );

  InputType pA = iP;
  InputType pB = iP;

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    oLaplacian += ( valueA + valueB - 2.0 * center_value )
        * vnl_math_sqr(m_NeighborhoodScales[dim1]);

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

  return oLaplacian;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  // If it has not already been computed before
  if( ioData.Value.m_Computed )
    {
    return;
    }

  ioData.Value.m_Value = m_Image->GetPixel( iP );
  ioData.Value.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;
    pB[dim] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim] = iP[dim];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );
    const OutputRealType scale = m_NeighborhoodScales[dim] / (pA[dim] - pB[dim]);

    ioData.Gradient.m_Value[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];
    }

  ioData.Gradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    ioData.Hessian.m_Value[dim1][dim1] =
        ( valueA + valueB - 2.0 * center_value ) * vnl_math_sqr( m_NeighborhoodScales[dim1] );

    if( !backward )
      {
      ioData.BackwardGradient.m_Computed = true;
      ioData.BackwardGradient.m_Value[dim1] =
          ( center_value - valueB ) * m_NeighborhoodScales[dim1];
      }

    if( !forward )
      {
      ioData.ForwardGradient.m_Computed = true;
      ioData.ForwardGradient.m_Value[dim1]  =
          ( valueA - center_value ) * m_NeighborhoodScales[dim1];
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

      if( !largestRegion.IsInside( pAa ) )
        {
        pAa[dim2] = pB[dim2];
        }

      if( !largestRegion.IsInside( pBa ) )
        {
        pBa[dim2] = pB[dim2];
        }

      if( !largestRegion.IsInside( pCa ) )
        {
        pCa[dim2] = pA[dim2];
        }

      if( !largestRegion.IsInside( pDa ) )
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
          * m_NeighborhoodScales[dim1] * m_NeighborhoodScales[dim2];

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
template< class TImage >
void
LevelSetImageBase< TImage >
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

  // compute the hessian
  const OutputRealType center_value = static_cast< OutputRealType >( ioData.Value.m_Value );

  InputType pA =iP;
  InputType pB = iP;

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim1 = 0; dim1 < Dimension; dim1++ )
    {
    pA[dim1] += 1;
    pB[dim1] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim1] = iP[dim1];
      }

    if( !largestRegion.IsInside( pB ) )
      {
      pB[dim1] = iP[dim1];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType valueB = static_cast< OutputRealType >( this->Evaluate( pB ) );

    ioData.Laplacian.m_Value +=
        ( valueA + valueB - 2.0 * center_value ) * vnl_math_sqr( m_NeighborhoodScales[dim1] );

    pA[dim1] = iP[dim1];
    pB[dim1] = iP[dim1];
    }

  ioData.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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

  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - center_value ) * scale;

    pA[dim] = iP[dim];
    }
  ioData.ForwardGradient.m_Value = dx;

  ioData.ForwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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
  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] -= 1;

    if( !largestRegion.IsInside( pA ) )
      {
      pA[dim] = iP[dim];
      }

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = m_NeighborhoodScales[dim];

    dx[dim] = ( center_value - valueA ) * scale;

    pA[dim] = iP[dim];
    }
  ioData.BackwardGradient.m_Value = dx;

  ioData.BackwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
::Initialize()
{
  Superclass::Initialize();

  this->m_Image = NULL;
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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
    itkExceptionMacro( << "itk::LevelSetImageBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetImageBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

// ----------------------------------------------------------------------------
template< class TImage >
void
LevelSetImageBase< TImage >
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
    itkExceptionMacro( << "itk::LevelSetImageBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                         << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetImageBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_Image = LevelSet->m_Image;
  this->m_NeighborhoodScales = LevelSet->m_NeighborhoodScales;
}

}
#endif // __itkLevelSetImageBase_hxx
