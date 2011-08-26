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

#ifndef __itkShiSparseLevelSetBase_hxx
#define __itkShiSparseLevelSetBase_hxx

#include "itkShiSparseLevelSetBase.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< unsigned int VDimension >
ShiSparseLevelSetBase< VDimension >
::ShiSparseLevelSetBase() : m_LabelMap( 0 )
{
  InitializeLayers();
  this->m_NeighborhoodScales.Fill( NumericTraits< OutputRealType >::One );
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
ShiSparseLevelSetBase< VDimension >
::~ShiSparseLevelSetBase()
{
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
::SetLabelMap( LabelMapType* iLabelMap )
{
  this->m_LabelMap = iLabelMap;

  typedef typename LabelMapType::SpacingType  SpacingType;

  SpacingType spacing = m_LabelMap->GetSpacing();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    this->m_NeighborhoodScales[dim] =
      NumericTraits< OutputRealType >::One / static_cast< OutputRealType >( spacing[dim ] );
    }
  this->Modified();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::LayerIdType
ShiSparseLevelSetBase< VDimension >
::Status( const InputType& iP ) const
{
  return this->m_LabelMap->GetPixel( iP );
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::OutputType
ShiSparseLevelSetBase< VDimension >
::Evaluate( const InputType& iP ) const
{
  LayerMapConstIterator layerIt = m_Layers.begin();

  while( layerIt != m_Layers.end() )
    {
    LayerConstIterator it = ( layerIt->second ).find( iP );
    if( it != ( layerIt->second ).end() )
      {
      return it->second;
      }

    ++layerIt;
    }

  if( m_LabelMap->GetLabelObject( MinusThreeLayer() )->HasIndex( iP ) )
    {
    return static_cast<OutputType>( MinusThreeLayer() );
    }
  else
    {
    const LayerIdType status = m_LabelMap->GetPixel( iP );

    if( status == PlusThreeLayer() )
      {
      return static_cast<OutputType>( PlusThreeLayer() );
      }
    else
      {
      itkGenericExceptionMacro( <<"status "
                                << static_cast< int >( status )
                                << " should be 3 or -3" );
      return static_cast<OutputType>( PlusThreeLayer() );
      }
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::GradientType
ShiSparseLevelSetBase< VDimension >
::EvaluateGradient( const InputType& iP ) const
{
  InputType pA = iP;
  InputType pB = iP;

  GradientType dx;
  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

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

    dx[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];
    }

  return dx;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::HessianType
ShiSparseLevelSetBase< VDimension >
::EvaluateHessian( const InputType& itkNotUsed( iP ) ) const
{
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  HessianType oHessian;
  oHessian.Fill( NumericTraits< OutputRealType >::Zero );
  return oHessian;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::OutputRealType
ShiSparseLevelSetBase< VDimension >
::EvaluateLaplacian( const InputType& itkNotUsed( iP ) ) const
{
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  OutputRealType oLaplacian;
  oLaplacian = NumericTraits< OutputRealType >::Zero;
  return oLaplacian;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  // If it has not already been computed before
  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Value = this->Evaluate( iP );
    ioData.Value.m_Computed = true;
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
::EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.Gradient.m_Computed )
    {
    return;
    }

  // compute the gradient

  if( !ioData.Value.m_Computed )
    {
    ioData.Value.m_Value = this->Evaluate( iP );
    ioData.Value.m_Computed = true;
    }

  InputType pA = iP;
  InputType pB = iP;

  GradientType dx;

  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

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

    dx[dim] = ( valueA - valueB ) * scale;

    pA[dim] = pB[dim] = iP[dim];
    }

  ioData.Gradient.m_Value = dx;

  ioData.Gradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
::EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.Hessian.m_Computed )
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
  InputType pAa;
  InputType pBa;
  InputType pCa;
  InputType pDa;

  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

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
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
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

  InputType pA = iP;
  InputType pB = iP;

  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

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
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >::Initialize()
{
  Superclass::Initialize();

  this->m_LabelMap = 0;
  this->m_Layers.clear();
  this->InitializeLayers();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >::
CopyInformation( const DataObject* data )
{
  Superclass::CopyInformation( data );

  const Self *LevelSet = NULL;

  try
    {
    LevelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // LevelSet could not be cast back down
    itkExceptionMacro( << "itk::ShiSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ShiSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >::
Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *LevelSet = 0;

  try
    {
    LevelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // mesh could not be cast back down
    itkExceptionMacro( << "itk::ShiSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ShiSparseLevelSetBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_LabelMap->Graft( LevelSet->m_LabelMap );
  this->m_Layers = LevelSet->m_Layers;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
const typename ShiSparseLevelSetBase< VDimension >::LayerType&
ShiSparseLevelSetBase< VDimension >::GetLayer( LayerIdType iVal ) const
{
  LayerMapConstIterator it = m_Layers.find( iVal );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetBase< VDimension >::LayerType&
ShiSparseLevelSetBase< VDimension >::GetLayer( LayerIdType iVal )
{
  LayerMapIterator it = m_Layers.find( iVal );
  if( it == m_Layers.end() )
    {
    itkGenericExceptionMacro( <<"This layer does not exist" );
    }
  return it->second;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >::SetLayer( LayerIdType iVal, const LayerType& iLayer )
{
  LayerMapIterator it = m_Layers.find( iVal );
  if( it != m_Layers.end() )
    {
    it->second = iLayer;
    }
  else
    {
    itkGenericExceptionMacro( <<iVal << "is out of bounds" );
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >::InitializeLayers()
{
  this->m_Layers.clear();
  this->m_Layers[ MinusOneLayer() ] = LayerType();
  this->m_Layers[ PlusOneLayer()  ] = LayerType();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
::EvaluateForwardGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( ioData.ForwardGradient.m_Computed )
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

  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] += 1;

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - center_value ) * scale;

    pA[dim] = iP[dim];
    }

  ioData.ForwardGradient.m_Value = dx;

  ioData.ForwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetBase< VDimension >
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

  const RegionType largestRegion = this->m_LabelMap->GetLargestPossibleRegion();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    pA[dim] -= 1;

    const OutputRealType valueA = static_cast< OutputRealType >( this->Evaluate( pA ) );
    const OutputRealType scale = m_NeighborhoodScales[dim];

    dx[dim] = ( center_value - valueA ) * scale;

    pA[dim] = iP[dim];
    }

  ioData.BackwardGradient.m_Value = dx;

  ioData.BackwardGradient.m_Computed = true;
}

}

#endif // __itkShiSparseLevelSetBase_h
