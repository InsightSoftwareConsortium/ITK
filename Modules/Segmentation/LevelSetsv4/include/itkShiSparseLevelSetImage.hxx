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

#ifndef __itkShiSparseLevelSetImage_hxx
#define __itkShiSparseLevelSetImage_hxx

#include "itkShiSparseLevelSetImage.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< unsigned int VDimension >
ShiSparseLevelSetImage< VDimension >
::ShiSparseLevelSetImage()
{
  this->InitializeLayers();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
ShiSparseLevelSetImage< VDimension >
::~ShiSparseLevelSetImage()
{
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetImage< VDimension >::OutputType
ShiSparseLevelSetImage< VDimension >
::Evaluate( const InputType& iP ) const
{
  LayerMapConstIterator layerIt = this->m_Layers.begin();

  while( layerIt != this->m_Layers.end() )
    {
    LayerConstIterator it = ( layerIt->second ).find( iP );
    if( it != ( layerIt->second ).end() )
      {
      return it->second;
      }

    ++layerIt;
    }

  if( this->m_LabelMap->GetLabelObject( this->MinusThreeLayer() )->HasIndex( iP ) )
    {
    return static_cast<OutputType>( this->MinusThreeLayer() );
    }
  else
    {
    const LayerIdType status = this->m_LabelMap->GetPixel( iP );

    if( status == this->PlusThreeLayer() )
      {
      return static_cast<OutputType>( this->PlusThreeLayer() );
      }
    else
      {
      itkGenericExceptionMacro( <<"status "
                                << static_cast< int >( status )
                                << " should be 3 or -3" );
      return static_cast<OutputType>( this->PlusThreeLayer() );
      }
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetImage< VDimension >::GradientType
ShiSparseLevelSetImage< VDimension >
::EvaluateGradient( const InputType& iP ) const
{
  return Superclass::EvaluateGradient( iP );
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename ShiSparseLevelSetImage< VDimension >::HessianType
ShiSparseLevelSetImage< VDimension >
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
typename ShiSparseLevelSetImage< VDimension >::OutputRealType
ShiSparseLevelSetImage< VDimension >
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
typename ShiSparseLevelSetImage< VDimension >::OutputRealType
ShiSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& itkNotUsed( iP ) ) const
{
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  OutputRealType oMeanCurvature;
  oMeanCurvature = NumericTraits< OutputRealType >::Zero;
  return oMeanCurvature;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  Superclass::Evaluate( iP, ioData );
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
::EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  Superclass::EvaluateGradient( iP, ioData );
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
::EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const
{
  (void) iP;

  if( ioData.Hessian.m_Computed )
    {
    return;
    }

  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );

  ioData.Hessian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
::EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const
{
  (void) iP;

  if( ioData.Laplacian.m_Computed )
    {
    return;
    }

  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );

  ioData.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& iP, LevelSetDataType& ioData ) const
{
  (void) iP;

  if( ioData.MeanCurvature.m_Computed )
    {
    return;
    }

  itkGenericExceptionMacro( <<"The approximation of the hessian in the Shi's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );

  ioData.MeanCurvature.m_Computed = true;
}


// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >::InitializeLayers()
{
  this->m_Layers.clear();
  this->m_Layers[ MinusOneLayer() ] = LayerType();
  this->m_Layers[ PlusOneLayer()  ] = LayerType();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
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
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( valueA - center_value ) * scale;

    pA[dim] = iP[dim];
    }

  ioData.ForwardGradient.m_Value = dx;

  ioData.ForwardGradient.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
ShiSparseLevelSetImage< VDimension >
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
    const OutputRealType scale = this->m_NeighborhoodScales[dim];

    dx[dim] = ( center_value - valueA ) * scale;

    pA[dim] = iP[dim];
    }

  ioData.BackwardGradient.m_Value = dx;

  ioData.BackwardGradient.m_Computed = true;
}

}

#endif // __itkShiSparseLevelSetImage_h
