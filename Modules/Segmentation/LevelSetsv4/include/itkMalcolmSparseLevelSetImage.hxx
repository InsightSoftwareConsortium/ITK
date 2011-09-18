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

#ifndef __itkMalcolmSparseLevelSetImage_hxx
#define __itkMalcolmSparseLevelSetImage_hxx

#include "itkMalcolmSparseLevelSetImage.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< unsigned int VDimension >
MalcolmSparseLevelSetImage< VDimension >
::MalcolmSparseLevelSetImage()
{
  this->InitializeLayers();
  this->InitializeInternalLabelList();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
MalcolmSparseLevelSetImage< VDimension >
::~MalcolmSparseLevelSetImage()
{
}

// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::OutputType
MalcolmSparseLevelSetImage< VDimension >::Evaluate( const InputType& iP ) const
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

  if( this->m_LabelMap->GetLabelObject( MinusOneLayer() )->HasIndex( iP ) )
    {
    return MinusOneLayer();
    }
  else
    {
    char status = this->m_LabelMap->GetPixel( iP );
    if( status == PlusOneLayer() )
      {
      return PlusOneLayer();
      }
    else
      {
      itkGenericExceptionMacro( <<"status "
                                << static_cast< int >( status )
                                << " should be 1 or -1" );
      return PlusOneLayer();
      }
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::HessianType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateHessian( const InputType& iP ) const
{
  (void) iP;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Malcolm's representation" );
  HessianType oHessian;
  oHessian.Fill( NumericTraits< OutputRealType >::Zero );
  return oHessian;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::OutputRealType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateLaplacian( const InputType& iP ) const
{
  (void) iP;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  OutputRealType oLaplacian;
  oLaplacian = NumericTraits< OutputRealType >::Zero;
  return oLaplacian;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::OutputRealType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& iP ) const
{
  (void) iP;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
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
MalcolmSparseLevelSetImage< VDimension >
::EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const
{
  (void) iP;

  if( ioData.Hessian.m_Computed )
    {
    return;
    }

  ioData.Hessian.m_Value = this->EvaluateHessian( iP );

  ioData.Hessian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( !ioData.Laplacian.m_Computed )
    {
    return;
    }

  ioData.Laplacian.m_Value = this->EvaluateLaplacian( iP );

  ioData.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& iP, LevelSetDataType& ioData ) const
{
  if( !ioData.MeanCurvature.m_Computed )
    {
    return;
    }

  ioData.MeanCurvature.m_Value = this->EvaluateMeanCurvature( iP );
  ioData.MeanCurvature.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::InitializeLayers()
{
  this->m_Layers.clear();
  this->m_Layers[ ZeroLayer() ] = LayerType();
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::InitializeInternalLabelList()
{
  this->m_InternalLabelList.clear();
  this->m_InternalLabelList.push_back( MinusOneLayer() );
  this->m_InternalLabelList.push_back( ZeroLayer() );
}

}
#endif // __itkMalcolmSparseLevelSetImage_hxx
