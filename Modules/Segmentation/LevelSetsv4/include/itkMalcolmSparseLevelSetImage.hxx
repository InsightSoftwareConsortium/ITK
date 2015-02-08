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

#ifndef itkMalcolmSparseLevelSetImage_hxx
#define itkMalcolmSparseLevelSetImage_hxx

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
MalcolmSparseLevelSetImage< VDimension >::Evaluate( const InputType& inputPixel ) const
{
  InputType mapIndex = inputPixel - this->m_DomainOffset;
  LayerMapConstIterator layerIt = this->m_Layers.begin();

  while( layerIt != this->m_Layers.end() )
    {
    LayerConstIterator it = ( layerIt->second ).find( mapIndex );
    if( it != ( layerIt->second ).end() )
      {
      return it->second;
      }

    ++layerIt;
    }

  if( this->m_LabelMap->GetLabelObject( MinusOneLayer() )->HasIndex( mapIndex ) )
    {
    return MinusOneLayer();
    }
  else
    {
    char status = this->m_LabelMap->GetPixel( mapIndex );
    if( status == PlusOneLayer() )
      {
      return PlusOneLayer();
      }
    else
      {
      itkGenericExceptionMacro( <<"status "
                                << static_cast< int >( status )
                                << " should be 1 or -1" );
      }
    }
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::HessianType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateHessian( const InputType& inputPixel ) const
{
  (void) inputPixel;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Malcolm's representation" );
  HessianType oHessian;
  oHessian.Fill( NumericTraits< OutputRealType >::ZeroValue() );
  return oHessian;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::OutputRealType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateLaplacian( const InputType& inputPixel ) const
{
  (void) inputPixel;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  OutputRealType oLaplacian;
  oLaplacian = NumericTraits< OutputRealType >::ZeroValue();
  return oLaplacian;
}
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
typename MalcolmSparseLevelSetImage< VDimension >::OutputRealType
MalcolmSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& inputPixel ) const
{
  (void) inputPixel;
  itkGenericExceptionMacro( <<"The approximation of the hessian in the Malcolm's"
                            <<" representation is poor, and far to be representative."
                            <<" If it was required for regularization purpose, "
                            <<" you better check recommended regularization methods"
                            <<" for Shi's representation" );
  OutputRealType oMeanCurvature;
  oMeanCurvature = NumericTraits< OutputRealType >::ZeroValue();
  return oMeanCurvature;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::EvaluateHessian( const InputType& inputPixel, LevelSetDataType& data ) const
{
  (void) inputPixel;

  if( data.Hessian.m_Computed )
    {
    return;
    }

  data.Hessian.m_Value = this->EvaluateHessian( inputPixel );

  data.Hessian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::EvaluateLaplacian( const InputType& inputPixel, LevelSetDataType& data ) const
{
  if( !data.Laplacian.m_Computed )
    {
    return;
    }

  data.Laplacian.m_Value = this->EvaluateLaplacian( inputPixel );

  data.Laplacian.m_Computed = true;
}

// ----------------------------------------------------------------------------
template< unsigned int VDimension >
void
MalcolmSparseLevelSetImage< VDimension >
::EvaluateMeanCurvature( const InputType& inputPixel, LevelSetDataType& data ) const
{
  if( !data.MeanCurvature.m_Computed )
    {
    return;
    }

  data.MeanCurvature.m_Value = this->EvaluateMeanCurvature( inputPixel );
  data.MeanCurvature.m_Computed = true;
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
#endif // itkMalcolmSparseLevelSetImage_hxx
