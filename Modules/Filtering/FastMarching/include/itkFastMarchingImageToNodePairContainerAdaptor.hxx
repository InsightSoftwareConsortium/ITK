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

#ifndef itkFastMarchingImageToNodePairContainerAdaptor_hxx
#define itkFastMarchingImageToNodePairContainerAdaptor_hxx

#include "itkMath.h"
#include "itkFastMarchingImageToNodePairContainerAdaptor.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template< typename TInput, typename TOutput, typename TImage >
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::FastMarchingImageToNodePairContainerAdaptor() :
  m_AliveImage( ITK_NULLPTR ), m_TrialImage( ITK_NULLPTR ), m_ForbiddenImage( ITK_NULLPTR ),
  m_AlivePoints( ITK_NULLPTR ), m_TrialPoints( ITK_NULLPTR ), m_ForbiddenPoints( ITK_NULLPTR ),
  m_AliveValue( NumericTraits< OutputPixelType >::ZeroValue() ),
  m_TrialValue( NumericTraits< OutputPixelType >::ZeroValue() ),
  m_IsForbiddenImageBinaryMask( false )
{}

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::SetAliveImage( const ImageType* iImage )
  {
  m_AliveImage = iImage;
  }

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::SetTrialImage( const ImageType* iImage )
  {
  m_TrialImage = iImage;
  }

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::SetForbiddenImage( const ImageType* iImage )
  {
  m_ForbiddenImage = iImage;
  }

template< typename TInput, typename TOutput, typename TImage >
typename FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::NodePairContainerType*
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::GetAlivePoints()
  {
  return m_AlivePoints.GetPointer();
  }

template< typename TInput, typename TOutput, typename TImage >
typename FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::NodePairContainerType*
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::GetTrialPoints()
  {
  return m_TrialPoints.GetPointer();
  }

template< typename TInput, typename TOutput, typename TImage >
typename FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::NodePairContainerType*
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::GetForbiddenPoints()
  {
  return m_ForbiddenPoints.GetPointer();
  }

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::Update()
  {
  this->GenerateData();
  }

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::GenerateData()
{
  bool is_ok = false;

  if( m_AliveImage.IsNotNull() )
    {
    SetPointsFromImage( m_AliveImage, Traits::Alive, m_AliveValue );
    is_ok = true;
    }

  if( m_TrialImage.IsNotNull() )
    {
    SetPointsFromImage( m_TrialImage, Traits::InitialTrial, m_TrialValue );
    is_ok = true;
    }

  if( m_ForbiddenImage.IsNotNull() )
    {
    SetPointsFromImage( m_ForbiddenImage, Traits::Forbidden,
                       NumericTraits< OutputPixelType >::ZeroValue() );
    is_ok = true;
    }

  if( !is_ok )
    {
    itkWarningMacro( <<"no input image provided" );
    }
}

template< typename TInput, typename TOutput, typename TImage >
void
FastMarchingImageToNodePairContainerAdaptor< TInput, TOutput, TImage >
::SetPointsFromImage( const ImageType* image, const LabelType& iLabel,
                     const OutputPixelType& iValue )
  {
  if ( iLabel == Traits::Alive ||
      iLabel == Traits::InitialTrial ||
      iLabel == Traits::Forbidden )
    {
    NodePairContainerPointer nodes = NodePairContainerType::New();
    nodes->Initialize();

    typedef ImageRegionConstIteratorWithIndex< ImageType > IteratorType;
    IteratorType it( image, image->GetBufferedRegion() );

    if( iLabel == Traits::Alive ||
        iLabel == Traits::InitialTrial ||
        ( iLabel == Traits::Forbidden && !m_IsForbiddenImageBinaryMask ) )
      {
      // Walk image
      for (it.GoToBegin(); !it.IsAtEnd(); ++it)
        {
        // Test if index value is greater than zero, if so add the node
        if ( Math::NotAlmostEquals( it.Get(), NumericTraits< ImagePixelType >::ZeroValue() ) )
          {
          nodes->push_back( NodePairType( it.GetIndex(), iValue ) );
          } //end if image iterator > zero
        } // end for each pixel
      }
    else
      {
      for (it.GoToBegin(); !it.IsAtEnd(); ++it)
        {
        // Test if index value is greater than zero, if so add the node
        if ( Math::AlmostEquals( it.Get(), NumericTraits< ImagePixelType >::ZeroValue() ) )
          {
          nodes->push_back( NodePairType( it.GetIndex(), iValue ) );
          } //end if image iterator > zero
        } // end for each pixel
      }

    switch( iLabel )
      {
      case Traits::Alive:
        m_AlivePoints = nodes;
        break;
      case Traits::InitialTrial:
        m_TrialPoints = nodes;
        break;
      case Traits::Forbidden:
        m_ForbiddenPoints = nodes;
        break;
      default:
        break;
      }
    }
  }


}

#endif // itkFastMarchingImageToNodePairContainerAdaptor_hxx
