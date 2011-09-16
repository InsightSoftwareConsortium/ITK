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

#ifndef __itkBinaryImageToDenseLevelSetImageAdaptor_hxx
#define __itkBinaryImageToDenseLevelSetImageAdaptor_hxx

#include "itkBinaryImageToDenseLevelSetImageAdaptor.h"

namespace itk
{
template< class TInput, class TLevelSet >
BinaryImageToDenseLevelSetImageAdaptor< TInput, TLevelSet >
::BinaryImageToDenseLevelSetImageAdaptor()
{}

template< class TInput, class TLevelSet >
BinaryImageToDenseLevelSetImageAdaptor< TInput, TLevelSet >
::~BinaryImageToDenseLevelSetImageAdaptor()
{}

template< class TInput, class TLevelSet >
void
BinaryImageToDenseLevelSetImageAdaptor< TInput, TLevelSet >
::Initialize()
{
  if( this->m_InputImage.IsNull() )
    {
    itkGenericExceptionMacro( "m_InputImage is NULL" );
    }

  if( m_SignedDistanceTransformFilter.IsNull() )
    {
    itkGenericExceptionMacro( "m_SignedDistanceTransformFilter is NULL" );
    }
  m_SignedDistanceTransformFilter->SetInput( this->m_InputImage );
  m_SignedDistanceTransformFilter->Update();

  typename LevelSetImageType::Pointer tempImage = LevelSetImageType::New();
  tempImage->Graft( m_SignedDistanceTransformFilter->GetOutput() );

  this->m_LevelSet = LevelSetType::New();
  this->m_LevelSet->SetImage( tempImage );
}

}

#endif // __itkBinaryImageToDenseLevelSetImageAdaptor_hxx
