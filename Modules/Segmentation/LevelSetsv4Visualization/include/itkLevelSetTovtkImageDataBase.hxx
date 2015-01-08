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

#ifndef itkLevelSetTovtkImageDataBase_hxx
#define itkLevelSetTovtkImageDataBase_hxx

#include "itkLevelSetTovtkImageDataBase.h"

namespace itk
{
template< typename TLevelSet >
LevelSetTovtkImageDataBase< TLevelSet >
::LevelSetTovtkImageDataBase()
  {}

template< typename TLevelSet >
LevelSetTovtkImageDataBase< TLevelSet >
::~LevelSetTovtkImageDataBase()
  {}

template< typename TLevelSet >
void
LevelSetTovtkImageDataBase< TLevelSet >
::SetInput( LevelSetType* iLevelSet )
{
  if( !iLevelSet )
    {
    itkGenericExceptionMacro( <<"iLevelSet is ITK_NULLPTR" );
    }
  m_LevelSet = iLevelSet;
}

template< typename TLevelSet >
void
LevelSetTovtkImageDataBase< TLevelSet >
::Update()
{
  this->GenerateData();
}

}

#endif // itkLevelSetTovtkImageDataBase_h
