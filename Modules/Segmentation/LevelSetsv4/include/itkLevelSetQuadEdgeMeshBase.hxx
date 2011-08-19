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

#ifndef __itkLevelSetQuadEdgeMeshBase_hxx
#define __itkLevelSetQuadEdgeMeshBase_hxx

#include "itkLevelSetQuadEdgeMeshBase.h"

namespace itk
{
template< class TMesh >
LevelSetQuadEdgeMeshBase< TMesh >
::LevelSetQuadEdgeMeshBase()
{}

template< class TMesh >
LevelSetQuadEdgeMeshBase< TMesh >
::~LevelSetQuadEdgeMeshBase()
{}

template< class TMesh >
typename LevelSetQuadEdgeMeshBase< TMesh >::OutputType
LevelSetQuadEdgeMeshBase< TMesh >::Evaluate( const InputType& iP ) const
{
  OutputType oValue = 0.;
  this->m_Mesh->GetPointData( iP, &oValue );
  return oValue;
}

template< class TMesh >
typename LevelSetQuadEdgeMeshBase< TMesh >::GradientType
LevelSetQuadEdgeMeshBase< TMesh >::EvaluateGradient( const InputType& iP ) const
{
  itkWarningMacro( <<"to be implemented" );
  return this->GradientType();
}

template< class TMesh >
typename LevelSetQuadEdgeMeshBase< TMesh >::HessianType
LevelSetQuadEdgeMeshBase< TMesh >::EvaluateHessian( const InputType& iP ) const
{
  itkWarningMacro( <<"to be implemented" );
  return this->HessianType();
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  // if it has not already been computed before
  if( !ioData.Value.first )
    {
    ioData.Value.first = true;
    this->m_Mesh->GetPointData( iP, &( ioData.Value.second ) );
    }
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const
{
  // if it has not already been computed before
  if( !ioData.Gradient.first )
    {
    ioData.Gradient.second = true;

    // compute the gradient
    ///\todo implement the computation of the gradient
    }
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const
{
if( !ioData.Hessian.first )
  {
  ioData.Hessian.first = true;

  // compute the hessian
  ///\todo implement the computation of the hessian
  }
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::Initialize()
{
  Superclass::Initialize();

  this->m_Mesh = NULL;
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::CopyInformation(const DataObject *data)
{
  Superclass::CopyInformation( data );

  const Self *levelSet = NULL;

  try
    {
    levelSet = dynamic_cast< const Self * >( data );
    }
  catch ( ... )
    {
    // levelSet could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMeshBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMeshBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

template< class TMesh >
void
LevelSetQuadEdgeMeshBase< TMesh >::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *levelSet = NULL;

  try
    {
    levelSet = dynamic_cast< const Self* >( data );
    }
  catch( ... )
    {
    // mesh could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMeshBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMeshBase::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_Mesh = levelSet->m_Mesh;
}

}

#endif // __itkLevelSetQuadEdgeMeshBase_hxx
