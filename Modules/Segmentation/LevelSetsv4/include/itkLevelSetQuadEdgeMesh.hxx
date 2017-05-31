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

#ifndef itkLevelSetQuadEdgeMesh_hxx
#define itkLevelSetQuadEdgeMesh_hxx

#include "itkLevelSetQuadEdgeMesh.h"

namespace itk
{
template< typename TMesh >
LevelSetQuadEdgeMesh< TMesh >
::LevelSetQuadEdgeMesh()
{}

template< typename TMesh >
LevelSetQuadEdgeMesh< TMesh >
::~LevelSetQuadEdgeMesh()
{}

template< typename TMesh >
typename LevelSetQuadEdgeMesh< TMesh >::OutputType
LevelSetQuadEdgeMesh< TMesh >::Evaluate( const InputType& iP ) const
{
  OutputType oValue = 0.;
  this->m_Mesh->GetPointData( iP, &oValue );
  return oValue;
}

template< typename TMesh >
typename LevelSetQuadEdgeMesh< TMesh >::GradientType
LevelSetQuadEdgeMesh< TMesh >::EvaluateGradient( const InputType& itkNotUsed(iP) ) const
{
  itkWarningMacro( <<"to be implemented" );
  return Self::GradientType(); //Create a new object with default initializer
}

template< typename TMesh >
typename LevelSetQuadEdgeMesh< TMesh >::HessianType
LevelSetQuadEdgeMesh< TMesh >::EvaluateHessian( const InputType& itkNotUsed(iP) ) const
{
  itkWarningMacro( <<"to be implemented" );
  return Self::HessianType(); //Create a new objet with default initializer
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::Evaluate( const InputType& iP, LevelSetDataType& ioData ) const
{
  // if it has not already been computed before
  if( !ioData.Value.first )
    {
    ioData.Value.first = true;
    this->m_Mesh->GetPointData( iP, &( ioData.Value.second ) );
    }
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::EvaluateGradient( const InputType& itkNotUsed(iP), LevelSetDataType& ioData ) const
{

  // if it has not already been computed before
  if( !ioData.Gradient.first )
    {
    ioData.Gradient.second = true;

    // compute the gradient
    ///\todo implement the computation of the gradient
    }
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::EvaluateHessian( const InputType& itkNotUsed(iP), LevelSetDataType& ioData ) const
{
  if( !ioData.Hessian.first )
    {
    ioData.Hessian.first = true;

    // compute the hessian
    ///\todo implement the computation of the hessian
    }
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::Initialize()
{
  Superclass::Initialize();

  this->m_Mesh = ITK_NULLPTR;
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::CopyInformation(const DataObject *data)
{
  Superclass::CopyInformation( data );

  const Self *levelSet = dynamic_cast< const Self * >( data );

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMesh::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

template< typename TMesh >
void
LevelSetQuadEdgeMesh< TMesh >::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *levelSet = dynamic_cast< const Self* >( data );

  if ( !levelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetQuadEdgeMesh::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_Mesh = levelSet->m_Mesh;
}

}

#endif // itkLevelSetQuadEdgeMesh_hxx
