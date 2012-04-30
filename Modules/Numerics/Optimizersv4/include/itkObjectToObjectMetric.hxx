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
#ifndef __itkObjectToObjectMetric_hxx
#define __itkObjectToObjectMetric_hxx

#include "itkObjectToObjectMetric.h"
#include "itkTransform.h"
#include "itkIdentityTransform.h"

namespace itk
{

/*
 * constructor
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::ObjectToObjectMetric()
{
  /* Both transforms default to an identity transform */
  typedef IdentityTransform<ParametersValueType, itkGetStaticConstMacro( MovingDimension ) > MovingIdentityTransformType;
  typedef IdentityTransform<ParametersValueType, itkGetStaticConstMacro( FixedDimension ) > FixedIdentityTransformType;
  this->m_FixedTransform  = FixedIdentityTransformType::New();
  this->m_MovingTransform = MovingIdentityTransformType::New();
std::cerr << "m_FixedTransform: " << m_FixedTransform << std::endl;
}

/*
 * destructor
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::~ObjectToObjectMetric()
{
}

/*
 * Initialize
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
void
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::Initialize() throw ( ExceptionObject )
{
  if ( !this->m_FixedTransform )
    {
    itkExceptionMacro( "Fixed transform is not present" );
    }

  if ( !this->m_MovingTransform )
    {
    itkExceptionMacro( "Moving transform is not present" );
    }
}

/*
 * SetTransform
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
void
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::SetTransform( MovingTransformType* transform )
{
  this->SetMovingTransform( transform );
}

/*
 * GetTransform
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
const typename ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>::MovingTransformType *
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::GetTransform()
{
  return this->GetMovingTransform();
}

/*
 * UpdateTransformParameters
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
void
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::UpdateTransformParameters( DerivativeType & derivative,
                             ParametersValueType factor )
{
  /* Rely on transform::UpdateTransformParameters to verify proper
   * size of derivative */
  this->m_MovingTransform->UpdateTransformParameters( derivative, factor );
}

/*
 * GetNumberOfParameters
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
typename ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>::NumberOfParametersType
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::GetNumberOfParameters() const
{
  return this->m_MovingTransform->GetNumberOfParameters();
}

/*
 * GetParameters
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
const typename ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>::ParametersType &
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::GetParameters() const
{
  return this->m_MovingTransform->GetParameters();
}

/*
 * SetParameters
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
void
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::SetParameters( ParametersType & params)
{
  this->m_MovingTransform->SetParametersByValue( params );
}

/*
 * GetNumberOfLocalParameters
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
typename ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>::NumberOfParametersType
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::GetNumberOfLocalParameters() const
{
  return this->m_MovingTransform->GetNumberOfLocalParameters();
}

/*
 * HasLocalSupport
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
bool
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::HasLocalSupport() const
{
  return this->m_MovingTransform->HasLocalSupport();
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension>
void
ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ObjectToObjectMetric: " << std::endl
     << indent << "Fixed Transform: " << std::endl
     << indent << this->m_FixedTransform << std::endl
     << indent << "Moving Transform: " << std::endl
     << indent << this->m_MovingTransform << std::endl;

}
}//namespace itk

#endif
