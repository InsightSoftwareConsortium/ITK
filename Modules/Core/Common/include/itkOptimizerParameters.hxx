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

#ifndef __itkOptimizerParameters_hxx
#define __itkOptimizerParameters_hxx

#include "itkOptimizerParameters.h"

namespace itk
{
/** Default contstructor */
template< typename TValueType >
OptimizerParameters< TValueType >
::OptimizerParameters() : Array< TValueType >()
{
  this->Initialize();
}

/** Copy constructor */
template< typename TValueType >
OptimizerParameters< TValueType >
::OptimizerParameters(const OptimizerParameters& rhs)
  : Array< TValueType >(rhs)
{
  //Note: don't copy the OptimizerParametersHelper.
  //The Array copy constructor will allocate new memory
  //and copy the data to it. So we end up here with a generic
  //OptimizerParameters data object even if 'rhs' points to
  //something different.
  this->Initialize();
}

/** Constructor with size */
template< typename TValueType >
OptimizerParameters< TValueType >
::OptimizerParameters(SizeValueType dimension)
  : Array< TValueType >(dimension)
{
  this->Initialize();
}

/** Constructor with Array assignment */
template< typename TValueType >
OptimizerParameters< TValueType >
::OptimizerParameters(const ArrayType& array)
  : Array< TValueType >(array)
{
  this->Initialize();
}

template< typename TValueType >
void
OptimizerParameters< TValueType >
::Initialize()
{
  this->m_Helper = NULL;
  // Set the default OptimizerParametersHelper
  OptimizerParametersHelperType* helper = new OptimizerParametersHelperType;
  // OptimizerParameters will manage this memory.
  this->SetHelper( helper );
}

/** Destructor */
template< typename TValueType >
OptimizerParameters< TValueType >
::~OptimizerParameters()
{
  delete this->m_Helper;
}

template< typename TValueType >
void
OptimizerParameters< TValueType >
::SetHelper( OptimizerParametersHelperType* helper )
{
  delete this->m_Helper;
  this->m_Helper = helper;
}

/** Copy operator for self */
template< typename TValueType >
const typename OptimizerParameters< TValueType >
::Self &
OptimizerParameters< TValueType >
::operator=(const Self & rhs)
{
  //Note: there's no need to copy the OptimizerParametersHelper.
  // Call the superclass implementation.
  this->ArrayType::operator=(rhs);
  return *this;
}

template< typename TValueType >
const typename OptimizerParameters< TValueType >
::Self &
OptimizerParameters< TValueType >
::operator=(const ArrayType & rhs)
{
  // Call the superclass implementation
  this->ArrayType::operator=(rhs);
  return *this;
}

template< typename TValueType >
const typename OptimizerParameters< TValueType >
::Self &
OptimizerParameters< TValueType >
::operator=(const VnlVectorType & rhs)
{
  // Call the superclass implementation
  this->ArrayType::operator=(rhs);
  return *this;
}

template< typename TValueType >
void
OptimizerParameters< TValueType >
::MoveDataPointer( TValueType * pointer )
{
  if( m_Helper == NULL )
    {
    itkGenericExceptionMacro("OptimizerParameters::MoveDataPointer: "
      "m_Helper must be set.");
    }
  this->m_Helper->MoveDataPointer( this, pointer );
}

template< typename TValueType >
void
OptimizerParameters< TValueType >
::SetParametersObject( LightObject * object )
{
  if( m_Helper == NULL )
    {
    itkGenericExceptionMacro("OptimizerParameters::SetParameterObject: "
      "m_Helper must be set.");
    }
    this->m_Helper->SetParametersObject( this, object );
}

}//namespace itk
#endif
