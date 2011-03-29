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

#ifndef __itkTransformParameters_hxx
#define __itkTransformParameters_hxx

#include "itkTransformParameters.h"

namespace itk
{
/** Default contstructor */
template< typename TValueType >
TransformParameters< TValueType >
::TransformParameters() : Array< TValueType >()
{
  this->Initialize();
}

/** Copy constructor */
template< typename TValueType >
TransformParameters< TValueType >
::TransformParameters(const TransformParameters& rhs)
  : Array< TValueType >(rhs)
{
  this->Initialize();
  //TODO: Don't copy the TransformParametersHelper?
}

/** Constructor with size */
template< typename TValueType >
TransformParameters< TValueType >
::TransformParameters(unsigned int dimension)
  : Array< TValueType >(dimension)
{
  this->Initialize();
}

/** Constructor with Array assignment */
template< typename TValueType >
TransformParameters< TValueType >
::TransformParameters(const ArrayType& array)
  : Array< TValueType >(array)
{
  this->Initialize();
}

template< typename TValueType >
void
TransformParameters< TValueType >
::Initialize()
{
  this->m_Helper = NULL;
  // Set the default TransformParametersHelper
  TransformParametersHelperType* helper = new TransformParametersHelperType;
  // TransformParameters will manage this memory.
  this->SetHelper( helper );
}

/** Destructor */
template< typename TValueType >
TransformParameters< TValueType >
::~TransformParameters()
{
  if( this->m_Helper )
    {
    delete this->m_Helper;
    }
}

template< typename TValueType >
void
TransformParameters< TValueType >
::SetHelper( TransformParametersHelperType* helper )
{
  if( this->m_Helper )
    {
    delete this->m_Helper;
    }
  this->m_Helper = helper;
}

/** Copy operator for self */
template< typename TValueType >
const typename TransformParameters< TValueType >
::Self &
TransformParameters< TValueType >
::operator=(const Self & rhs)
{
  if ( this == &rhs ) { return *this; }

  // Call the superclass implementation
  this->ArrayType::operator=(rhs);

  //TODO: Don't copy the TransformParametersHelper?

  return *this;
}

template< typename TValueType >
const typename TransformParameters< TValueType >
::Self &
TransformParameters< TValueType >
::operator=(const ArrayType & rhs)
{
  if ( this == &rhs ) { return *this; }

  // Call the superclass implementation
  this->ArrayType::operator=(rhs);

  return *this;
}

template< typename TValueType >
const typename TransformParameters< TValueType >
::Self &
TransformParameters< TValueType >
::operator=(const VnlVectorType & rhs)
{
  if ( this == &rhs ) { return *this; }

  // Call the superclass implementation
  this->ArrayType::operator=(rhs);

  return *this;
}

}//namespace itk
#endif
