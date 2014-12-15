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
#ifndef itkClassifierBase_hxx
#define itkClassifierBase_hxx

#include "itkClassifierBase.h"
#include "itkCommand.h"

namespace itk
{
template< typename TDataContainer >
ClassifierBase< TDataContainer >
::ClassifierBase()
{
  m_NumberOfClasses = 0;
  m_DecisionRule = ITK_NULLPTR;
  m_MembershipFunctions.resize(0);
}

template< typename TDataContainer >
ClassifierBase< TDataContainer >
::~ClassifierBase()
{}

template< typename TDataContainer >
void
ClassifierBase< TDataContainer >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of classes: " << m_NumberOfClasses << std::endl;
  os << indent << "DecisionRule: ";
  if ( m_DecisionRule.IsNotNull() )
    {
    os << m_DecisionRule << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }

  os << indent << "MembershipFunctions: " << std::endl;
  for ( unsigned int i = 0; i < m_MembershipFunctions.size(); ++i )
    {
    os << indent << m_MembershipFunctions[i] << std::endl;
    }
}

template< typename TDataContainer >
void
ClassifierBase< TDataContainer >
::Update()
{
  if ( m_NumberOfClasses == 0 )
    {
    itkExceptionMacro("Zero class");
    return;
    }

  if ( m_MembershipFunctions.size() == 0 )
    {
    itkExceptionMacro("No membership function");
    return;
    }

  if ( m_NumberOfClasses != m_MembershipFunctions.size() )
    {
    itkExceptionMacro("The number of classes and the number of membership mismatch.");
    return;
    }

  this->GenerateData();
}

template< typename TDataContainer >
unsigned int
ClassifierBase< TDataContainer >
::AddMembershipFunction(MembershipFunctionType *function)
{
  m_MembershipFunctions.push_back(function);
  return static_cast< unsigned int >( m_MembershipFunctions.size() );
}
} // namespace itk

#endif
