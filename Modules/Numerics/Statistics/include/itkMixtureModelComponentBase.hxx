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
#ifndef __itkMixtureModelComponentBase_hxx
#define __itkMixtureModelComponentBase_hxx

#include "itkMixtureModelComponentBase.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
MixtureModelComponentBase< TSample >
::MixtureModelComponentBase()
{
  m_Sample = 0;
  m_MembershipFunction = 0;
  m_MinimalParametersChange = 1.0e-06;
  m_ParametersModified = true;
}

template< class TSample >
MixtureModelComponentBase< TSample >
::~MixtureModelComponentBase()
{}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sample: ";
  if ( m_Sample != 0 )
    {
    os << m_Sample << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }

  os << indent << "Membership Function: ";
  if ( m_MembershipFunction != 0 )
    {
    os << m_MembershipFunction << std::endl;
    }
  else
    {
    os << "not instantiated yet." << std::endl;
    }

  os << indent << "Weights Array: ";
  os << m_Weights << std::endl;

  os << indent << "Parameters are modified: " << m_ParametersModified
     << std::endl;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetSample(const TSample *sample)
{
  m_Sample = sample;
  m_Weights = WeightArrayType( m_Sample->Size() );
}

template< class TSample >
const TSample *
MixtureModelComponentBase< TSample >
::GetSample() const
{
  return m_Sample;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetParameters(const ParametersType & parameters)
{
  if ( m_Parameters != parameters )
    {
    m_Parameters = parameters;
    this->AreParametersModified(true);
    }
}

template< class TSample >
bool
MixtureModelComponentBase< TSample >
::AreParametersModified()
{
  return m_ParametersModified;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::AreParametersModified(bool flag)
{
  m_ParametersModified = flag;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetMembershipFunction(MembershipFunctionType *function)
{
  m_MembershipFunction = function;
}

template< class TSample >
typename MixtureModelComponentBase< TSample >::MembershipFunctionType *
MixtureModelComponentBase< TSample >
::GetMembershipFunction()
{
  return m_MembershipFunction;
}

template< class TSample >
inline double
MixtureModelComponentBase< TSample >
::Evaluate(MeasurementVectorType & measurements)
{
  return m_MembershipFunction->Evaluate(measurements);
}

template< class TSample >
inline void
MixtureModelComponentBase< TSample >
::SetWeight(unsigned int index, double value)
{
  if ( index < m_Weights.size() )
    {
    ( m_Weights )[index] = value;
    }
  else
    {
    itkExceptionMacro("Weight array is not allocated.");
    }
}

template< class TSample >
inline double
MixtureModelComponentBase< TSample >
::GetWeight(unsigned int index) const
{
  if ( index < m_Weights.Size() )
    {
    return m_Weights[index];
    }
  else
    {
    itkExceptionMacro("Weight array is not allocated.");
    }
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::Update()
{
  this->GenerateData();
}
} // end of namespace Statistics
} // end of namespace itk

#endif
