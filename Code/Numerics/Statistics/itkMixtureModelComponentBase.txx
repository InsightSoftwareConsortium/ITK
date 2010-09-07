/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMixtureModelComponentBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMixtureModelComponentBase_txx
#define __itkMixtureModelComponentBase_txx

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
