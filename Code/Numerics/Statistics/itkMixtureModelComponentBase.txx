/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMixtureModelComponentBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMixtureModelComponentBase_txx
#define __itkMixtureModelComponentBase_txx

#include "itkMixtureModelComponentBase.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
MixtureModelComponentBase< TSample >
::MixtureModelComponentBase()
{
  m_Weights = 0 ;
  m_MinimalParametersChange = 1.0e-06 ;
}

template< class TSample >
MixtureModelComponentBase< TSample >
::~MixtureModelComponentBase()
{
  if ( m_Weights != 0 )
    {
      delete m_Weights ;
      m_Weights = 0 ;
    }
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "Membership Function: " << m_MembershipFunction << std::endl;
  os << indent << "Weights Array: " << m_Weights << std::endl;
  os << indent << "Parameters are modified: " << m_ParametersModified
     << std::endl;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetSample(TSample* sample)
{
  m_Sample = sample ;
  this->CreateWeightArray() ;
} 

template< class TSample >
TSample*
MixtureModelComponentBase< TSample >
::GetSample()
{
  return m_Sample ;
} 

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetParameters(const ParametersType &parameters)
{
  if ( m_Parameters != parameters )
    {
      m_Parameters = parameters ;
      this->AreParametersModified(true) ;
    }
}

template< class TSample >
bool
MixtureModelComponentBase< TSample >
::AreParametersModified()
{
  return m_ParametersModified ;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::AreParametersModified(bool flag)
{
  m_ParametersModified = flag;
}

template< class TSample >
typename MixtureModelComponentBase< TSample >::WeightArrayType*
MixtureModelComponentBase< TSample >
::GetWeights()
{
  return m_Weights ;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::CreateWeightArray()
{
  if ( m_Weights != 0 )
    {
      delete m_Weights ;
      m_Weights = 0 ;
    }

  m_Weights = new WeightArrayType(m_Sample->Size());
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::DeleteWeightArray()
{
  delete m_Weights ;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::SetMembershipFunction(MembershipFunctionType* function)
{
  m_MembershipFunction = function ;
}

template< class TSample >
typename MixtureModelComponentBase< TSample >::MembershipFunctionType*
MixtureModelComponentBase< TSample >
::GetMembershipFunction()
{
  return m_MembershipFunction ;
}

template< class TSample >
inline double
MixtureModelComponentBase< TSample >
::Evaluate(MeasurementVectorType& measurements) 
{
  return m_MembershipFunction->Evaluate(measurements) ;
}

template< class TSample >
inline void
MixtureModelComponentBase< TSample >
::SetWeight(int index, double value)
{
  (*m_Weights)[index] = value ;
}

template< class TSample >
inline double
MixtureModelComponentBase< TSample >
::GetWeight(int index)
{
  return (*m_Weights)[index] ;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::Update() 
{
  this->GenerateData() ;
}

template< class TSample >
void
MixtureModelComponentBase< TSample >
::GenerateData()
{
  // subclasses should override this function.
}


} // end of namespace Statistics 
} // end of namespace itk

#endif

