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
#ifndef itkCompensatedSummation_hxx
#define itkCompensatedSummation_hxx

#include "itkCompensatedSummation.h"

namespace itk
{

void ITKCommon_EXPORT CompensatedSummationAddElement( float& compensation, float& sum, const float& element );
void ITKCommon_EXPORT CompensatedSummationAddElement( double& compensation, double& sum, const double& element );

#ifndef itkCompensatedSummation_cxx
// We try the looser pragma guards if we don't have an explicit instantiation.
#ifdef __INTEL_COMPILER
#pragma optimize("", off)
#endif // __INTEL_COMPILER
#ifdef _MSC_VER
#pragma float_control(push)
#pragma float_control( precise, on )
#endif // _MSC_VER i.e. Microsoft Visual Studio
#endif // not itkCompensatedSummation_cxx
template <typename TFloat>
/** A helper for the CompensatedSummation class. */
void CompensatedSummationAddElement( TFloat& compensation, TFloat& sum, const TFloat& element, int=0)
{
  typedef typename NumericTraits< TFloat >::AccumulateType AccumulateType;
  const AccumulateType compensatedInput = static_cast< AccumulateType >( element - compensation );
  const AccumulateType tempSum          = sum + compensatedInput;
  // Warning: watch out for the compiler optimizing this out!
  compensation = static_cast<TFloat>( (tempSum - sum) - compensatedInput);
  sum = static_cast<TFloat>( tempSum );
}
#ifndef itkCompensatedSummation_cxx
#ifdef __INTEL_COMPILER
#pragma optimize("", on)
#endif // __INTEL_COMPILER
#ifdef _MSC_VER
#pragma float_control(pop)
#endif // _MSC_VER
#endif // not itkCompensatedSummation_cxx

template < typename TFloat >
CompensatedSummation< TFloat >
::CompensatedSummation():
  m_Sum( NumericTraits< AccumulateType >::ZeroValue() ),
  m_Compensation( NumericTraits< AccumulateType >::ZeroValue() )
{
}

template < typename TFloat >
CompensatedSummation< TFloat >
::CompensatedSummation( const Self & rhs )
{
  this->m_Sum = rhs.m_Sum;
  this->m_Compensation = rhs.m_Compensation;
}

template < typename TFloat >
typename CompensatedSummation<TFloat>::Self &
CompensatedSummation< TFloat >
::operator=( const Self & rhs )
{
  if ( this != &rhs )
    {
    this->m_Sum = rhs.m_Sum;
    this->m_Compensation = rhs.m_Compensation;
    }
  return *this;
}

template < typename TFloat >
void
CompensatedSummation< TFloat >
::AddElement( const FloatType & element )
{
  CompensatedSummationAddElement( this->m_Compensation, this->m_Sum, element );
}

template < typename TFloat >
CompensatedSummation< TFloat > &
CompensatedSummation< TFloat >
::operator+=( const FloatType & rhs )
{
  this->AddElement( rhs );
  return *this;
}

template < typename TFloat >
CompensatedSummation< TFloat > &
CompensatedSummation< TFloat >
::operator-=( const FloatType & rhs )
{
  this->AddElement( - rhs );
  return *this;
}

template < typename TFloat >
CompensatedSummation< TFloat > &
CompensatedSummation< TFloat >
::operator*=( const FloatType & rhs )
{
  this->m_Sum *= rhs;
  this->m_Compensation *= rhs;
  return *this;
}

template < typename TFloat >
CompensatedSummation< TFloat > &
CompensatedSummation< TFloat >
::operator/=( const FloatType & rhs )
{
  this->m_Sum /= rhs;
  this->m_Compensation /= rhs;
  return *this;
}

template < typename TFloat >
void
CompensatedSummation< TFloat >
::ResetToZero()
{
  this->m_Sum          = NumericTraits< AccumulateType >::ZeroValue();
  this->m_Compensation = NumericTraits< AccumulateType >::ZeroValue();
}

template < typename TFloat >
CompensatedSummation< TFloat > &
CompensatedSummation< TFloat >
::operator=( const FloatType & rhs )
{
  this->m_Sum          = rhs;
  this->m_Compensation = NumericTraits< AccumulateType >::ZeroValue();

  return *this;
}

template < typename TFloat >
const typename CompensatedSummation< TFloat >::AccumulateType &
CompensatedSummation< TFloat >
::GetSum() const
{
  return this->m_Sum;
}

} // end namespace itk

#endif
