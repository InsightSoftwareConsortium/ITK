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
#ifndef itkClampImageFilter_hxx
#define itkClampImageFilter_hxx

#include "itkClampImageFilter.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

namespace Functor
{

template< typename TInput, typename TOutput >
Clamp< TInput, TOutput >
::Clamp() :
  m_LowerBound(NumericTraits< OutputType >::NonpositiveMin()),
  m_UpperBound(NumericTraits< OutputType >::max())
  {}

template< typename TInput, typename TOutput >
Clamp< TInput, TOutput >
::~Clamp() {}

template< typename TInput, typename TOutput >
typename Clamp< TInput, TOutput >::OutputType
Clamp< TInput, TOutput >
::GetLowerBound() const
  { return m_LowerBound; }

template< typename TInput, typename TOutput >
typename Clamp< TInput, TOutput >::OutputType
Clamp< TInput, TOutput >
::GetUpperBound() const
  { return m_UpperBound; }

template< typename TInput, typename TOutput >
void
Clamp< TInput, TOutput >
::SetBounds( const OutputType lowerBound, const OutputType upperBound)
  {
  if(lowerBound > upperBound)
    {
    itkGenericExceptionMacro("invalid bounds: [" << lowerBound << "; " << upperBound << "]");
    }

  m_LowerBound = lowerBound;
  m_UpperBound = upperBound;
  }

template< typename TInput, typename TOutput >
bool
Clamp< TInput, TOutput >
::operator!=( const Self & other ) const
  {
  return m_UpperBound != other.m_UpperBound
    || m_LowerBound != other.m_LowerBound;
  }

template< typename TInput, typename TOutput >
bool
Clamp< TInput, TOutput >
::operator==( const Self & other ) const
  {
  return !(*this != other);
  }

} // end namespace Functor


template <typename TInputImage, typename TOutputImage>
ClampImageFilter< TInputImage, TOutputImage >
::ClampImageFilter() {}

template <typename TInputImage, typename TOutputImage>
typename ClampImageFilter< TInputImage, TOutputImage >::OutputPixelType
ClampImageFilter< TInputImage, TOutputImage >
::GetLowerBound() const
  {
  return this->GetFunctor().GetLowerBound();
  }

template <typename TInputImage, typename TOutputImage>
typename ClampImageFilter< TInputImage, TOutputImage >::OutputPixelType
ClampImageFilter< TInputImage, TOutputImage >
::GetUpperBound() const
  {
  return this->GetFunctor().GetUpperBound();
  }

template <typename TInputImage, typename TOutputImage>
void
ClampImageFilter< TInputImage, TOutputImage >
::SetBounds(const OutputPixelType lowerBound, const OutputPixelType upperBound)
  {
  if ( Math::ExactlyEquals(lowerBound, this->GetFunctor().GetLowerBound()) && Math::ExactlyEquals(upperBound, this->GetFunctor().GetUpperBound()))
    {
    return;
    }

  this->GetFunctor().SetBounds(lowerBound, upperBound);
  this->Modified();
  }

template <typename TInputImage, typename TOutputImage>
void
ClampImageFilter< TInputImage, TOutputImage >
::GenerateData()
  {
  if( this->GetInPlace() && this->CanRunInPlace()
    && this->GetLowerBound() <= NumericTraits< OutputPixelType >::NonpositiveMin()
    && this->GetUpperBound() >= NumericTraits< OutputPixelType >::max() )
    {
    // If the filter is asked to run in-place, is able to run in-place,
    // and the specified bounds are equal to the output-type limits,
    // then there is nothing to do. To avoid iterating over all the pixels for
    // nothing, graft the input to the output, generate a fake progress and exit.
    this->AllocateOutputs();
    ProgressReporter progress(this, 0, 1);
    return;
    }
  Superclass::GenerateData();
  }

template <typename TInputImage, typename TOutputImage>
void
ClampImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
  {
  Superclass::PrintSelf(os, indent);

  os << indent << "Lower bound: "
    << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->GetLowerBound() ) << std::endl;
  os << indent << "Upper bound: "
    << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->GetUpperBound() ) << std::endl;
  }
}

#endif
