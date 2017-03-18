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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBinaryThresholdImageFilter_hxx
#define itkBinaryThresholdImageFilter_hxx

#include "itkBinaryThresholdImageFilter.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::BinaryThresholdImageFilter() :
  m_InsideValue( NumericTraits< OutputPixelType >::max() ),
  m_OutsideValue( NumericTraits< OutputPixelType >::ZeroValue() )
{
  // We are going to create the object with a few default inputs to
  // hold the threshold values.

  typename InputPixelObjectType::Pointer lower = InputPixelObjectType::New();
  lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
  this->ProcessObject::SetNthInput(1, lower);

  typename InputPixelObjectType::Pointer upper = InputPixelObjectType::New();
  upper->Set( NumericTraits< InputPixelType >::max() );
  this->ProcessObject::SetNthInput(2, upper);
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetLowerThreshold(const InputPixelType threshold)
{
  // First check to see if anything changed
  typename InputPixelObjectType::Pointer lower = this->GetLowerThresholdInput();
  if ( lower && Math::ExactlyEquals(lower->Get(), threshold) )
    {
    return;
    }

  // Create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters)
  lower = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(1, lower);

  lower->Set(threshold);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetLowerThresholdInput(const InputPixelObjectType *input)
{
  if ( input != this->GetLowerThresholdInput() )
    {
    this->ProcessObject::SetNthInput( 1,
                                      const_cast< InputPixelObjectType * >( input ) );
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelType
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThreshold() const
{
  typename InputPixelObjectType::Pointer lower =
    const_cast< Self * >( this )->GetLowerThresholdInput();

  return lower->Get();
}

template< typename TInputImage, typename TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThresholdInput()
{
  typename InputPixelObjectType::Pointer lower =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(1) );
  if ( !lower )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
    this->ProcessObject::SetNthInput(1, lower);
    }

  return lower;
}

template< typename TInputImage, typename TOutputImage >
const
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThresholdInput() const
{
  typename InputPixelObjectType::Pointer lower =
    const_cast< InputPixelObjectType * >( static_cast< const InputPixelObjectType * >( this->ProcessObject::GetInput(1) ) );

  if ( !lower )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
    const_cast< Self * >( this )->ProcessObject::SetNthInput(1, lower);
    }

  return lower;
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetUpperThreshold(const InputPixelType threshold)
{
  // First check to see if anything changed
  typename InputPixelObjectType::Pointer upper = this->GetUpperThresholdInput();
  if ( upper && Math::ExactlyEquals(upper->Get(), threshold) )
    {
    return;
    }

  // Create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters)
  upper = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(2, upper);

  upper->Set(threshold);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetUpperThresholdInput(const InputPixelObjectType *input)
{
  if ( input != this->GetUpperThresholdInput() )
    {
    this->ProcessObject::SetNthInput( 2,
                                      const_cast< InputPixelObjectType * >( input ) );
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelType
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThreshold() const
{
  typename InputPixelObjectType::Pointer upper =
    const_cast< Self * >( this )->GetUpperThresholdInput();

  return upper->Get();
}

template< typename TInputImage, typename TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThresholdInput()
{
  typename InputPixelObjectType::Pointer upper =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(2) );
  if ( !upper )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits< InputPixelType >::max() );
    this->ProcessObject::SetNthInput(2, upper);
    }

  return upper;
}

template< typename TInputImage, typename TOutputImage >
const
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThresholdInput() const
{
  typename InputPixelObjectType::Pointer upper =
    const_cast< InputPixelObjectType * >( static_cast< const InputPixelObjectType * >( this->ProcessObject::GetInput(2) ) );

  if ( !upper )
    {
    // No input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits< InputPixelType >::max() );
    const_cast< Self * >( this )->ProcessObject::SetNthInput(2, upper);
    }

  return upper;
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // Set up the functor values
  typename InputPixelObjectType::Pointer lowerThreshold = this->GetLowerThresholdInput();
  typename InputPixelObjectType::Pointer upperThreshold = this->GetUpperThresholdInput();

  if ( lowerThreshold->Get() > upperThreshold->Get() )
    {
    itkExceptionMacro(<< "Lower threshold cannot be greater than upper threshold.");
    }

  // Set up the functor
  this->GetFunctor().SetLowerThreshold( lowerThreshold->Get() );
  this->GetFunctor().SetUpperThreshold( upperThreshold->Get() );

  this->GetFunctor().SetInsideValue(m_InsideValue);
  this->GetFunctor().SetOutsideValue(m_OutsideValue);
}

template< typename TInputImage, typename TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutsideValue ) << std::endl;
  os << indent << "InsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_InsideValue ) << std::endl;
  os << indent << "LowerThreshold: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( this->GetLowerThreshold() ) << std::endl;
  os << indent << "UpperThreshold: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( this->GetUpperThreshold() ) << std::endl;
}
} // end namespace itk

#endif
