/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdImageFilter_txx
#define __itkBinaryThresholdImageFilter_txx

#include "itkBinaryThresholdImageFilter.h"

namespace itk
{
/**
 *
 */
template< class TInputImage, class TOutputImage >
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::BinaryThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits< OutputPixelType >::Zero;
  m_InsideValue    = NumericTraits< OutputPixelType >::max();

  // We are going to create the object with a few default inputs to
  // hold the threshold values.

  typename InputPixelObjectType::Pointer lower = InputPixelObjectType::New();
  lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
  this->ProcessObject::SetNthInput(1, lower);

  typename InputPixelObjectType::Pointer upper = InputPixelObjectType::New();
  upper->Set( NumericTraits< InputPixelType >::max() );
  this->ProcessObject::SetNthInput(2, upper);
}

/**
 *
 */
template< class TInputImage, class TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetLowerThreshold(const InputPixelType threshold)
{
  // first check to see if anything changed
  typename InputPixelObjectType::Pointer lower = this->GetLowerThresholdInput();
  if ( lower && lower->Get() == threshold )
    {
    return;
    }

  // create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters)
  lower = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(1, lower);

  lower->Set(threshold);
  this->Modified();
}

template< class TInputImage, class TOutputImage >
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

template< class TInputImage, class TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelType
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThreshold() const
{
  typename InputPixelObjectType::Pointer lower =
    const_cast< Self * >( this )->GetLowerThresholdInput();

  return lower->Get();
}

template< class TInputImage, class TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThresholdInput()
{
  typename InputPixelObjectType::Pointer lower =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(1) );
  if ( !lower )
    {
    // no input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
    this->ProcessObject::SetNthInput(1, lower);
    }

  return lower;
}

template< class TInputImage, class TOutputImage >
const
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetLowerThresholdInput() const
{
  typename InputPixelObjectType::Pointer lower =
    const_cast< InputPixelObjectType * >( static_cast< const InputPixelObjectType * >( this->ProcessObject::GetInput(1) ) );

  if ( !lower )
    {
    // no input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits< InputPixelType >::NonpositiveMin() );
    const_cast< Self * >( this )->ProcessObject::SetNthInput(1, lower);
    }

  return lower;
}

/**
 *
 */
template< class TInputImage, class TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::SetUpperThreshold(const InputPixelType threshold)
{
  // first check to see if anything changed
  typename InputPixelObjectType::Pointer upper = this->GetUpperThresholdInput();
  if ( upper && upper->Get() == threshold )
    {
    return;
    }

  // create a data object to use as the input and to store this
  // threshold. we always create a new data object to use as the input
  // since we do not want to change the value in any current input
  // (the current input could be the output of another filter or the
  // current input could be used as an input to several filters)
  upper = InputPixelObjectType::New();
  this->ProcessObject::SetNthInput(2, upper);

  upper->Set(threshold);
  this->Modified();
}

template< class TInputImage, class TOutputImage >
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

template< class TInputImage, class TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelType
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThreshold() const
{
  typename InputPixelObjectType::Pointer upper =
    const_cast< Self * >( this )->GetUpperThresholdInput();

  return upper->Get();
}

template< class TInputImage, class TOutputImage >
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThresholdInput()
{
  typename InputPixelObjectType::Pointer upper =
    static_cast< InputPixelObjectType * >( this->ProcessObject::GetInput(2) );
  if ( !upper )
    {
    // no input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits< InputPixelType >::max() );
    this->ProcessObject::SetNthInput(2, upper);
    }

  return upper;
}

template< class TInputImage, class TOutputImage >
const
typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::InputPixelObjectType *
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::GetUpperThresholdInput() const
{
  typename InputPixelObjectType::Pointer upper =
    const_cast< InputPixelObjectType * >( static_cast< const InputPixelObjectType * >( this->ProcessObject::GetInput(2) ) );

  if ( !upper )
    {
    // no input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits< InputPixelType >::max() );
    const_cast< Self * >( this )->ProcessObject::SetNthInput(2, upper);
    }

  return upper;
}

/**
 *
 */
template< class TInputImage, class TOutputImage >
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

/**
 *
 */
template< class TInputImage, class TOutputImage >
void
BinaryThresholdImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // set up the functor values
  typename InputPixelObjectType::Pointer lowerThreshold = this->GetLowerThresholdInput();
  typename InputPixelObjectType::Pointer upperThreshold = this->GetUpperThresholdInput();

  if ( lowerThreshold->Get() > upperThreshold->Get() )
    {
    itkExceptionMacro(<< "Lower threshold cannot be greater than upper threshold.");
    }

  // Setup up the functor
  this->GetFunctor().SetLowerThreshold( lowerThreshold->Get() );
  this->GetFunctor().SetUpperThreshold( upperThreshold->Get() );

  this->GetFunctor().SetInsideValue(m_InsideValue);
  this->GetFunctor().SetOutsideValue(m_OutsideValue);
}
} // end namespace itk

#endif
