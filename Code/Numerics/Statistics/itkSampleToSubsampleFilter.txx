/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleToSubsampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleToSubsampleFilter_txx
#define __itkSampleToSubsampleFilter_txx

#include "itkSampleToSubsampleFilter.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
SampleToSubsampleFilter< TSample >
::SampleToSubsampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< class TSample >
SampleToSubsampleFilter< TSample >
::~SampleToSubsampleFilter()
{}

template< class TSample >
void
SampleToSubsampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< SampleType * >( sample ) );
}

template< class TSample >
const typename
SampleToSubsampleFilter< TSample >::SampleType *
SampleToSubsampleFilter< TSample >
::GetInput() const
{
  const SampleType *input =
    static_cast< const SampleType * >( this->ProcessObject::GetInput(0) );

  return input;
}

template< class TSample >
typename SampleToSubsampleFilter< TSample >::DataObjectPointer
SampleToSubsampleFilter< TSample >
::MakeOutput(unsigned int)
{
  return static_cast< DataObject * >( SubsampleType::New().GetPointer() );
}

template< class TSample >
const typename SampleToSubsampleFilter< TSample >::OutputType *
SampleToSubsampleFilter< TSample >
::GetOutput() const
{
  const SubsampleType *output =
    static_cast< const SubsampleType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< class TSample >
void
SampleToSubsampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end of namespace Statistics
} // end of namespace itk

#endif
