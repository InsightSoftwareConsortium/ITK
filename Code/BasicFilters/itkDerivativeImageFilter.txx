/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDerivativeImageFilter_txx
#define _itkDerivativeImageFilter_txx
#include "itkDerivativeImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
void 
DerivativeImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr = 
    const_cast< InputImageType * >( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  DerivativeOperator<OutputPixelType, ImageDimension> oper;
  oper.SetDirection(m_Direction);
  oper.SetOrder(m_Order);
  oper.CreateDirectional();

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( oper.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template< class TInputImage, class TOutputImage >
void
DerivativeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  ZeroFluxNeumannBoundaryCondition<TOutputImage> nbc;
  
  // Filter
  DerivativeOperator<OutputPixelType, ImageDimension> oper;
  oper.SetDirection(m_Direction);
  oper.SetOrder(m_Order);
  oper.CreateDirectional();
  oper.FlipAxes();

  if (m_UseImageSpacing == true)
    {
    if ( this->GetInput()->GetSpacing()[m_Direction] == 0.0 )
      {
      itkExceptionMacro(<< "Image spacing cannot be zero.");
      }
    else
      {
      oper.ScaleCoefficients( 1.0 / this->GetInput()->GetSpacing()[m_Direction] );
      }
    }

  typename NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>
    ::Pointer filter =
    NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>
    ::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  progress->RegisterInternalFilter(filter,1.0f);

  filter->OverrideBoundaryCondition(&nbc);

  //
  // Set up the mini-pipline
  //
  filter->SetOperator(oper);
  filter->SetInput(this->GetInput());

  // Graft this filter's output to the mini-pipeline.  this sets up
  // the mini-pipeline to write to this filter's output and copies
  // region ivars and meta-data
  filter->GraftOutput(this->GetOutput());

  // Execute the mini-pipeline.
  filter->Update();

  // Graft the output of the mini-pipeline back onto the filter's output,
  // this copies back the region ivars and meta-data.
  this->GraftOutput(filter->GetOutput());
}

template< class TInputImage, class TOutputImage >
void
DerivativeImageFilter< TInputImage, TOutputImage >::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Order: " << m_Order << std::endl;
  os << indent << "Direction: " << m_Direction << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
}

} // end namespace itk

#endif
