/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFiniteDifferenceImageFilter_txx_
#define __itkFiniteDifferenceImageFilter_txx_

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkExceptionObject.h"
#include "itkEventObject.h"

namespace itk {

template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::CopyInputToOutput()
{
  typename TInputImage::ConstPointer  input  = this->GetInput();
  typename TOutputImage::Pointer      output = this->GetOutput();
  
  ImageRegionConstIterator<TInputImage>  in(input, output->GetRequestedRegion());
  ImageRegionIterator<TOutputImage> out(output, output->GetRequestedRegion());

  while( ! out.IsAtEnd() )
    {
      out.Value() =  in.Get();  // Supports input image adaptors only
      ++in;
      ++out;
    }
}
  
template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output image
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy the input image to the output image.  Algorithms will operate
  // directly on the output image and the update buffer.
  this->CopyInputToOutput();

  // Perform any other necessary pre-iteration initialization.
  this->Initialize();

  // Allocate the internal update buffer.  This takes place entirely within
  // the subclass, since this class cannot define an update buffer type.
  this->AllocateUpdateBuffer();

  // Iterative algorithm
  TimeStepType dt;
  m_ElapsedIterations = 0;
  while ( ! this->Halt() )
    {
    this->InitializeIteration(); // An optional method for precalculating
                                 // global values, or otherwise setting up
                                 // for the next iteration
    dt = this->CalculateChange();
    this->ApplyUpdate(dt);
    ++m_ElapsedIterations;

    // Invoke the iteration event.
    this->InvokeEvent( IterationEvent() );
    this->InvokeEvent( ProgressEvent() );
    }

  // Any further processing of the solution can be done here.
  this->PostProcessOutput();
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
FiniteDifferenceImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input
  typename Superclass::InputImagePointer  inputPtr  = 
    const_cast< TInputImage * >( this->GetInput());

  if ( !inputPtr )
    {
    return;
    }

  // Get the size of the neighborhood on which we are going to operate.  This
  // radius is supplied by the difference function we are using.
  typename FiniteDifferenceFunctionType::RadiusType radius
    = this->GetDifferenceFunction()->GetRadius();

  // Try to set up a buffered region that will accommodate our
  // neighborhood operations.  This may not be possible and we
  // need to be careful not to request a region outside the largest
  // possible region, because the pipeline will give us whatever we
  // ask for.

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

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
    msg << (const char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }

}

template <class TInputImage, class TOutputImage>
typename FiniteDifferenceImageFilter<TInputImage, TOutputImage>::TimeStepType
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::ResolveTimeStep(const TimeStepType *list, const bool *valid, int size)
{  
  TimeStepType min;
  bool flag;
  min = NumericTraits<TimeStepType>::Zero;
  
  // grab first valid value
  flag = false;
  for (int i = 0; i < size; ++i)
    {
      if (valid[i])
        {
          min = list[i];
          flag = true;
          break;
        }
    }
  
  if (!flag)
    {  // no values!
      throw ExceptionObject(__FILE__, __LINE__);
    }

  // find minimum value
  for (int i = 0; i < size; ++i)
    {      if ( valid[i] && (list[i] < min) )   min = list[i];      }

  return min;
}

template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ElapsedIterations: " << m_ElapsedIterations;
  os << std::endl;
  os << indent << "DifferenceFunction: " << m_DifferenceFunction;
  os << std::endl;
}


}// end namespace itk

#endif
