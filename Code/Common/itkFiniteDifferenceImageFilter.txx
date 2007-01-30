/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkFiniteDifferenceImageFilter.h"

namespace itk {
  
template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  if (this->GetState() == UNINITIALIZED)
    {
    // Set the coefficients for the deriviatives
    double coeffs[TInputImage::ImageDimension];
    if (m_UseImageSpacing)
      {
      for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
        {
        coeffs[i] = 1.0 / this->GetInput()->GetSpacing()[i];
        }
      }
    else
      {
      for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
        {
        coeffs[i] = 1.0;
        }
      }
    m_DifferenceFunction->SetScaleCoefficients(coeffs);

    // Allocate the output image
    this->AllocateOutputs();

    // Copy the input image to the output image.  Algorithms will operate
    // directly on the output image and the update buffer.
    this->CopyInputToOutput();

    // Perform any other necessary pre-iteration initialization.
    this->Initialize();
    
    // Allocate the internal update buffer.  This takes place entirely within
    // the subclass, since this class cannot define an update buffer type.
    this->AllocateUpdateBuffer();

    this->SetStateToInitialized();
    m_ElapsedIterations = 0;
    }
    
  // Iterative algorithm
  TimeStepType dt;

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
    if( this->GetAbortGenerateData() )
      {
      this->InvokeEvent( IterationEvent() );
      this->ResetPipeline(); 
      throw ProcessAborted(__FILE__,__LINE__);
      }
    }

  if (m_ManualReinitialization == false)
    {
    this->SetStateToUninitialized(); // Reset the state once execution is
                                     // completed
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

//     std::cout << "inputRequestedRegion: " << inputRequestedRegion << std::endl;
//     std::cout << "largestPossibleRegion: " << inputPtr->GetLargestPossibleRegion() << std::endl;

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
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }

}

template <class TInputImage, class TOutputImage>
typename FiniteDifferenceImageFilter<TInputImage, TOutputImage>::TimeStepType
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::ResolveTimeStep(const TimeStepType *timeStepList, const bool *valid, int size)
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
      min = timeStepList[i];
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
    {      if ( valid[i] && (timeStepList[i] < min) )   min = timeStepList[i];      }

  return min;
}

template <class TInputImage, class TOutputImage>
bool
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::Halt()
{
  if (m_NumberOfIterations != 0)
    {
    this->UpdateProgress( static_cast<float>( this->GetElapsedIterations() ) /
                          static_cast<float>( m_NumberOfIterations ) );
    }

  if (this->GetElapsedIterations() >= m_NumberOfIterations)
    {
    return true;
    }
  else if ( this->GetElapsedIterations() == 0)
    {
    return false; 
    }
  else if ( this->GetMaximumRMSError() > m_RMSChange )
    {
    return true;
    }
  else
    { 
    return false; 
    }
}


template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ElapsedIterations: " << m_ElapsedIterations << std::endl;
  os << indent << "UseImageSpacing: " << (m_UseImageSpacing ? "On" : "Off") << std::endl;
  os << indent << "State: " << m_State << std::endl;
  os << indent << "MaximumRMSError: " << m_MaximumRMSError << std::endl;
  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
  os << indent << "ManualReinitialization: " << m_ManualReinitialization << std::endl;
  os << indent << "RMSChange: " << m_RMSChange << std::endl;
  os << std::endl;
  if (m_DifferenceFunction)
    {
    os << indent << "DifferenceFunction: " << std::endl;
    m_DifferenceFunction->Print(os,indent.GetNextIndent());
    }
  else
    {
    os << indent << "DifferenceFunction: " << "(None)" << std::endl;
    }
  os << std::endl;
}


}// end namespace itk

#endif
