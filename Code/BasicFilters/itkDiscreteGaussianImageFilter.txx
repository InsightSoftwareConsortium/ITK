/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDiscreteGaussianImageFilter_txx
#define _itkDiscreteGaussianImageFilter_txx

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template <class TInputImage, class TOutputImage>
void 
DiscreteGaussianImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr = 
    const_cast< TInputImage *>( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  GaussianOperator<OutputPixelType, ImageDimension> oper;
  typename TInputImage::SizeType radius;
  
  for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
    {
    // Determine the size of the operator in this dimension.  Note that the
    // Gaussian is built as a 1D operator in each of the specified directions.
    oper.SetDirection(i);
    if (m_UseImageSpacing == true)
      {
      if (this->GetInput()->GetSpacing()[i] == 0.0)
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        oper.SetVariance(m_Variance[i] / this->GetInput()->GetSpacing()[i]);
        }
      }
    else
      {
      oper.SetVariance(m_Variance[i]);
      }
    oper.SetMaximumError(m_MaximumError[i]);
    oper.SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper.CreateDirectional();
    
    radius[i] = oper.GetRadius(i);
    }

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
DiscreteGaussianImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TOutputImage::Pointer swapPtrA; 
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  GaussianOperator<OutputPixelType, ImageDimension> *oper;
  oper = new GaussianOperator<OutputPixelType,ImageDimension>;

  typename NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::Pointer filter;
  filter
    = NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::New();

  // Graft this filters output onto the mini-pipeline so that the mini-pipeline
  // has the correct region ivars and will write to this filters bulk data
  // output.
  filter->GraftOutput( output );
  swapPtrA = const_cast< TInputImage * >( this->GetInput() );
  for (unsigned int i = 0; i < m_FilterDimensionality; ++i)
    {
    // Set up the operator for this dimension
    oper->SetDirection(i);
    if (m_UseImageSpacing == true)
      {
      if (this->GetInput()->GetSpacing()[i] == 0.0)
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        oper->SetVariance(m_Variance[i] / this->GetInput()->GetSpacing()[i]);
        }
      }
    else
      {
      oper->SetVariance(m_Variance[i]);
      }

    oper->SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper->SetMaximumError(m_MaximumError[i]);
    oper->CreateDirectional();

    // Set up the filter and run the mini-pipeline
    filter->SetOperator(*oper);
    filter->SetInput(swapPtrA);
    filter->Update();

    // Disconnect the output of the mini-pipeline so that it can be
    // used as the input to the mini-pipeline
    swapPtrA = filter->GetOutput();
    swapPtrA->DisconnectPipeline();
    }
  // Graft the last output of the mini-pipeline onto this filters output so
  // the final output has the correct region ivars and a handle to the final
  // bulk data
  this->GraftOutput(swapPtrA);

  // Clean up
  delete oper;
}

template< class TInputImage, class TOutputImage >
void
DiscreteGaussianImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);

  os << indent << "Variance: " << m_Variance << std::endl;
  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "FilterDimensionality: " << m_FilterDimensionality << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
}

} // end namespace itk

#endif
