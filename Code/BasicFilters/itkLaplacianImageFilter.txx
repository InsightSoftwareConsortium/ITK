/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLaplacianImageFilter_txx
#define _itkLaplacianImageFilter_txx
#include "itkLaplacianImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkLaplacianOperator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
void
LaplacianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream& os, Indent indent) const  
{
  Superclass::PrintSelf(os,indent);
  os << indent << "UseImageSpacing = " << m_UseImageSpacing << std::endl;
}


template <class TInputImage, class TOutputImage>
void 
LaplacianImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast< TInputImage * > ( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  LaplacianOperator<OutputPixelType, ImageDimension> oper;
  oper.CreateOperator();

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
LaplacianImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  double s[ImageDimension];
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  ZeroFluxNeumannBoundaryCondition<TOutputImage> nbc;
  
  // Create the Laplacian operator
  LaplacianOperator<OutputPixelType, ImageDimension> oper;
  for (unsigned i = 0; i < ImageDimension; i++)
    {
    if (this->GetInput()->GetSpacing()[i] == 0.0 )
      {
      itkExceptionMacro( << "Image spacing cannot be zero" );
      }
    else
      {
      s[i] = 1.0 / this->GetInput()->GetSpacing()[i];
      }
    }
  oper.SetDerivativeScalings( s );
  oper.CreateOperator();

  
  typedef NeighborhoodOperatorImageFilter<InputImageType, OutputImageType> NOIF;
  typename NOIF::Pointer filter = NOIF::New();
  
  filter->OverrideBoundaryCondition(&nbc);

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  progress->RegisterInternalFilter(filter,1.0f);

  //
  // set up the mini-pipline
  //
  filter->SetOperator(oper);
  filter->SetInput(this->GetInput());

  // graft this filter's output to the mini-pipeline.  this sets up
  // the mini-pipeline to write to this filter's output and copies
  // region ivars and meta-data
  filter->GraftOutput(output);

  // execute the mini-pipeline
  filter->Update();

  // graft the output of the mini-pipeline back onto the filter's output.
  // this copies back the region ivars and meta-dataig
  this->GraftOutput(filter->GetOutput());
}

} // end namespace itk

#endif
