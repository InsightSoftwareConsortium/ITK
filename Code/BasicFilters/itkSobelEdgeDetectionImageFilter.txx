/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelEdgeDetectionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSobelEdgeDetectionImageFilter_txx
#define _itkSobelEdgeDetectionImageFilter_txx
#include "itkSobelEdgeDetectionImageFilter.h"

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkSobelOperator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkNaryAddImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSqrtImageFilter.h"


namespace itk
{

template <class TInputImage, class TOutputImage>
void 
SobelEdgeDetectionImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  SobelOperator<OutputPixelType, ImageDimension> oper;
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
SobelEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Define the filter types used.
  typedef NeighborhoodOperatorImageFilter<InputImageType,
    OutputImageType> OpFilter;
  typedef MultiplyImageFilter<OutputImageType,
    OutputImageType,
    OutputImageType> MultFilter;
  typedef NaryAddImageFilter<OutputImageType, OutputImageType> AddFilter;
  typedef SqrtImageFilter<OutputImageType, OutputImageType> SqrtFilter;
  
  unsigned int i;  
  
  typename TOutputImage::Pointer output = this->GetOutput();  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  
  // Create the sobel operator
  SobelOperator<OutputPixelType, ImageDimension> opers[ImageDimension];
  ZeroFluxNeumannBoundaryCondition<TOutputImage> nbc;
  
  // Setup mini-pipelines along each axis.
  typename OpFilter::Pointer opFilter[ImageDimension];
  typename MultFilter::Pointer multFilter[ImageDimension];
  typename AddFilter::Pointer addFilter = AddFilter::New();
  typename SqrtFilter::Pointer sqrtFilter =  SqrtFilter::New();  
  for(i=0; i < ImageDimension; ++i)
    {
    // Create the filters for this axis.
    opFilter[i] = OpFilter::New();
    multFilter[i] = MultFilter::New();
    
    // Set boundary condition and operator for this axis.
    opers[i].SetDirection(i);
    opers[i].CreateOperator();
    opFilter[i]->OverrideBoundaryCondition(&nbc);
    opFilter[i]->SetOperator(opers[i]);
    
    // Setup the mini-pipeline for this axis.
    opFilter[i]->SetInput(this->GetInput());
    multFilter[i]->SetInput1(opFilter[i]->GetOutput());
    multFilter[i]->SetInput2(opFilter[i]->GetOutput());
    
    // All axes' mini-pipelines come together in addFilter.
    addFilter->SetInput(i, multFilter[i]->GetOutput());
    }
  
  // calculate the gradient magnitude
  sqrtFilter->SetInput(addFilter->GetOutput());
  
  // setup the mini-pipeline to calculate the correct regions and
  // write to the appropriate bulk data block
  sqrtFilter->GraftOutput( this->GetOutput() );
  
  // execute the mini-pipeline
  sqrtFilter->Update();
  
  // graft the mini-pipeline output back onto this filter's output.
  // this is needed to get the appropriate regions passed back.
  this->GraftOutput(sqrtFilter->GetOutput());
}

} // end namespace itk

#endif
