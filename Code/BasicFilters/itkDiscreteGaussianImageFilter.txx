/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkDiscreteGaussianImageFilter_txx
#define _itkDiscreteGaussianImageFilter_txx

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
void
DiscreteGaussianImageFilter< TInputImage, TOutputImage >
::ImageRegionCopy(TOutputImage *imgT, TInputImage *input)
{
  ImageRegionIterator<TInputImage> in_it(input,
                                         imgT->GetRequestedRegion());
  ImageRegionIterator<TOutputImage> out_it(imgT,
                                           imgT->GetRequestedRegion());
  for (in_it = in_it.Begin(), out_it = out_it.Begin(); in_it < in_it.End();
       ++in_it, ++out_it)
    {
    out_it.Set( in_it.Get() );
    }
}

  
template< class TInputImage, class TOutputImage >
void
DiscreteGaussianImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typename TInputImage::Pointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TOutputImage::Pointer swapPtrA, swapPtrB, swapPtrC;
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  typename TOutputImage::Pointer imgT = TOutputImage::New();
  imgT->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  imgT->SetRequestedRegion(output->GetRequestedRegion());
  imgT->SetBufferedRegion(output->GetBufferedRegion());
  imgT->Allocate();

  Self::ImageRegionCopy(imgT, input);

  GaussianOperator<ImageDimension> *oper;
  NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::Pointer filter;

  swapPtrA = imgT;
  swapPtrB = output;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      // Filter
      oper = new GaussianOperator<ImageDimension>;
      oper->SetDirection(i);
      oper->SetVariance(m_Variance[i]);
      oper->SetMaximumError(m_MaximumError[i]);
      oper->CreateDirectional();
      
      filter = NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::New();
      filter->SetOperator(*oper);
      filter->SetInput(swapPtrA);
      filter->SetOutput(swapPtrB);
      filter->Update();

      //      delete oper;       pipeline problems cause seg fault? --3/13/01
      //      filter->Delete();

      swapPtrC = swapPtrB;
      swapPtrB = swapPtrA;
      swapPtrA = swapPtrC;
    }

  if ((ImageDimension % 2) == 0)
    { 
      Self::ImageRegionCopy(output, imgT);
    }
}

} // end namespace itk

#endif
