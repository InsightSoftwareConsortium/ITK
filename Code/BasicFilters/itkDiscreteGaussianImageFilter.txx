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
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"
namespace itk
{

template< class TPixel, unsigned int VDimension>
void
DiscreteGaussianImageFilter<TPixel, VDimension>
::ImageRegionCopy(Image<TPixel, VDimension> *imgT,
                  Image<TPixel, VDimension> *input)
{
  ImageRegionIterator<TPixel, VDimension> in_it(input,
                                                imgT->GetRequestedRegion());
  ImageRegionIterator<TPixel, VDimension> out_it(imgT,
                                                imgT->GetRequestedRegion());
  for (in_it = in_it.Begin(), out_it = out_it.Begin(); in_it < in_it.End();
       ++in_it, ++out_it)
    {
      *out_it = *in_it;
    }
}

  
template< class TPixel, unsigned int VDimension>
void
DiscreteGaussianImageFilter<TPixel, VDimension>
::GenerateData()
{
  typedef Image<TPixel, VDimension> ImageType;
  ImageType::Pointer input = this->GetInput();
  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer swapPtrA, swapPtrB, swapPtrC;
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  ImageType::Pointer imgT = ImageType::New();
  imgT->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  imgT->SetRequestedRegion(output->GetRequestedRegion());
  imgT->SetBufferedRegion(output->GetBufferedRegion());
  imgT->Allocate();

  Self::ImageRegionCopy(imgT, input);

  GaussianOperator<VDimension> *oper;
  NeighborhoodOperatorImageFilter<TPixel, VDimension>::Pointer filter;

  swapPtrA = imgT;
  swapPtrB = output;
  for (int i = 0; i < VDimension; ++i)
    {
      // Filter
      oper = new GaussianOperator<VDimension>;
      oper->SetDirection(i);
      oper->SetVariance(m_Variance[i]);
      oper->SetMaximumError(m_MaximumError[i]);
      oper->CreateDirectional();
      
      filter = NeighborhoodOperatorImageFilter<TPixel, VDimension>::New();
      filter->SetOperator(*oper);
      filter->SetInput(swapPtrA);
      filter->SetOutput(swapPtrB);
      filter->Update();

      delete oper;
      filter->Delete();

      swapPtrC = swapPtrB;
      swapPtrB = swapPtrA;
      swapPtrA = swapPtrC;
    }

  if ((VDimension % 2) == 0)
    { 
      Self::ImageRegionCopy(output, imgT);
    }
}

} // end namespace itk
