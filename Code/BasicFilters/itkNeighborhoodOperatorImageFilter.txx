/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkNeighborhoodOperatorImageFilter_txx
#define _itkNeighborhoodOperatorImageFilter_txx

#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
namespace itk
{

template< class TInputImage, class TOutputImage>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typedef RegionNonBoundaryNeighborhoodIterator<OutputImageType> RNI;
  typedef RegionBoundaryNeighborhoodIterator<OutputImageType>  RBI;
  typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI,
    Neighborhood<OutputPixelType, ImageDimension> > SNIP;
  typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI,
    Neighborhood<OutputPixelType, ImageDimension> > SBIP;

  NeighborhoodAlgorithm::ApplyOperatorToEach<SNIP, RNI> f1;
  NeighborhoodAlgorithm::ApplyOperatorToEach<SBIP, RBI> f2;
  
  // Allocate output
  OutputImageType *output = this->GetOutput();
  InputImageType *input  = this->GetInput();
 
  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Filter
  f1(input, output, m_Operator);
  f2(input, output, m_Operator);
}

} // end namespace itk

#endif
