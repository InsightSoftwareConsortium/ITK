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
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
namespace itk
{

template< class TPixel, unsigned int VDimension>
void
NeighborhoodOperatorImageFilter<TPixel, VDimension>
::GenerateData()
{
  typedef Image<TPixel, VDimension> ImageType;
  typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI;
  typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension>    RBI;
  typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI> SNIP;
  typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI> SBIP;

  NeighborhoodAlgorithm::ApplyOperatorToEach<SNIP, RNI> f1;
  NeighborhoodAlgorithm::ApplyOperatorToEach<SBIP, RBI> f2;
  
  // Allocate output
  ImageType *output = this->GetOutput();
  ImageType *input  = this->GetInput();
 
  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Filter
  f1(input, output, m_Operator);
  f2(input, output, m_Operator);
}

} // end namespace itk
