/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkGradientMagnitudeImageFilter_txx
#define _itkGradientMagnitudeImageFilter_txx

#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"

namespace itk
{

template<class TInnerProduct, class TIterator>
void
GradientMagnitudeStrategy<TInnerProduct, TIterator>
::operator()(ImageType *in, ImageType *out) const
{
  unsigned int i;
  ScalarValueType a, g;
  TInnerProduct IP;
  NeighborhoodAlgorithm::CalculateOutputWrapOffsetModifiers<ImageType> OM;
  ZeroFluxNeumannBoundaryCondition<ImageType> nbc;
  
  // Set up operators
  DerivativeOperator<PixelType, ImageDimension> op;
   op.SetDirection(0);
   op.SetOrder(1);
   op.CreateDirectional();

  // Calculate iterator radius
  Size<ImageDimension> radius;
  for (i = 0; i < ImageDimension; ++i) radius[i]  = op.GetRadius()[0];

  // Create an iterator. The output buffer pointer of the iterator is set
  // up to account for any differences in the buffer size of the two images.
  Offset<ImageDimension> mod;
  TIterator it(radius, in, in->GetRequestedRegion());
  it.SetOutputBuffer(out->GetBufferPointer() +
                     out->ComputeOffset(it.GetRegion().GetIndex()));
  mod = OM(in, out);
  it.SetOutputWrapOffsetModifier(mod);

  it.OverrideBoundaryCondition(&nbc);
  
  // Slice the iterator
  std::slice x_slice[ImageDimension];
  const unsigned long center = it.Size() / 2;
  for (i = 0; i < ImageDimension; ++i)
    {
      x_slice[i] = std::slice( center - it.GetStride(i) * radius[i],
                               op.GetSize()[0], it.GetStride(i));
    }

  // Process
  for (it = it.Begin(); !it.IsAtEnd(); ++it)
    {
      a = NumericTraits<ScalarValueType>::Zero;
      for (i = 0; i < ImageDimension; ++i)
        {
          g = IP(x_slice[i], it, op);
          a += g * g;
        }
      *(it.GetOutputBuffer()) = ::sqrt(a);     
    }
}
 
template< class TInputImage, class TOutputImage >
void
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typedef RegionBoundaryNeighborhoodIterator<TOutputImage>    RBI;
  typedef RegionNonBoundaryNeighborhoodIterator<TOutputImage> RNI;
  typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI,
    NeighborhoodOperator<OutputPixelType, ImageDimension> > SNIP;
  typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI,
    NeighborhoodOperator<OutputPixelType, ImageDimension> > SBIP;
  
  GradientMagnitudeStrategy<SNIP, RNI> f1;
  GradientMagnitudeStrategy<SBIP, RBI> f2;
  
  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::Pointer input  = this->GetInput();

  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Filter
  f1(input, output);
  f2(input, output);
}
  
} // end namespace itk

#endif
