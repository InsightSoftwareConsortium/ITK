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
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{

template<class TInnerProduct, class TIterator>
void
GradientMagnitudeStrategy<TInnerProduct, TIterator>
::operator()(ImageType *in, ImageType *out) const
{
  int i;
  ScalarValueType a, g;
  TInnerProduct IP;
  NeighborhoodAlgorithm::CalculateOutputWrapOffsetModifiers<ImageType> OM;
  
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
  long int *mod = new long int[ImageDimension];
  TIterator it(radius, in, in->GetRequestedRegion());
  it.SetOutputBuffer(out->GetBufferPointer() +
                     out->ComputeOffset(it.GetRegion().GetIndex()));
  OM(mod, in, out);
  it.SetOutputWrapOffsetModifier(mod);
  delete[] mod;
  
  // Slice the iterator
  std::slice x_slice[ImageDimension];
  const unsigned long center = it.size() / 2;
  for (i = 0; i < ImageDimension; ++i)
    {
      x_slice[i] = std::slice( center - it.GetStride(i) * radius[i],
                               op.GetSize()[0], it.GetStride(i));
    }

  // Process
  const TIterator _end = it.End();
  for (it = it.Begin(); it != _end; ++it)
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
 
template< class TPixel, unsigned int VDimension>
void
GradientMagnitudeImageFilter<TPixel, VDimension>
::GenerateData()
{
  typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension>    RBI;
  typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI;
  typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI> SNIP;
  typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI> SBIP;
  
  GradientMagnitudeStrategy<SNIP, RNI> f1;
  GradientMagnitudeStrategy<SBIP, RBI> f2;
  
  // Allocate output
  typename ImageType::Pointer output = this->GetOutput();
  typename ImageType::Pointer input  = this->GetInput();

  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Filter
  f1(input, output);
  f2(input, output);
}
  
} // end namespace itk
