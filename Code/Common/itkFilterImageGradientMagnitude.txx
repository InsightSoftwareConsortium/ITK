/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGradientMagnitude.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
namespace itk
{

template< class TPixel, unsigned int VDimension>
void
FilterImageGradientMagnitude<TPixel, VDimension>
::GenerateData()
{
  // Allocate output
  typename ImageType::Pointer output = this->GetOutput();
  typename ImageType::Pointer input  = this->GetInput();

  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy input to output so that non-scalar pixels are passed through?
  
  // Filter
  RegionNeighborhoodIterator<TPixel, VDimension> rni;
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension> bni;
  this->GradientMagnitude(&rni, input, output, false);
  this->GradientMagnitude(&bni, input, output, true);
}

template< class TPixel, unsigned int VDimension>
void
FilterImageGradientMagnitude<TPixel, VDimension>
::GradientMagnitude(NeighborhoodIterator<TPixel, VDimension> *it,
                    ImageType *input,
                    ImageType *output,
                    bool IncludeBoundaryPixels) 
{
  // Set up operators
  DerivativeOperator<TPixel, VDimension> op;
   op.SetDirection(0);
   op.SetOrder(1);
   op.CreateDirectional();

  // Calculate iterator radius
  Size<VDimension> radius;
  for (int i = 0; i < VDimension; ++i)
    {
      radius[i]  = op.GetRadius()[0];
      //      std::cout << "HoodRadius =" << radius[i] << std::endl;
    }

   // Set up iteration region
  ImageRegion<VDimension> region;
  Size<VDimension> sz;
  Index<VDimension> idx;
  
  if (IncludeBoundaryPixels)
    {
      region = output->GetRequestedRegion();
    }
  else
    {
      for (int i = 0; i < VDimension; ++i)
        {
          sz[i] = output->GetRequestedRegion().GetSize()[i]
            -   2 * radius[i];
          idx[i] = output->GetRequestedRegion().GetIndex()[i]
            + radius[i];
        }
      region.SetSize(sz);
      region.SetIndex(idx);
    }
  
  // Initialize the iterator
  it->Initialize(radius, input, region);

  // Set the output buffer and its parameters
  long bufferSizeDifference[VDimension];
  for (int i = 0; i < VDimension; ++i)
    {
      bufferSizeDifference[i] = output->GetBufferedRegion().GetSize()[i]
        - input->GetBufferedRegion().GetSize()[i];
    }
  it->SetOutputBuffer(output->GetBufferPointer()
                      + output->ComputeOffset(it->GetRegion().GetIndex()));
  it->SetOutputWrapOffsetModifier(bufferSizeDifference);
  
  // Slice the iterator
  std::slice x_slice[VDimension];
  const unsigned long center = it->size() / 2;
  for (int i = 0; i < VDimension; ++i)
    {
      x_slice[i] = std::slice( center - it->GetStride(i) * radius[i],
                               op.GetSize()[0], it->GetStride(i));
    }

  // Process
  typename ScalarTraits<TPixel>::ScalarValueType accumulator;
  typename ScalarTraits<TPixel>::ScalarValueType grad;
  for (it->SetToBegin(); ! it->IsAtEnd(); it->operator++())
    {
      accumulator = NumericTraits<typename
                           ScalarTraits<TPixel>::ScalarValueType>::Zero;
      for (int i = 0; i < VDimension; ++i)
        {
          grad = it->SlicedInnerProduct(x_slice[i], op);
          accumulator += grad * grad;;
        }
      *(it->GetOutputBuffer()) = std::sqrt(accumulator);
     
    }

}
  
} // end namespace itk
