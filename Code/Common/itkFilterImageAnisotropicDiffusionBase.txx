/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAnisotropicDiffusionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkDerivativeHalfForwardOperator.h"
#include "itkDerivativeHalfBackwardOperator.h"
#include "itkDerivativeOperator.h"
#include "itkImageRegionIterator.h"
namespace itk
{
  
template<class TPixel, unsigned int VDimension>
void
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>
::UpdateOutputScalar(ImageType *ip, const TPixelScalarValueType multiplier)
{
  // Update the scalar portion of the output only.
  ImageRegionIterator<TPixel, VDimension>
    update(ip, ip->GetRequestedRegion());
  ImageRegionIterator<TPixel, VDimension>
    output(this->GetOutput(), ip->GetRequestedRegion());
  
  update = update.Begin();
  output = output.Begin();

  while ( ! update.IsAtEnd() )
    {
      //*output += *update * multiplier;
      ScalarTraits<TPixel>::SetScalar(*output,
               ScalarTraits<TPixel>::GetScalar(*output)  +
               ScalarTraits<TPixel>::GetScalar(*update)  *    multiplier);
      ++update;
      ++output;
    }
  
}


template<class TPixel, unsigned int VDimension>
void
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>
::UpdateOutputScalar(ImageType *ip, const TPixelScalarValueType multiplier,
                     const VectorComponentDataAccessor<TPixel,
                     TPixelVectorValueType> &accessor)
{

  // Update the scalar portion of the output only.
  ImageRegionIterator<TPixel, VDimension>
    update(ip, ip->GetRequestedRegion());
  ImageRegionIterator<TPixel, VDimension>
    output(this->GetOutput(), ip->GetRequestedRegion());
  
  update = update.Begin();
  output = output.Begin();

  while ( ! update.IsAtEnd() )
    {
      //*output += *update * multiplier;
      accessor.Set(*output, accessor.Get(*output) + accessor.Get(*update) *
                   multiplier);
      //      cout << accessor.Get(*update) * multiplier << endl;
      ++update;
      ++output;
    }
  
}


  
template<class TPixel, unsigned int VDimension>
void
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>
::CopyInputToOutput()
{
  // Copies the entire pixel, not just the scalar part.
  ImageRegionIterator<TPixel, VDimension>
    it(this->GetInput(), this->GetOutput()->GetRequestedRegion());
  TPixel *out = this->GetOutput()->GetBufferPointer();

  it = it.Begin();
  while( ! it.IsAtEnd() )
    {
      *out = *it;
      ++out;
      ++it;
    }  
}

template<class TPixel, unsigned int VDimension>
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>::TPixelScalarValueType
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>
::AverageGradientMagnitudeScalar(ImageType *ip,
                           const ImageRegion<VDimension> &region)
{
  // Average gradient magnitude of the SCALAR portion of the data only.
  TPixelScalarValueType accumulator;
  TPixelScalarValueType val;
  unsigned long counter;
  typedef RegionNeighborhoodIterator<TPixel, VDimension>
    RegionNeighborhoodIterator;
  
  RegionNeighborhoodIterator iterator_list[VDimension];
  DerivativeOperator<TPixel, VDimension> operator_list[VDimension];
  TPixel gradient_value_list[VDimension];
  
  ImageRegion<VDimension> iteration_region;
  Size<VDimension> iteration_size;
  Index<VDimension> iteration_start;
  
  // Shrink the iteration region to exclude boundary pixels
  operator_list[0].SetOrder(1);
  operator_list[0].SetDirection(0);
  operator_list[0].CreateDirectional();
  
  unsigned long offset = operator_list[0].GetRadius()[0];
  for (int i = 0; i < VDimension; ++i)
    {
      iteration_size[i]  = region.GetSize()[i]  - offset*2;
      iteration_start[i] = region.GetIndex()[i] + offset;
    }
  iteration_region.SetSize(iteration_size);
  iteration_region.SetIndex(iteration_start);
  
  iterator_list[0] = RegionNeighborhoodIterator(operator_list[0].GetRadius(),
                                                ip, iteration_region);
  // Set up the rest of the derivative operators and their iterators
  for (int i = 1; i < VDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      iterator_list[i] =
        RegionNeighborhoodIterator(operator_list[i].GetRadius(),
                                   ip, iteration_region);
      iterator_list[i] = iterator_list[i].Begin();
    }

  // Now do the actual processing
  accumulator = NumericTraits<TPixelScalarValueType>::Zero;
  counter     = 0;
  const RegionNeighborhoodIterator iterator_end = iterator_list[0].End();
  for (iterator_list[0] = iterator_list[0].Begin();
       iterator_list[0] < iterator_end; ++counter)
    {
      for (int i = 0; i < VDimension; ++i)
        {
          val = iterator_list[i].InnerProduct(operator_list[i]);     
          accumulator += val * val;
          ++iterator_list[i];
        }
    }

  return (accumulator / counter);
}

template<class TPixel, unsigned int VDimension>
void
FilterImageAnisotropicDiffusionBase<TPixel, VDimension>
::GenerateData()
{
  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer input  = this->GetInput();

  
  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy input to output.  This moves all the non-scalar data untouched to the 
  // output and allows us to sych up the output image region sizes with the
  // scalar update image.  We will operate directly on the output from now on.
  this->CopyInputToOutput();
}

} // end namespace itk
