/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkAnisotropicDiffusionImageFilter_txx
#define _itkAnisotropicDiffusionImageFilter_txx

#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
namespace itk
{

template<class TInputImage, class TOutputImage>
void UpdateStrategyScalar<TInputImage, TOutputImage>
::operator()(void *d1, void *d2) const 
{
  TInputImage *ip = static_cast<TInputImage *>(d1);
  TOutputImage *op = static_cast<TOutputImage *>(d2);
  SimpleImageRegionIterator<TInputImage> in(ip, op->GetRequestedRegion());
  SimpleImageRegionIterator<TOutputImage> out(op, op->GetRequestedRegion());
  in.Begin();
  out.Begin();

  while (! in.IsAtEnd() )
    { // *out += *in * m_Multiplier
      out.Set( out.Get() + ( in.Get() * m_Multiplier ) );
      ++out;
      ++in;
    }
}

template <class TInputImage, class TOutputImage>
void CopyStrategyScalar<TInputImage, TOutputImage>
::operator()(void *d1, void *d2) const
{
  TInputImage *ip  = static_cast<TInputImage *>(d1);
  TOutputImage *op = static_cast<TOutputImage *>(d2);
  SimpleImageRegionIterator<TInputImage>  in(ip, op->GetRequestedRegion());
  SimpleImageRegionIterator<TOutputImage> out(op, op->GetRequestedRegion());
  in.Begin();
  out.Begin();

  while (! in.IsAtEnd() )
    {
      // *out = *in;
      out.Set( in.Get() );
      ++out;
      ++in;
    }
}
   
template<class TInputImage, class TOutputImage>
void
AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typedef RegionNonBoundaryNeighborhoodIterator<TOutputImage> RNI;
  typedef RegionBoundaryNeighborhoodIterator<TOutputImage>    RBI;

  DiffusionStrategy *a = this->GetDiffusionStrategy();
  UpdateStrategy    *u = this->GetUpdateStrategy();
  CopyStrategy      *c = this->GetCopyStrategy();

  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::Pointer  input  = this->GetInput();
  
  // Allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy input to output.
  c->operator()(input, output);

  // Temp image
  typename TOutputImage::Pointer delta = TOutputImage::New();
  delta->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  delta->SetRequestedRegion(output->GetRequestedRegion());
  delta->SetBufferedRegion(output->GetBufferedRegion());
  delta->Allocate();

  // Iterate
  u->m_Multiplier = this->GetTimeStep();
  for (unsigned int i=0; i< this->GetIterations(); ++i)
    {
      a->operator()(output, delta);
      u->operator()(delta, output);
    }
  
  delete a;
  delete u;
  delete c;
  //delta->Delete();
}

template<class TImageType>
AvgGradMagSquared<TImageType>::PixelType
AvgGradMagSquared<TImageType>
::operator()(TImageType *ip,
             const RegionType &region)
  const
{
  PixelType accumulator;
  PixelType val;
  PixelType counter;
  typedef RegionNonBoundaryNeighborhoodIterator<TImageType> RNI_type;
  NeighborhoodAlgorithm::IteratorInnerProduct<RNI_type,
    NeighborhoodOperator<PixelType, ImageDimension> > IP;
  
  RNI_type iterator_list[ImageDimension];
  DerivativeOperator<PixelType, ImageDimension> operator_list[ImageDimension];
  
  // Set up the derivative operators and their iterators.
  // Instead of maintaining a single N-d neighborhood of pointers,
  // we maintain a list of 1-d neighborhoods along each axial direction.
  // This is more efficient for higher dimensions.
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      iterator_list[i]=RNI_type(operator_list[i].GetRadius(), ip,
                                ip->GetRequestedRegion()); 
      iterator_list[i] = iterator_list[i].Begin();
    }

  // Now do the actual processing
  accumulator = NumericTraits<PixelType>::Zero;
  counter     = NumericTraits<PixelType>::Zero;
  const RNI_type iterator_end = iterator_list[0].End();
  for (iterator_list[0] = iterator_list[0].Begin();
       !iterator_list[0].IsAtEnd(); )
    {
      counter += NumericTraits<PixelType>::One;
      for (unsigned int i = 0; i < ImageDimension; ++i)
        {
          val = IP(iterator_list[i], operator_list[i]);     
          accumulator += val * val;
          ++iterator_list[i];
        }
    }

  return ( (PixelType) (accumulator / counter) );
}

} // end namespace itk

#endif
