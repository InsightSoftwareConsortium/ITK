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
#include "itkImageTraits.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
namespace itk
{

template<class TImage>
void UpdateStrategyScalar<TImage>
::operator()(void *d1, void *d2) const 
{
  TImage *ip = static_cast<TImage *>(d1);
  TImage *op = static_cast<TImage *>(d2);
  ImageRegionIterator<ImageTraits<TImage>::ScalarValueType,
    ImageTraits<TImage>::ImageDimension> in(ip, op->GetRequestedRegion());
  ImageRegionIterator<ImageTraits<TImage>::ScalarValueType,
    ImageTraits<TImage>::ImageDimension>  out(op, op->GetRequestedRegion());
  in = in.Begin();
  out = out.Begin();

  while (! in.IsAtEnd() )
    {
      *out += *in * m_Multiplier;
      ++out;
      ++in;
    }
}

template <class TImage>
void CopyStrategyScalar<TImage>::operator()(void *d1, void *d2) const
{
  TImage *ip = static_cast<TImage *>(d1);
  TImage *op = static_cast<TImage *>(d2);
  ImageRegionIterator<ImageTraits<TImage>::ScalarValueType,
    ImageTraits<TImage>::ImageDimension>  in(ip, op->GetRequestedRegion());
  ImageRegionIterator<ImageTraits<TImage>::ScalarValueType,
    ImageTraits<TImage>::ImageDimension>  out(op, op->GetRequestedRegion());
  in = in.Begin();
  out = out.Begin();

  while (! in.IsAtEnd() )
    {
      *out = *in;
      ++out;
      ++in;
    }
}
   
template<class TPixel, unsigned int VDimension>
void
AnisotropicDiffusionImageFilter<TPixel, VDimension>
::GenerateData()
{
  typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI;
  typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension>    RBI;

  DiffusionStrategy *a = this->GetDiffusionStrategy();
  UpdateStrategy    *u = this->GetUpdateStrategy();
  CopyStrategy      *c = this->GetCopyStrategy();

  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer input  = this->GetInput();
  
  // Allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy input to output.
  c->operator()(input, output);

  // Temp image
  ImageType::Pointer delta = ImageType::New();
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

template<class TPixel, unsigned long VDimension>
TPixel AvgGradMagSquared<TPixel, VDimension>
::operator()(Image<TPixel, VDimension> *ip,
             const ImageRegion<VDimension> &region)
  const
{
  TPixel accumulator;
  TPixel val;
  unsigned long counter;
  typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI_type;
  NeighborhoodAlgorithm::IteratorInnerProduct<RNI_type,
    NeighborhoodOperator<TPixel, VDimension> > IP;
  
  RNI_type iterator_list[VDimension];
  DerivativeOperator<TPixel, VDimension> operator_list[VDimension];
  
  // Set up the derivative operators and their iterators
  for (unsigned int i = 0; i < VDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      iterator_list[i]=RNI_type(operator_list[i].GetRadius(), ip,
                                ip->GetRequestedRegion()); 
      iterator_list[i] = iterator_list[i].Begin();
    }

  // Now do the actual processing
  accumulator = NumericTraits<TPixel>::Zero;
  counter     = 0;
  const RNI_type iterator_end = iterator_list[0].End();
  for (iterator_list[0] = iterator_list[0].Begin();
       !iterator_list[0].IsAtEnd(); ++counter)
    {
      for (unsigned int i = 0; i < VDimension; ++i)
        {
          val = IP(iterator_list[i], operator_list[i]);     
          accumulator += val * val;
          ++iterator_list[i];
        }
    }

  return ( (TPixel) (accumulator / ((TPixel)counter)) );
}

  


} // end namespace itk
