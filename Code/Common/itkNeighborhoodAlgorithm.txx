/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkNeighborhoodAlgorithm_txx
#define _itkNeighborhoodAlgorithm_txx
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{
  
template<class TContainer, class TArray>
typename InnerProduct<TContainer, TArray>::ScalarType
InnerProduct<TContainer, TArray>
::operator()(TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::Iterator this_it;
  const InternalType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * *this_it;
    }
  return sum;
}

template<class TContainer, class TArray>
typename InnerProduct<TContainer, TArray>::ScalarType
InnerProduct<TContainer, TArray>
::operator()(std::slice &s, TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const InternalType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * *slice_it;
    }

  return sum;
}

template<class TContainer, class TArray>
typename VectorComponentInnerProduct<TContainer, TArray>::ScalarType
VectorComponentInnerProduct<TContainer, TArray>
::operator()(TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {     
      sum += *it * (*this_it)[m_VisibleComponent];
    }
  return sum;
}


template<class TContainer, class TArray>
typename VectorComponentInnerProduct<TContainer, TArray>::ScalarType
VectorComponentInnerProduct<TContainer, TArray>
::operator()(std::slice &s, TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (*slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator, class TArray>
typename IteratorInnerProduct<TIterator, TArray>::ScalarType
IteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * **this_it;
    }
  return sum;
}

template<class TIterator, class TArray>
typename IteratorInnerProduct<TIterator, TArray>::ScalarType
IteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * **slice_it;
    }

  return sum;
}

template<class TIterator, class TArray>
typename BoundsCheckingIteratorInnerProduct<TIterator, TArray>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension>, TArray > IP;
  
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      ScalarType *it;
      typename TIterator::Iterator this_it;
      const ScalarType *_end = &(v[v.Size()]);
      
      for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
        {
          sum += *it * **this_it;
        }
      return sum;
    }
  else
    {
      Neighborhood<PixelType, Dimension> N = d.GetNeighborhood();
      return IP(N, v);;
    }
}

template<class TIterator, class TArray>
typename BoundsCheckingIteratorInnerProduct<TIterator, TArray>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension>, TArray > IP;
  ScalarType sum;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  
  if ( d.InBounds() )
    {
      sum = NumericTraits<ScalarType>::Zero;
        
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.Size()]);
      for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
        {
          sum += *it * **slice_it;
        }
      
      return sum;
    }
  else
    {
      Neighborhood<PixelType, Dimension> N = d.GetNeighborhood();
      return IP(s, N, v);
    }
}

template<class TIterator, class TArray>
typename VectorComponentIteratorInnerProduct<TIterator, TArray>::ScalarType
VectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
     
      sum += *it * (**this_it)[m_VisibleComponent];
    }
  return sum;
}

template<class TIterator, class TArray>
typename VectorComponentIteratorInnerProduct<TIterator, TArray>::ScalarType
VectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (**slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator, class TArray>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension>, TArray > IP;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  if ( d.InBounds() )
    { 
      ScalarType sum = NumericTraits<ScalarType>::Zero; 
      const ScalarType *_end = &(v[v.Size()]);
      
      for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
        {
          
          sum += *it * (**this_it)[m_VisibleComponent];
        }
      return sum;
    }
  else
    {
      Neighborhood<VectorType, Dimension> N = d.GetNeighborhood();
      return IP(N, v);
    }
}

template<class TIterator, class TArray>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension>, TArray > IP;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.Size()]);
      for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
        {
          sum += *it * (**slice_it)[m_VisibleComponent];
        }
      return sum;
    }
  else
    {
      Neighborhood<VectorType, Dimension> N = d.GetNeighborhood();
      return IP(s, N, v);
    }
}


template<class TOperation, class TIterator>
void ApplyOperatorToEach<TOperation, TIterator>
::operator()(ImageType *in, ImageType *out,
             Neighborhood<ScalarValueType, ImageDimension> &o)
  const
{
  TOperation OP;
  CalculateOutputWrapOffsetModifiers<ImageType> OM;

  // Create iterator.  The output buffer pointer of the iterator is set up
  // to account for any differences in the buffer sizes of the two images.
  long int *mod = new long int[ImageDimension];
  TIterator it(o.GetRadius(), in, in->GetRequestedRegion());
  it.SetOutputBuffer(out->GetBufferPointer() +
                     out->ComputeOffset(it.GetRegion().GetIndex()));
  OM(mod, in, out);
  it.SetOutputWrapOffsetModifier(mod);
  delete[] mod;
  
  // Process image to output.
  it = it.Begin();
  while( !it.IsAtEnd() )
    {
      *( it.GetOutputBuffer() ) = OP(it, o);
      ++it;
    }
}

template<class TImage>
void CalculateOutputWrapOffsetModifiers<TImage>
::operator()(long int *ans, TImage *input, TImage *output) const
{
  const Size<TImage::ImageDimension> isz =  input->GetBufferedRegion().GetSize();
  const Size<TImage::ImageDimension> osz = output->GetBufferedRegion().GetSize();

  for (int i=0; i<TImage::ImageDimension; ++i) ans[i] = osz[i] - isz[i];
}





} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#endif
