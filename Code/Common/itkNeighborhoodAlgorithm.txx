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
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{
  
template<class TContainer>
typename InnerProduct<TContainer>::ScalarType
InnerProduct<TContainer>
::operator()(TContainer &d, std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::Iterator this_it;
  const InternalType *_end = &(v[v.size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * *this_it;
    }
  return sum;
}

template<class TContainer>
typename InnerProduct<TContainer>::ScalarType
InnerProduct<TContainer>
::operator()(std::slice &s, TContainer &d,
             std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const InternalType *itEnd = &(v[v.size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * *slice_it;
    }

  return sum;
}

template<class TContainer>
typename VectorComponentInnerProduct<TContainer>::ScalarType
VectorComponentInnerProduct<TContainer>
::operator()(TContainer &d, std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::Iterator this_it;
  const ScalarType *_end = &(v[v.size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {     
      sum += *it * (*this_it)[m_VisibleComponent];
    }
  return sum;
}


template<class TContainer>
typename VectorComponentInnerProduct<TContainer>::ScalarType
VectorComponentInnerProduct<TContainer>
::operator()(std::slice &s, TContainer &d,
             std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (*slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator>
typename IteratorInnerProduct<TIterator>::ScalarType
IteratorInnerProduct<TIterator>
::operator()(TIterator &d, std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * **this_it;
    }
  return sum;
}

template<class TIterator>
typename IteratorInnerProduct<TIterator>::ScalarType
IteratorInnerProduct<TIterator>
::operator()(std::slice &s, TIterator &d,
             std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * **slice_it;
    }

  return sum;
}

template<class TIterator>
typename BoundsCheckingIteratorInnerProduct<TIterator>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator>
::operator()(TIterator &d, std::valarray<ScalarType> &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension> > IP;
  
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      ScalarType *it;
      typename TIterator::Iterator this_it;
      const ScalarType *_end = &(v[v.size()]);
      
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

template<class TIterator>
typename BoundsCheckingIteratorInnerProduct<TIterator>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator>
::operator()(std::slice &s, TIterator &d,
             std::valarray<ScalarType> &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension> > IP;
  ScalarType sum;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  
  if ( d.InBounds() )
    {
      sum = NumericTraits<ScalarType>::Zero;
        
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.size()]);
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

template<class TIterator>
typename VectorComponentIteratorInnerProduct<TIterator>::ScalarType
VectorComponentIteratorInnerProduct<TIterator>
::operator()(TIterator &d, std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
     
      sum += *it * (**this_it)[m_VisibleComponent];
    }
  return sum;
}

template<class TIterator>
typename VectorComponentIteratorInnerProduct<TIterator>::ScalarType
VectorComponentIteratorInnerProduct<TIterator>
::operator()(std::slice &s, TIterator &d,
             std::valarray<ScalarType> &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (**slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator>
::operator()(TIterator &d, std::valarray<ScalarType> &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension> > IP;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  if ( d.InBounds() )
    { 
      ScalarType sum = NumericTraits<ScalarType>::Zero; 
      const ScalarType *_end = &(v[v.size()]);
      
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

template<class TIterator>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator>
::operator()(std::slice &s, TIterator &d,
             std::valarray<ScalarType> &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension> > IP;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.size()]);
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
