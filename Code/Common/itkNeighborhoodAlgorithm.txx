/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkNeighborhoodAlgorithm_txx
#define _itkNeighborhoodAlgorithm_txx
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"
#include "itkConstSliceIterator.h"

namespace itk
{

namespace NeighborhoodAlgorithm {
  
template<class TContainer, class TArray>
typename InnerProduct<TContainer, TArray>::ScalarType
InnerProduct<TContainer, TArray>
::operator()(TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
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
  ScalarType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
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
  TIterator it(o.GetRadius(), in, in->GetRequestedRegion());
  it.SetOutputBuffer(out->GetBufferPointer() +
                     out->ComputeOffset(it.GetRegion().GetIndex()));
  Offset<ImageDimension> mod = OM(in, out);
  it.SetOutputWrapOffsetModifier(mod);
  
  // Process image to output.
  it = it.Begin();
  while( !it.IsAtEnd() )
    {
      *( it.GetOutputBuffer() ) = OP(it, o);
      ++it;
    }
}

template<class TImage>
typename CalculateOutputWrapOffsetModifiers<TImage>::OffsetType
CalculateOutputWrapOffsetModifiers<TImage>
::operator()(TImage *input, TImage *output) const
{
  OffsetType ans;
  const Size<TImage::ImageDimension> isz =  input->GetBufferedRegion().GetSize();
  const Size<TImage::ImageDimension> osz = output->GetBufferedRegion().GetSize();

  for (int i=0; i<TImage::ImageDimension; ++i) ans[i] = osz[i] - isz[i];
  return ans;
}





} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#endif
