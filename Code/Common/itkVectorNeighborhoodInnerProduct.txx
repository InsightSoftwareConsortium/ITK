/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodInnerProduct.txx
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
#ifndef __itkVectorNeighborhoodInnerProduct_txx
#define __itkVectorNeighborhoodInnerProduct_txx

namespace itk {

template<class TImage>
typename VectorNeighborhoodInnerProduct<TImage>::PixelType
VectorNeighborhoodInnerProduct<TImage>
::operator()(const ConstNeighborhoodIterator<TImage> &n,
             const Neighborhood<ScalarValueType, ImageDimension> &op) const
{
  typedef ConstNeighborhoodIterator<TImage> Neighborhood_t;
  typedef Neighborhood<ScalarValueType, ImageDimension> Operator_t;
  PixelType sum;
  unsigned int i;
  
  for (i = 0; i < VectorDimension; ++i)
    { sum[i] = NumericTraits<ScalarValueType>::Zero; }
  
  typename Neighborhood_t::ConstIterator n_it;
  typename Operator_t::ConstIterator o_it;
  
  n_it = n.Begin();
  const typename Operator_t::ConstIterator op_end = op.End();
  for (o_it = op.Begin(); o_it < op_end; ++o_it, ++n_it)
    {
      for (i = 0; i < VectorDimension; ++i)
        {  sum[i] += *o_it * (**n_it)[i];  }
    }
  return sum;
}

template<class TImage>
typename VectorNeighborhoodInnerProduct<TImage>::PixelType
VectorNeighborhoodInnerProduct<TImage>
::operator()(const std::slice &s,
             const ConstNeighborhoodIterator<TImage> &n,
             const Neighborhood<ScalarValueType, ImageDimension> &op) const
{
  typedef ConstNeighborhoodIterator<TImage> Neighborhood_t;
  typedef Neighborhood<ScalarValueType, ImageDimension> Operator_t;
  PixelType sum;
  unsigned int i;

  for (i= 0; i < VectorDimension; ++i)
    { sum[i] = NumericTraits<ScalarValueType>::Zero; }
  
  ConstSliceIterator<PixelType *, Neighborhood_t> s_it(&n, s);
  typename Operator_t::ConstIterator o_it;

  s_it = s_it.Begin();
  const typename Operator_t::ConstIterator op_end = op.End();
  for (o_it = op.Begin(); o_it < op_end; ++o_it, ++s_it)
    {
      for (i = 0; i < VectorDimension; ++i)
        {  sum[i] += *o_it * (**s_it)[i]; }
    }
  
  return sum;
}

template<class TImage>
typename SmartVectorNeighborhoodInnerProduct<TImage>::PixelType
SmartVectorNeighborhoodInnerProduct<TImage>
::operator()(const std::slice &s,
             /*           const ImageBoundaryCondition<TImage> *,*/
             const ConstSmartNeighborhoodIterator<TImage> &it,
             const Neighborhood<ScalarValueType, ImageDimension>
             &op) const
{
  typedef ConstSmartNeighborhoodIterator<TImage> Neighborhood_t;
  typedef Neighborhood<ScalarValueType, ImageDimension> Operator_t;

  PixelType sum;
  unsigned int j;
  
  typename Operator_t::ConstIterator o_it;

  for (j = 0; j < VectorDimension; ++j)
    { sum[j] = NumericTraits<ScalarValueType>::Zero; }
  
  o_it = op.Begin();
  const typename Operator_t::ConstIterator op_end = op.End();

  for ( unsigned int i = s.start(); o_it < op_end; i+=s.stride(), ++o_it )
    {
      for (j = 0; j< VectorDimension; ++j)
        {  sum[j] += *o_it * (it.GetPixel(i))[j]; }
    }
  
  return sum;
} 

}// end namespace itk
#endif
