/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodInnerProduct.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorNeighborhoodInnerProduct_txx
#define _itkVectorNeighborhoodInnerProduct_txx
#include "itkVectorNeighborhoodInnerProduct.h"


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
