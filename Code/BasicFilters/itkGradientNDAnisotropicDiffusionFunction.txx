/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientNDAnisotropicDiffusionFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientNDAnisotropicDiffusionFunction_txx_
#define __itkGradientNDAnisotropicDiffusionFunction_txx_

#include "itkNumericTraits.h"

namespace itk {

template<class TImage>
double GradientNDAnisotropicDiffusionFunction<TImage>
::m_MIN_NORM = 1.0e-10;
  
template<class TImage>
GradientNDAnisotropicDiffusionFunction<TImage>
::GradientNDAnisotropicDiffusionFunction()
{
  unsigned int i, j;
  RadiusType r;

  for (i = 0; i < ImageDimension; ++i)
    {
      r[i] = 1;
    }
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);
  
 // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for (i = 0; i< ImageDimension; ++i)
    { m_Stride[i] = it.GetStride(i); }

  for (i = 0; i< ImageDimension; ++i)
    { x_slice[i]  = std::slice( m_Center - m_Stride[i], 3, m_Stride[i]); }
  
  for (i = 0; i< ImageDimension; ++i)
    {
      for (j = 0; j < ImageDimension; ++j)
        {
          // For taking derivatives in the i direction that are offset one
          // pixel in the j direction.
          xa_slice[i][j]
            = std::slice((m_Center + m_Stride[j])-m_Stride[i], 3, m_Stride[i]); 
          xd_slice[i][j]
            = std::slice((m_Center - m_Stride[j])-m_Stride[i], 3, m_Stride[i]);
        }
    }

  // Allocate the derivative operator.
  dx_op.SetDirection(0);  // Not relevant, will be applied in a slice-based
                          // fashion.
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
typename GradientNDAnisotropicDiffusionFunction<TImage>::PixelType
GradientNDAnisotropicDiffusionFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void *,
                const FloatOffsetType&) const
{
  unsigned int i, j;

  double accum;
  double accum_d;
  double Cx;
  double Cxd;
  
  // PixelType is scalar in this context
  typedef typename NumericTraits<PixelType>::RealType PixelRealType;  
  PixelRealType delta;
  PixelRealType dx_forward;
  PixelRealType dx_backward;
  PixelRealType dx[ImageDimension];
  PixelRealType dx_aug;
  PixelRealType dx_dim;

  delta = NumericTraits<PixelRealType>::Zero;
  
  // Calculate the centralized derivatives for each dimension.
  for (i = 0; i < ImageDimension; i++)
    {      dx[i]  = m_InnerProduct(x_slice[i], it, dx_op);    }

  for (i = 0; i < ImageDimension; i++)
    {
      // ``Half'' directional derivatives
      dx_forward = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward =  it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);      

      // Calculate the conductance terms.  Conductance varies with each
      // dimension because the gradient magnitude approximation is different
      // along each  dimension.      
      accum   = 0.0;
      accum_d = 0.0;
      for (j = 0; j < ImageDimension; j++)
        {
          if (j != i)
            {
              dx_aug     = m_InnerProduct(xa_slice[j][i], it, dx_op);
              dx_dim     = m_InnerProduct(xd_slice[j][i], it, dx_op);
              accum   += 0.25f * vnl_math_sqr( dx[j] + dx_aug );
              accum_d += 0.25f * vnl_math_sqr( dx[j] + dx_dim );
            }
        }
      
      if (m_K == 0.0)
        {       
        Cx = 0.0;
        Cxd = 0.0;
        }
      else
        {
        Cx = exp(( vnl_math_sqr( dx_forward ) + accum)  / m_K );
        Cxd= exp(( vnl_math_sqr( dx_backward) + accum_d)/ m_K );
        }

      // Conductance modified first order derivatives.
      dx_forward  = dx_forward * Cx;
      dx_backward = dx_backward * Cxd;

      // Conductance modified second order derivative.
      delta += dx_forward - dx_backward;
    }
  
  return static_cast<PixelType>(delta);
}

} // end namespace itk

#endif
