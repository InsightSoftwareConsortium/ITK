/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkGradientNDAnisotropicDiffusionEquation_txx_
#define __itkGradientNDAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
GradientNDAnisotropicDiffusionEquation<TImage>
::GradientNDAnisotropicDiffusionEquation()
{
  unsigned int i;
  RadiusType r;
  r[0] = 2;
  r[1] = 1;
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);
  
 // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for (i = 0; i< ImageDimension; ++i)
    {
      m_Stride[i]   = it.GetStride(i);
      x_slice[i]  = std::slice( m_Center - m_Stride[i],  3, m_Stride[i]);
      xa_slice[i] = std::slice((m_Center+1)-m_Stride[i], 3, m_Stride[i]);
      xd_slice[i] = std::slice((m_Center-1)-m_Stride[i], 3, m_Stride[i]);
    }

  // Allocate the derivative operator.
  dx_op.SetDirection(0);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
GradientNDAnisotropicDiffusionEquation<TImage>::PixelType
GradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, TimeStepType &dt) const
{
  unsigned int i, j;
  PixelType accum, accum_d, delta;
  PixelType Cx[ImageDimension];
  PixelType Cxd[ImageDimension];
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];

  delta = NumericTraits<PixelType>::Zero;
  
  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center - m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);

      dx[i]         = m_InnerProduct(x_slice[i], it, dx_op);
      dx_aug[i]     = m_InnerProduct(xa_slice[i],it, dx_op);
      dx_dim[i]     = m_InnerProduct(xd_slice[i],it, dx_op);
    }

  // Calculate the conductance terms
  for (i = 0; i < ImageDimension; i++)
    {
      accum   = NumericTraits<PixelType>::Zero;
      accum_d = NumericTraits<PixelType>::Zero;
      for (j = 0; j < ImageDimension; j++)
        {
          if (j != i)
            {
              accum   += 0.25f * (dx[j]+dx_aug[j]) * (dx[j]+dx_aug[j]);
              accum_d += 0.25f * (dx[j]+dx_dim[j]) * (dx[j]+dx_dim[j]);
            }
        }
      Cx[i] = exp(( dx_forward[i] * dx_forward[i]  + accum)  / m_k);
      Cxd[i]= exp((dx_backward[i] * dx_backward[i] + accum_d)/ m_k);
      dx_forward[i]  *= Cx[i];
      dx_backward[i] *= Cxd[i];
      delta += dx_forward[i] + dx_backward[i];
    }
  
  return ( delta );
}

template<class TImage>
GradientNDAnisotropicDiffusionEquation<TImage>::PixelType
GradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, TimeStepType &dt) const
{
 unsigned int i, j;
  PixelType accum, accum_d, delta;
  PixelType Cx[ImageDimension];
  PixelType Cxd[ImageDimension];
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];

  delta = NumericTraits<PixelType>::Zero;
  
  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center - m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);

      dx[i]         = m_SmartInnerProduct(x_slice[i], it, dx_op);
      dx_aug[i]     = m_SmartInnerProduct(xa_slice[i],it, dx_op);
      dx_dim[i]     = m_SmartInnerProduct(xd_slice[i],it, dx_op);
    }

  // Calculate the conductance terms
  for (i = 0; i < ImageDimension; i++)
    {
      accum   = NumericTraits<PixelType>::Zero;
      accum_d = NumericTraits<PixelType>::Zero;
      for (j = 0; j < ImageDimension; j++)
        {
          if (j != i)
            {
              accum   += 0.25f * (dx[j]+dx_aug[j]) * (dx[j]+dx_aug[j]);
              accum_d += 0.25f * (dx[j]+dx_dim[j]) * (dx[j]+dx_dim[j]);
            }
        }
      Cx[i] = exp(( dx_forward[i] * dx_forward[i]  + accum)  / m_k);
      Cxd[i]= exp((dx_backward[i] * dx_backward[i] + accum_d)/ m_k);
      dx_forward[i]  *= Cx[i];
      dx_backward[i] *= Cxd[i];
      delta += dx_forward[i] + dx_backward[i];
    }
  
  return ( delta );
}

} // end namespace itk

#endif
