/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientNDAnisotropicDiffusionEquation.txx
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
#ifndef __itkVectorGradientNDAnisotropicDiffusionEquation_txx_
#define __itkVectorGradientNDAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
double VectorGradientNDAnisotropicDiffusionEquation<TImage>
::m_MIN_NORM = 1.0e-10;
  
template<class TImage>
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::VectorGradientNDAnisotropicDiffusionEquation()
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
  dx_op.SetDirection(0); // Not relelevant, we'll apply in a slice-based
                         // fashion 
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
VectorGradientNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  PixelType delta;

  double GradMag;
  double GradMag_d;
  double Cx[ImageDimension];
  double Cxd[ImageDimension];

  // Remember: PixelType is a Vector of length VectorDimension.
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;

  // Calculate the directional and centralized derivatives.
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]=  it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      dx[i]      = m_InnerProduct(x_slice[i], it, dx_op);
    }

  // Calculate the conductance term for each dimension.
  for (i = 0; i < ImageDimension; i++)
    {
      // Calculate gradient magnitude approximation in this
      // dimension linked (summed) across the vector components.
      GradMag   = 0.0;
      GradMag_d = 0.0;
      for (k =0; k < VectorDimension; k++)
        {
          GradMag   +=  vnl_math_sqr( dx_forward[i][k] );
          GradMag_d +=  vnl_math_sqr( dx_backward[i][k] );

          for (j = 0; j < ImageDimension; j++)
            {
              if ( j != i)
                {
                  dx_aug  = m_InnerProduct(xa_slice[j][i], it, dx_op);
                  dx_dim  = m_InnerProduct(xd_slice[j][i], it, dx_op);
                  GradMag   += 0.25f * vnl_math_sqr( dx[j][k]+dx_aug[k] );
                  GradMag_d += 0.25f * vnl_math_sqr( dx[j][k]+dx_dim[k] );
                }
            }
        }
      
      Cx[i]  = ::exp( GradMag   / m_k );
      Cxd[i] = ::exp( GradMag_d / m_k ); 
    }

  // Compute update value  
  for (k = 0; k < VectorDimension; k++)
    {
      delta[k] = NumericTraits<ScalarValueType>::Zero;
      
      for (i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][k]  *= Cx[i];
          dx_backward[i][k] *= Cxd[i];
          delta[k] += dx_forward[i][k] - dx_backward[i][k];
        }
    }
      
  return delta;
}

template<class TImage>
VectorGradientNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  PixelType delta;
  
  double GradMag;
  double GradMag_d;
  double Cx[ImageDimension];
  double Cxd[ImageDimension];

  // Remember: PixelType is a Vector of length VectorDimension.
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;

  // Calculate the directional and centralized derivatives.
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]=  it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      dx[i]      = m_SmartInnerProduct(x_slice[i], it, dx_op);
    }

  // Calculate the conductance term for each dimension.
  for (i = 0; i < ImageDimension; i++)
    {
      // Calculate gradient magnitude approximation in this
      // dimension linked (summed) across the vector components.
      GradMag   = 0.0;
      GradMag_d = 0.0;
      for (k =0; k < VectorDimension; k++)
        {
          GradMag   +=  vnl_math_sqr( dx_forward[i][k] );
          GradMag_d +=  vnl_math_sqr( dx_backward[i][k] );

          for (j = 0; j < ImageDimension; j++)
            {
              if ( j != i)
                {
                  dx_aug  = m_SmartInnerProduct(xa_slice[j][i], it, dx_op);
                  dx_dim  = m_SmartInnerProduct(xd_slice[j][i], it, dx_op);
                  GradMag   += 0.25f * vnl_math_sqr( dx[j][k]+dx_aug[k] );
                  GradMag_d += 0.25f * vnl_math_sqr( dx[j][k]+dx_dim[k] );
                }
            }
        }
      
      Cx[i]  = ::exp( GradMag   / m_k );
      Cxd[i] = ::exp( GradMag_d / m_k ); 
    }

  // Compute update value  
  for (k = 0; k < VectorDimension; k++)
    {
      delta[k] = NumericTraits<ScalarValueType>::Zero;
      
      for (i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][k]  *= Cx[i];
          dx_backward[i][k] *= Cxd[i];
          delta[k] += dx_forward[i][k] - dx_backward[i][k];
        }
    }
      
  return delta;
}

} // end namespace itk

#endif
