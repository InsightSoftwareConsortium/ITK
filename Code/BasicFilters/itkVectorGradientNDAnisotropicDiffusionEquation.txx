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
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::VectorGradientNDAnisotropicDiffusionEquation()
{
  unsigned int i;
  RadiusType r;
  r[0] = 2;
  r[1] = 1;
  this->SetRadius(r);
  
  // Dummy neighborhood
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);

  // Set up neighborhood slicing
  m_Center = it.Size()/2;
  for (int i = 0; i< ImageDimension; i++)
    {
      m_Stride[i]   = it.GetStride(i);
      x_slice[i]  = std::slice(m_Center - m_Stride[i], 3, m_Stride[i]);
      xa_slice[i] = std::slice((m_Center+1)-m_Stride[i], 3, m_Stride[i]);
      xd_slice[i] = std::slice((m_Center-1)-m_Stride[i], 3, m_Stride[i]);
    }
 
  // Allocate the derivative operator
  dx_op.SetDirection(0);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
  
}

template<class TImage>
VectorGradientNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, TimeStepType &dt) const
{
  unsigned int j;
  PixelType delta;

  ScalarValueType GradMag[ImageDimension];
  ScalarValueType GradMag_d[ImageDimension];  
  ScalarValueType Cx[ImageDimension];
  ScalarValueType Cxd[ImageDimension];
  ScalarValueType Cx_gradmag[ImageDimension];
  ScalarValueType Cxd_gradmag[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];

  Cx_gradmag  = NumericTraits<ScalarValueType>::Zero;
  Cxd_gradmag = NumericTraits<ScalarValueType>::Zero;

  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i]  = it.GetPixel(m_Center - m_Stride[i])
        - (it.GetPixel(m_Center));
      dx_backward[i] = it.GetPixel(m_Center + m_Stride[i])
        - (it.GetPixel(m_Center));

      dx[i]      = m_InnerProduct(x_slice[i]  , it, dx_op);
      dx_aug[i]  = m_InnerProduct(xa_slice[i] , it, dx_op);
      dx_dim[i]  = m_InnerProduct(xd_slice[i] , it, dx_op);
    }

  // Calculate approximate gradient magnitude values
  for (i = 0; i < ImageDimension; i++)
    {
      GradMag[i]   = NumericTraits<ScalarValueType>::Zero;
      GradMag_d[i] = NumericTraits<ScalarValueType>::Zero;

      for (j = 0; j < VectorDimension; j++)
        {
          for (unsigned int m = 0; m < ImageDimension; m++)
            {
              if ( m != i)
                {
                  GradMag[i]   += 0.25f * (dx[m][j]+dx_aug[m][j]) *
                    (dx[m][j] + dx_aug[m][j]);
                  GradMag_d[i] += 0.25f * (dx[m][j]+dx_dim[m][j]) *
                    (dx[m][j] + dx_dim[m][j] );
                }
            }
          GradMag[i]   +=  dx_forward[i][j] *  dx_forward[i][j];
          GradMag_d[i] += dx_backward[i][j] * dx_backward[i][j];
        }
    }

  // Calculate conductance terms
  for (i = 0; i < ImageDimension; ++i)
    {
      Cx[i] = exp( GradMag[i]    / m_k );
      Cxd[i]= exp( GradMag_d[i] / m_k );
    }

  // Compute update value  
  for (unsigned int j = 0; j < ImageDimension; j++)
    {
      delta[j] = NumericTraits<ScalarValueType>::Zero;
      
      for (unsigned int i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][j]  *= Cx[i];
          dx_backward[i][j] *= Cxd[i];
          delta[j] += dx_forward[i][j] + dx_backward[i][j];
        }
    }
      
  return delta;
}

template<class TImage>
VectorGradientNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, TimeStepType &dt) const
{
    unsigned int j;
  PixelType delta;

  ScalarValueType GradMag[ImageDimension];
  ScalarValueType GradMag_d[ImageDimension];  
  ScalarValueType Cx[ImageDimension];
  ScalarValueType Cxd[ImageDimension];
  ScalarValueType Cx_gradmag[ImageDimension];
  ScalarValueType Cxd_gradmag[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];

  Cx_gradmag  = NumericTraits<ScalarValueType>::Zero;
  Cxd_gradmag = NumericTraits<ScalarValueType>::Zero;

  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i]  = it.GetPixel(m_Center - m_Stride[i])
        - (it.GetPixel(m_Center));
      dx_backward[i] = it.GetPixel(m_Center + m_Stride[i])
        - (it.GetPixel(m_Center));

      dx[i]      = m_SmartInnerProduct(x_slice[i]  , it, dx_op);
      dx_aug[i]  = m_SmartInnerProduct(xa_slice[i] , it, dx_op);
      dx_dim[i]  = m_SmartInnerProduct(xd_slice[i] , it, dx_op);
    }

  // Calculate approximate gradient magnitude values
  for (i = 0; i < ImageDimension; i++)
    {
      GradMag[i]   = NumericTraits<ScalarValueType>::Zero;
      GradMag_d[i] = NumericTraits<ScalarValueType>::Zero;

      for (j = 0; j < VectorDimension; j++)
        {
          for (unsigned int m = 0; m < ImageDimension; m++)
            {
              if ( m != i)
                {
                  GradMag[i]   += 0.25f * (dx[m][j]+dx_aug[m][j]) *
                    (dx[m][j] + dx_aug[m][j]);
                  GradMag_d[i] += 0.25f * (dx[m][j]+dx_dim[m][j]) *
                    (dx[m][j] + dx_dim[m][j] );
                }
            }
          GradMag[i]   +=  dx_forward[i][j] *  dx_forward[i][j];
          GradMag_d[i] += dx_backward[i][j] * dx_backward[i][j];
        }
    }

  // Calculate conductance terms
  for (i = 0; i < ImageDimension; ++i)
    {
      Cx[i] = exp( GradMag[i]    / m_k );
      Cxd[i]= exp( GradMag_d[i] / m_k );
    }

  // Compute update value  
  for (unsigned int j = 0; j < ImageDimension; j++)
    {
      delta[j] = NumericTraits<ScalarValueType>::Zero;
      
      for (unsigned int i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][j]  *= Cx[i];
          dx_backward[i][j] *= Cxd[i];
          delta[j] += dx_forward[i][j] + dx_backward[i][j];
        }
    }
      
  return delta;
}

} // end namespace itk

#endif
