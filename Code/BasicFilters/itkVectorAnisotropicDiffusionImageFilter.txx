/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionImageFilter.txx
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
#ifndef _itkVectorAnisotropicDiffusionImageFilter_txx
#define _itkVectorAnisotropicDiffusionImageFilter_txx

#include "itkDerivativeOperator.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{

template<class TImage>
float AvgGradMagSquaredVector<TImage>
::operator()(TImage *ip, const ImageRegion<ImageDimension> &ir ) const 
{
  typedef RegionNonBoundaryNeighborhoodIterator<TImage> RNI_type;
  
  VectorValueType accumulator, val, counter;
  float ans;
  unsigned int i, k;
  NeighborhoodAlgorithm::VectorComponentIteratorInnerProduct<RNI_type,
    NeighborhoodOperator<VectorValueType, ImageDimension> > IP;
  RNI_type it[ImageDimension];
  DerivativeOperator<VectorValueType, ImageDimension> op[ImageDimension];

  // set up the derivative operators and their iterators
  for ( i = 0; i < ImageDimension; ++i)
    {
      op[i].SetOrder(1);
      op[i].SetDirection(i);
      op[i].CreateDirectional();
      it[i] = RNI_type(op[i].GetRadius(), ip, ir);
      it[i] = it[i].Begin();
    }

  // process
  ans = 0.0f;
  for ( k = 0; k < VectorDimension; ++k )
    {
      IP.SetVisibleComponent(k);
      accumulator = NumericTraits<VectorValueType>::Zero;
      counter     = NumericTraits<VectorValueType>::Zero;
      for ( i = 0; i < ImageDimension; ++i ) it[i] = it[i].Begin();

      for ( ; !it[0].IsAtEnd(); )
        {
          counter += NumericTraits<VectorValueType>::One;
          for (i = 0; i < ImageDimension; ++i)
            {
              val = IP(it[i], op[i]);
              accumulator += val * val;
              ++it[i];
            }
        }
      
      ans += ((float)(accumulator / counter) );
    }
  
  // return the sum of the average values of the components
  return ans;
};
  
template<class TInputImage, class TOutputImage>
void UpdateStrategyVector<TInputImage, TOutputImage>
::operator() (void *d1, void *d2) const
{
  typedef typename TOutputImage::PixelType PixelType;
  enum { VectorDimension = PixelType::VectorDimension };
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  TInputImage *ip = static_cast<TInputImage *>(d1);
  TOutputImage *op = static_cast<TOutputImage *>(d2);
  ImageRegionIterator<TInputImage>
    in(ip,op->GetRequestedRegion());
  ImageRegionIterator<TOutputImage>
    out(op, op->GetRequestedRegion());

  in.Begin();
  out.Begin();
  // Update each component of the output
  PixelType otmp;
  while (! in.IsAtEnd() )
    {
    otmp = out.Get();
    for (unsigned int i = 0; i < VectorDimension; ++i)
      {
       otmp[i] += in.Get()[i] * m_Multiplier;
      }
    out.Set( otmp );
    ++out;
    ++in;
    }
  
}

template<class TInputImage, class TOutputImage>
void CopyStrategyVector<TInputImage, TOutputImage>
::operator() (void *d1, void *d2) const
{
  typedef typename TOutputImage::PixelType PixelType;
  enum { VectorDimension = PixelType::VectorDimension };
  enum { ImageDimension = TOutputImage::ImageDimension };
    
  TInputImage *ip = static_cast<TInputImage *>(d1);
  TOutputImage *op = static_cast<TOutputImage *>(d2);
  ImageRegionIterator<TInputImage>
    in(ip,op->GetRequestedRegion()); 
  ImageRegionIterator<TOutputImage>
    out(op, op->GetRequestedRegion());
  in = in.Begin();
  out = out.Begin();
  
  // Update each component of the output
  PixelType otmp;
  while (! in.IsAtEnd() )
    {
    otmp = out.Get();
    for (unsigned int i = 0; i < VectorDimension; ++i)
      {
      otmp[i] = in.Get()[i];
      }
    out.Set( otmp );
    ++out;
    ++in;
    }
}
  
template<class TInnerProduct,  class TIterator >
void AnisoDiffuseVector2D<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  enum { X=0, Y=1 };
  const unsigned int N = VectorDimension;
  unsigned int j;

  typename ImageType::Pointer input = static_cast<ImageType*>(d1);
  typename ImageType::Pointer output= static_cast<ImageType*>(d2);
  ZeroFluxNeumannBoundaryCondition<ImageType> nbc;
  TInnerProduct IP;
  AvgGradMagSquaredVector<ImageType> GradMag;

  // modified conductance term
  const float k = GradMag(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;

  // set up the iterator
  Size<ImageDimension> hR;
  hR[0] = 2;
  hR[1] = 1;
  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));


  it.OverrideBoundaryCondition(&nbc);  // Make sure we use bounds conditions
                                       // compatible with this solver
  
  // set up operators and variable terms
  VectorValueType Cx, Cy, Cxd, Cyd; 
  VectorValueType Cx_gradmag, Cy_gradmag, Cxd_gradmag, Cyd_gradmag; 
  VectorValueType dx_forward[N], dx_backward[N], dy_forward[N], 
    dy_backward[N]; 
  VectorValueType dy[N], dx[N], dy_aug[N], dy_dim[N], dx_aug[N], 
    dx_dim[N];  

  DerivativeOperator<VectorValueType, 2> dx_op;
  dx_op.SetDirection(X);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
  
  DerivativeOperator<VectorValueType, 2> dy_op;
  dy_op.SetDirection(Y);
  dy_op.SetOrder(1);
  dy_op.CreateDirectional();
  
  // Slice the neighborhood
  // 0  1  2  3  4
  // 5  6 *7* 8  9
  // 10 11 12 13 14
  std::slice  x_slice(6, 3, 1);
  std::slice  y_slice(2, 3, 5);
  std::slice xa_slice(7, 3, 1);
  std::slice ya_slice(3, 3, 5);
  std::slice xd_slice(5, 3, 1);
  std::slice yd_slice(1, 3, 5);
  
  // process the image
  for (it = it.Begin(); !it.IsAtEnd(); ++it)
    {
      Cx_gradmag  = NumericTraits<VectorValueType>::Zero;
      Cy_gradmag  = NumericTraits<VectorValueType>::Zero;
      Cxd_gradmag = NumericTraits<VectorValueType>::Zero;
      Cyd_gradmag = NumericTraits<VectorValueType>::Zero;
      
      for (j = 0; j<N; ++j)  // take the derivatives & approximate
        {                        // gradient magnitudes
          IP.SetVisibleComponent(j);
          dx_forward[j]  = (it.GetPixel(6))[j] - (it.GetPixel(7))[j]; 
          dx_backward[j] = (it.GetPixel(8))[j] - (it.GetPixel(7))[j];
          dy_forward[j]  = (it.GetPixel(12))[j]- (it.GetPixel(7))[j];
          dy_backward[j] = (it.GetPixel(2))[j] - (it.GetPixel(7))[j];
          dx[j]         = IP(x_slice, it, dx_op);
          dy[j]         = IP(y_slice, it, dy_op);
          dx_aug[j]     = IP(xa_slice,it, dx_op);
          dy_aug[j]     = IP(ya_slice,it, dy_op);
          dx_dim[j]     = IP(xd_slice,it, dx_op);
          dy_dim[j]     = IP(yd_slice,it, dy_op);

          Cx_gradmag += (dx_forward[j]*dx_forward[j] +
                         0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]));
          Cy_gradmag += (dy_forward[j]*dy_forward[j] +
                         0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j])); 
          Cxd_gradmag +=(dx_backward[j]*dx_backward[j] +
                         0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]));
          Cyd_gradmag += (dy_backward[j]*dy_backward[j] +
                          0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]));
        }

      // calculate conductance terms
      Cx = exp( Cx_gradmag / k );
      Cy = exp( Cy_gradmag / k );
      Cxd= exp( Cxd_gradmag / k );
      Cyd= exp( Cyd_gradmag / k );

      for (j = 0; j<N; ++j)  // compute diffusion updates
        {
          dx_forward[j]  *= Cx;
          dy_forward[j]  *= Cy;
          dx_backward[j] *= Cxd;
          dy_backward[j] *= Cyd;
          (*(it.GetOutputBuffer()))[j] = dx_forward[j] + dy_forward[j]
            + dx_backward[j] + dy_backward[j];
        }
    }
}

template<class TInnerProduct,  class TIterator >
void AnisoDiffuseVectorND<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  const unsigned int  N = VectorDimension;
  const unsigned long D = ImageDimension;
  
  typename ImageType::Pointer input = static_cast<ImageType*>(d1);
  typename ImageType::Pointer output= static_cast<ImageType*>(d2);
  ZeroFluxNeumannBoundaryCondition<ImageType> nbc;
  TInnerProduct IP;
  AvgGradMagSquaredVector<ImageType> GradMagFunction;

  const float k = GradMagFunction(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;
  
  // set up the iterator
  Size<ImageDimension> hR;
  hR[0] = 2;
  for (unsigned int i = 1; i < ImageDimension; ++i) hR[i] = 1; 

  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));

 it.OverrideBoundaryCondition(&nbc);  // Make sure we use bounds conditions
                                      // compatible with this solver
  
  VectorValueType GradMag[D], GradMag_d[D], delta[N];

  VectorValueType Cx[D];
  VectorValueType Cxd[D];
  VectorValueType dx_forward[D][N];
  VectorValueType dx_backward[D][N];
  VectorValueType dx[D][N];
  VectorValueType dx_aug[D][N];
  VectorValueType dx_dim[D][N]; 

  DerivativeOperator<VectorValueType, 2> dx_op;
   dx_op.SetDirection(0);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  // slice the neighborhood
   unsigned long stride[D];
   const unsigned long center =  it.Size() / 2;
   std::slice x_slice[D];
   std::slice xa_slice[D];
   std::slice xd_slice[D];
   for (unsigned int i = 0; i< D; ++i)
     {
       stride[i]   = it.GetStride(i);
       x_slice[i]  = std::slice(center - stride[i], 3, stride[i]);
       xa_slice[i] = std::slice((center+1)-stride[i], 3, stride[i]);
       xd_slice[i] = std::slice((center-1)-stride[i], 3, stride[i]);
     }
   

 // process the image
  for (it = it.Begin(); !it.IsAtEnd(); ++it)
    {
      for (unsigned int i = 0; i < D; ++i) // calculate all derivatives
        {
          for (unsigned int j = 0; j<N; ++j)
            {
              IP.SetVisibleComponent(j);
              dx_forward[i][j] = (it.GetPixel(center - stride[i]))[j]
                - (it.GetPixel(center))[j];
              dx_backward[i][j] = (it.GetPixel(center + stride[i]))[j]
                - (it.GetPixel(center))[j];
              dx[i][j]     = IP(x_slice[i], it, dx_op);
              dx_aug[i][j] = IP(xa_slice[i],it, dx_op);
              dx_dim[i][j] = IP(xd_slice[i],it, dx_op);
            }
        }
      for (unsigned int i = 0; i < D; ++i) // approximate gradient magnitudes
        {
          GradMag[i]   = NumericTraits<VectorValueType>::Zero;
          GradMag_d[i] = NumericTraits<VectorValueType>::Zero;

          for (unsigned int j = 0; j < N; ++j)
            {
              for (unsigned int m = 0; m < D; ++m)
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

      for (unsigned int i = 0; i < D; ++i)  // calculate conductance terms
        {
          Cx[i] = exp( GradMag[i] / k );
          Cxd[i]= exp( GradMag_d[i] / k );
        }
      
      for (unsigned int j = 0; j<N; ++j)    // update values
        {
          delta[j] = NumericTraits<VectorValueType>::Zero;
          
          for (unsigned int i = 0; i < D; ++i)
            {
              dx_forward[i][j]  *= Cx[i];
              dx_backward[i][j] *= Cxd[i];
              delta[j] += dx_forward[i][j] + dx_backward[i][j];
            }
          (*(it.GetOutputBuffer()))[j] = delta[j];
        }
    }
}

} // end namespace itk

#endif
