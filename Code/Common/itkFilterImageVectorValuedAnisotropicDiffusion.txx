/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageVectorValuedAnisotropicDiffusion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkDerivativeHalfForwardOperator.h"
#include "itkDerivativeHalfBackwardOperator.h"
#include "itkDerivativeOperator.h"
#include "itkImageRegionIterator.h"
#include "itkVectorComponentDataAccessor.h"
namespace itk {

template<class TPixel, unsigned int VDimension>
void
FilterImageVectorValuedAnisotropicDiffusion<TPixel, VDimension>
::GenerateData()
{
  Superclass::GenerateData();  // Allocates output, copies input to output
  VectorComponentDataAccessor<TPixel, ScalarValueType> accessor;
  const unsigned long VectorLength = TPixel::GetVectorDimension();
  ScalarValueType k_adj;
  
  ScalarValueType grad_mag_avg;
  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer input  = this->GetInput();
  
  // Create a temporary update image.
  ImageType::Pointer delta = ImageType::New();
  delta->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  delta->SetRequestedRegion(output->GetRequestedRegion());
  delta->SetBufferedRegion(output->GetBufferedRegion());
  delta->Allocate();

  // Define the neighborhood radius we'll need to use.
  unsigned long siz[VDimension];
  Size<VDimension> hoodRadius;
  siz[0] = 2;
  for (unsigned int i = 1; i < VDimension; ++i)
    {
      siz[i] = 1;
    }
  hoodRadius.SetSize(siz);
  
  // Do two passes: Non-boundary pixels, then boundary pixels.
  ImageRegion<VDimension> cropped;
  Size<VDimension> szc;
  Index<VDimension> idxc;
  for (unsigned int i = 0; i< VDimension; ++i)
    {
      szc[i] = delta->GetRequestedRegion().GetSize()[i] - hoodRadius[i]*2;
      idxc[i]= delta->GetRequestedRegion().GetIndex()[i]+ hoodRadius[i];
    }
  cropped.SetSize(szc);
  cropped.SetIndex(idxc);

  RegionNeighborhoodIterator<TPixel, VDimension>
    rni(hoodRadius, output, cropped);
  
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bni(hoodRadius, output, delta->GetRequestedRegion());

  for (unsigned int i=0; i< this->GetIterations(); ++i)
    {
      // Calculate average gradient magnitude as the average of the
      // image component's gradient magnitude's.
      grad_mag_avg = 0;
      VectorValueType vo;
      for (unsigned int j = 0; j < VectorLength; ++j)
        {
          accessor.SetVisibleComponent(j);
          vo=  NeighborhoodAlgorithm::AverageGradientMagnitudeSquared
            <RegionNeighborhoodIterator<TPixel, VDimension>, TPixel,
            VectorValueType>(output, cropped, accessor);
          grad_mag_avg += vo;
          //          std::cout << "vo=" << vo << std::endl;
        }
      //     grad_mag_avg /= VectorLength;

      //      std::cout << "Grad mag avg = " << grad_mag_avg << std::endl;

      k_adj = grad_mag_avg * this->GetConductanceParameter() * -1.0f;

      //      std::cout << "k_adj = " << k_adj << std::endl;

      // Calculate update image
      rni.SetOutputBuffer(delta->GetBufferPointer()
                          +delta->ComputeOffset(idxc));
      bni.SetOutputBuffer(delta->GetBufferPointer());

      if (VDimension==2)
        {
          this->VectorValuedAnisotropicDiffuse2D(rni, k_adj);
          this->VectorValuedAnisotropicDiffuse2D(bni, k_adj);
        }
      else
        {
          this->VectorValuedAnisotropicDiffuseND(rni, k_adj);
          this->VectorValuedAnisotropicDiffuseND(bni, k_adj);
        }
      
      // Update each component of the output
      for (unsigned int j = 0; j < VectorLength; ++j)
        {
          accessor.SetVisibleComponent(j);
          this->UpdateOutputScalar(delta, this->GetTimeStep(), accessor);
        }
    }
  delta->Delete();
}

template< class TPixel, unsigned int VDimension >
template< class TNeighborhoodIterator >
void
FilterImageVectorValuedAnisotropicDiffusion<TPixel, VDimension>
::VectorValuedAnisotropicDiffuse2D(TNeighborhoodIterator &it, const float k)
{
  VectorComponentDataAccessor<TPixel, VectorValueType> accessor;

  const unsigned int N = TPixel::GetVectorDimension();
  enum { X=0, Y=1 };
  
  
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

  // Process the image
  const TNeighborhoodIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      Cx_gradmag  = NumericTraits<VectorValueType>::Zero;
      Cy_gradmag  = NumericTraits<VectorValueType>::Zero;
      Cxd_gradmag = NumericTraits<VectorValueType>::Zero;
      Cyd_gradmag = NumericTraits<VectorValueType>::Zero;
      
      for (int j = 0; j<N; ++j)  // Take the derivatives & approximate gradient 
        {                        // magnitudes
          accessor.SetVisibleComponent(j);
          dx_forward[j]  = accessor.Get(it.GetPixel(6))
                           - accessor.Get(it.GetPixel(7)); 
          dx_backward[j] = accessor.Get(it.GetPixel(8))
                           - accessor.Get(it.GetPixel(7));
          dy_forward[j]  = accessor.Get(it.GetPixel(12))
                           - accessor.Get(it.GetPixel(7));
          dy_backward[j] = accessor.Get(it.GetPixel(2))
                           - accessor.Get(it.GetPixel(7));
          dx[j]         = it.SlicedInnerProduct(x_slice, dx_op, accessor);
          dy[j]         = it.SlicedInnerProduct(y_slice, dy_op, accessor);
          dx_aug[j]     = it.SlicedInnerProduct(xa_slice, dx_op, accessor);
          dy_aug[j]     = it.SlicedInnerProduct(ya_slice, dy_op, accessor);
          dx_dim[j]     = it.SlicedInnerProduct(xd_slice, dx_op, accessor);
          dy_dim[j]     = it.SlicedInnerProduct(yd_slice, dy_op, accessor);

          Cx_gradmag += (dx_forward[j]*dx_forward[j] +
                         0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]));
          Cy_gradmag += (dy_forward[j]*dy_forward[j] +
                         0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j])); 
          Cxd_gradmag +=(dx_backward[j]*dx_backward[j] +
                         0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]));
          Cyd_gradmag += (dy_backward[j]*dy_backward[j] +
                          0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]));
        }

      // Calculate conductance terms
      Cx = std::exp( Cx_gradmag / k );
      Cy = std::exp( Cy_gradmag / k );
      Cxd= std::exp( Cxd_gradmag / k );
      Cyd= std::exp( Cyd_gradmag / k );

      for (int j = 0; j<N; ++j)  // Compute diffusion updates
        {
          accessor.SetVisibleComponent(j);
          dx_forward[j]  *= Cx;
          dy_forward[j]  *= Cy;
          dx_backward[j] *= Cxd;
          dy_backward[j] *= Cyd;
          accessor.Set(*(it.GetOutputBuffer()),  dx_forward[j] + dy_forward[j]
                       + dx_backward[j] + dy_backward[j]);
        }
    }  

}

template< class TPixel, unsigned int VDimension >
template< class TNeighborhoodIterator >
void
FilterImageVectorValuedAnisotropicDiffusion<TPixel, VDimension>
::VectorValuedAnisotropicDiffuseND(TNeighborhoodIterator &it, const float k)
{
  VectorComponentDataAccessor<TPixel, VectorValueType> accessor;
  const unsigned int N = TPixel::GetVectorDimension();
  
  ScalarValueType GradMag[VDimension], GradMag_d[VDimension], delta[N];

  VectorValueType Cx[VDimension];
  VectorValueType Cxd[VDimension];
  VectorValueType Cx_gradmag[VDimension];
  VectorValueType Cxd_gradmag[VDimension];
  VectorValueType dx_forward[VDimension][N];
  VectorValueType dx_backward[VDimension][N];
  VectorValueType dx[VDimension][N];
  VectorValueType dx_aug[VDimension][N];
  VectorValueType dx_dim[VDimension][N]; 

  DerivativeOperator<VectorValueType, 2> dx_op;
   dx_op.SetDirection(0);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  // Slice the neighborhood
   unsigned long stride[VDimension];
   const unsigned long center =  it.size() / 2;
   std::slice x_slice[VDimension];
   std::slice xa_slice[VDimension];
   std::slice xd_slice[VDimension];
   for (unsigned int i = 0; i< VDimension; ++i)
     {
       stride[i]   = it.GetStride(i);
       x_slice[i]  = std::slice(center - stride[i], 3, stride[i]);
       xa_slice[i] = std::slice((center+1)-stride[i], 3, stride[i]);
       xd_slice[i] = std::slice((center-1)-stride[i], 3, stride[i]);
     }
   

 // Process the image
  const TNeighborhoodIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      for (unsigned int i = 0; i < VDimension; ++i) // Calculate all derivatives
        {
          for (unsigned int j = 0; j<N; ++j)
            {
              accessor.SetVisibleComponent(j);
              dx_forward[i][j]  = accessor.Get(it.GetPixel(center - stride[i]))
                - accessor.Get(it.GetPixel(center));
              dx_backward[i][j] = accessor.Get(it.GetPixel(center + stride[i]))
                - accessor.Get(it.GetPixel(center));
              dx[i][j]      = it.SlicedInnerProduct(x_slice[i], dx_op, accessor);
              dx_aug[i][j]  = it.SlicedInnerProduct(xa_slice[i], dx_op, accessor);
              dx_dim[i][j]  = it.SlicedInnerProduct(xd_slice[i], dx_op, accessor);
            }
        }
      for (unsigned int i = 0; i < VDimension; ++i) // Approx. grad magnitudes
        {
          GradMag[i]   = NumericTraits<ScalarValueType>::Zero;
          GradMag_d[i] = NumericTraits<ScalarValueType>::Zero;

          for (unsigned int j = 0; j < N; ++j)
            {
              for (unsigned int m = 0; m < VDimension; ++m)
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

      for (unsigned int i = 0; i < VDimension; ++i)  // Calculate conductance terms
        {
          Cx[i] = std::exp( GradMag[i] / k );
          Cxd[i]= std::exp( GradMag_d[i] / k );
        }
      
      for (unsigned int j = 0; j<N; ++j)            // Update values
        {
          delta[j] = NumericTraits<VectorValueType>::Zero;
          accessor.SetVisibleComponent(j);
          
          for (unsigned int i = 0; i < VDimension; ++i)
            {
              dx_forward[i][j]  *= Cx[i];
              dx_backward[i][j] *= Cxd[i];
              delta[j] += dx_forward[i][j] + dx_backward[i][j];
            }
          accessor.Set(*(it.GetOutputBuffer()), delta[j]);
        } 

    }
}

} // end namespace itk
