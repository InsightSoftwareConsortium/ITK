/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilter.txx
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

#ifndef _itkCannyEdgeDetectionImageFilter_txx
#define _itkCannyEdgeDetectionImageFilter_txx

#include "itkDiscreteGaussianImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNumericTraits.h"


namespace itk
{

template <class TInputImage, class TOutputImage>
void
CannyEdgeDetectionImageFilter<TInputImage, TOutputImage>
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the output.
  typename TInputImage::Pointer input = this->GetInput();

  m_UpdateBuffer->SetLargestPossibleRegion(input->GetLargestPossibleRegion());
  m_UpdateBuffer->SetRequestedRegion(input->GetRequestedRegion());
  m_UpdateBuffer->SetBufferedRegion(input->GetBufferedRegion());
  m_UpdateBuffer->Allocate();
  
  m_UpdateBuffer1->SetLargestPossibleRegion(input->GetLargestPossibleRegion());
  m_UpdateBuffer1->SetRequestedRegion(input->GetRequestedRegion());
  m_UpdateBuffer1->SetBufferedRegion(input->GetBufferedRegion());
  m_UpdateBuffer1->Allocate();
}


template <class TInputImage, class TOutputImage>
void 
CannyEdgeDetectionImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  //Set the kernel size.
  unsigned long radius = 1;
  
  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()" << std::ends;
    e.SetLocation(msg.str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivative(const OutputImageRegionType& outputRegionForThread, int threadId)
{


  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  //  ConstNeighborhoodIterator<TInputImage> nit;
  //ConstSmartNeighborhoodIterator<TInputImage> bit;
  ImageRegionIterator<TOutputImage> it;

  void *globalData;

  // Here input is the result from the gaussian filter
  // Output is the update buffer;
  typename OutputImageType::Pointer input = this->GetOutput();
  typename  InputImageType::Pointer output  = m_UpdateBuffer;
  

  // set iterator radius
  Size<ImageDimension> radius;
  for (i = 0; i < ImageDimension; ++i) radius[i]  = 1;

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;
  fit = faceList.begin();

  // support progress methods/callbacks
  unsigned long ii = 0;
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Process non-boundary face
  
  NeighborhoodType nit(radius, input, *fit);

  it  = ImageRegionIterator<TOutputImage>(output, *fit);

  nit.GoToBegin();
  it.GoToBegin();

  InputImagePixelType zero = NumericTraits<InputImagePixelType>::Zero;


  // Now Process the non-boundary region.
  while( ! nit.IsAtEnd() )
    {
      if ( threadId == 0 && !(ii % updateVisits ) )
        {
          this->UpdateProgress((float)ii++ / (float)totalPixels);
        }
      
      it.Value() = ComputeCannyEdge(nit, globalData);
      ++nit;
      ++it;
    }
  
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (++fit; fit != faceList.end(); ++fit)
    { 

      
      //bit = ConstSmartNeighborhoodIterator<OutputImageType>(radius,
      //                                                     input, *fit);

      BoundaryNeighborhoodType bit(radius, input, *fit);

      it = ImageRegionIterator<OutputImageType>(output, *fit);
      bit.OverrideBoundaryCondition(&nbc);
      bit.GoToBegin();
    
      while ( ! bit.IsAtEnd() )
        {
          if ( threadId == 0 && !(ii % updateVisits ) )
            {
              this->UpdateProgress((float)ii++ / (float)totalPixels);
            }
          
          it.Value() = ComputeCannyEdge(bit, globalData);
          
          ++bit;
          ++it;
        }
      
    }
  
  //  cout<<"Finished calculate"<<endl;
  
}



template< class TInputImage, class TOutputImage >
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >::OutputImagePixelType

CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ComputeCannyEdge(const NeighborhoodType &it,
                   void *globalData ) 
{

  OutputImagePixelType dx[ImageDimension];
  OutputImagePixelType dxx[ImageDimension];
  OutputImagePixelType dxy[ImageDimension*(ImageDimension-1)/2];
  OutputImagePixelType deriv;
  OutputImagePixelType gradMag;

  DerivativeOperator<OutputImagePixelType,ImageDimension> oper;
  NeighborhoodInnerProduct<OutputImageType> innerProduct;

  //Calculate 1st & 2nd order derivative
  for(int i = 0; i < ImageDimension; i++)
    {
      oper.SetDirection( i ); 
      oper.SetOrder(1);
      oper.CreateToRadius(1);
      dx[i] = innerProduct(it, oper);
      
      oper.SetDirection( i ); 
      oper.SetOrder(2);
      oper.CreateToRadius(1);
      dxx[i] = innerProduct(it, oper);
    }
  

  unsigned long center = it.Size()/2;
  
  unsigned long stride[ImageDimension];

  for ( int i = 0; i < ImageDimension; i++)
    stride[i] = it.GetStride(i);

  OutputImagePixelType zero = NumericTraits<OutputImagePixelType>::Zero;

  deriv = zero;
  int k = 0;

  //Calculate the 2nd derivative
  for(int i = 0; i < ImageDimension-1; i++)
    {
      for(int j = i+1; j < ImageDimension ; j++)
        {

          dxy[k] = 0.25 * it.GetPixel(center - stride[i] -stride[j])
            - 0.25 * it.GetPixel(center - stride[i]+ stride[j])
            -0.25 * it.GetPixel(center +stride[i] - stride[j])
            +0.25 * it.GetPixel(center +stride[i] + stride[j]);

          deriv += 2.0 * dx[i]*dx[j]*dxy[k];
          k++;
        }
    }
  
  gradMag = zero;
  for (int i = 0; i < ImageDimension; i++)
    { 
      deriv += dx[i] * dx[i] * dxx[i];
      gradMag += dx[i] * dx[i];
    }
  
  if(gradMag != zero)
    {
      //gradMag = std::sqrt(gradMag);
      deriv = deriv/gradMag;
    }
  else
    deriv = zero;
  

  return deriv;
  
}


template< class TInputImage, class TOutputImage >
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >::OutputImagePixelType
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ComputeCannyEdge(const BoundaryNeighborhoodType &bit,
                   void *globalData )
{

  
  OutputImagePixelType dx[ImageDimension];
  OutputImagePixelType dxx[ImageDimension];
  OutputImagePixelType dxy[ImageDimension*(ImageDimension-1)/2];
  OutputImagePixelType deriv;
  OutputImagePixelType gradMag;


  DerivativeOperator<OutputImagePixelType, ImageDimension> oper;
  SmartNeighborhoodInnerProduct<OutputImageType> innerProduct;

  //Calculate 1st & 2nd order derivative
  for(int i = 0; i < ImageDimension; i++)
    {
      oper.SetDirection( i ); 
      oper.SetOrder(1);
      oper.CreateToRadius(1);
      dx[i] = innerProduct(bit, oper);
      
      oper.SetDirection( i ); 
      oper.SetOrder(2);
      oper.CreateToRadius(1);
      dxx[i] = innerProduct(bit, oper);
    }
  

  unsigned long center = bit.Size()/2;
  
  unsigned long stride[ImageDimension];

  for ( int i = 0; i < ImageDimension; i++)
    stride[i] = bit.GetStride(i);

  OutputImagePixelType zero = NumericTraits<OutputImagePixelType>::Zero;

  deriv = zero;
  int k = 0;


  //calculate the 2nd derivative
  for(int i = 0; i < ImageDimension-1; i++)
    {
      for(int j = i+1; j < ImageDimension ; j++)
        {

          dxy[k] = 0.25 * bit.GetPixel(center - stride[i] -stride[j])
            - 0.25 * bit.GetPixel(center - stride[i]+ stride[j])
            -0.25 * bit.GetPixel(center +stride[i] - stride[j])
            +0.25 * bit.GetPixel(center +stride[i] + stride[j]);

          deriv += 2.0*dx[i]*dx[j]*dxy[k];
          k++;
        }
    }
  
  gradMag = zero;
  for (int i = 0; i < ImageDimension; i++)
    { 
      deriv += dx[i] * dx[i] * dxx[i];
      gradMag += dx[i] * dx[i];
    }
  
    if(gradMag != zero)
    {
      //gradMag = gradMag * std::sqrt(gradMag);
      deriv = deriv/gradMag;
    }
  else
    deriv = zero;
  

  return deriv;
 

}

//Calculate the second derivative

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivative() 
{
  CannyThreadStruct str;
  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->Compute2ndDerivativeThreaderCallback, &str);
  
  this->GetMultiThreader()->SingleMethodExecute();
}

template<class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
CannyEdgeDetectionImageFilter<TInputImage, TOutputImage>
::Compute2ndDerivativeThreaderCallback( void * arg )
{
  CannyThreadStruct *str;
  
  int total, threadId, threadCount;
  
  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;
  
  str = (CannyThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.

  OutputImageRegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);
  
  if (threadId < total)
    {
    str->Filter->ThreadedCompute2ndDerivative(splitRegion, threadId);
    }
  
  return ITK_THREAD_RETURN_VALUE;
}


  //The following methos is not good. lets work on neighbor hood instead.
  
template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{


  unsigned int i;

  typename  InputImageType::Pointer input  = this->GetInput();
  typename  OutputImageType::Pointer sndDeriv;
  typename  OutputImageType::Pointer zeroCross;
  typename  OutputImageType::Pointer sndDerivPos;
  typename  OutputImageType::Pointer multOut, edge;

  //create the filters that needed
  DiscreteGaussianImageFilter<TInputImage, TOutputImage>::Pointer gaussianFilter
    = DiscreteGaussianImageFilter<TInputImage, TOutputImage>::New();

  ZeroCrossingImageFilter<TOutputImage, TOutputImage>::Pointer zeroCrossFilter
    = ZeroCrossingImageFilter<TOutputImage, TOutputImage>::New();

  ThresholdImageFilter<TOutputImage>::Pointer threshFilter
    = ThresholdImageFilter<TOutputImage>::New();

  MultiplyImageFilter<TOutputImage, TOutputImage,TOutputImage>::Pointer multFilter 
    = MultiplyImageFilter<TOutputImage, TOutputImage,TOutputImage>::New();



  this->AllocateUpdateBuffer();

  //Apply Gaussian Filter
  gaussianFilter->SetVariance(m_Variance);
  gaussianFilter->SetMaximumError(m_MaximumError);
  gaussianFilter->SetInput(input);
  gaussianFilter->Update();


  //write the gaussian smoothed image to output
  this->GraftOutput(gaussianFilter->GetOutput());
  
  //Calculate the 2nd Derivative and write the output to m_UpdateBuffer

  this->Compute2ndDerivative();


  //calculate zeroCrossing and write the result to output buffer
  zeroCrossFilter->SetInput(m_UpdateBuffer);
  zeroCrossFilter->Update();
  zeroCross = zeroCrossFilter->GetOutput();
  
  // Calculate the 2nd derivative gradient here, and write result to m_UpdateBuffer1

  this->Compute2ndDerivativePos();
 
  // multiply the edge with the zerocrossing
  multFilter->SetInput1(m_UpdateBuffer1);
  multFilter->SetInput2(zeroCross);
  multFilter->Update();

  edge = multFilter->GetOutput();

  //Do Thresholding 
  //Note: Here we need connected-components to implement the classical
  //       canny edge
  threshFilter->ThresholdBelow(m_Threshold);
  threshFilter->SetInput(edge);
  threshFilter->Update();


  // graft the output of the mini-pipeline back onto the filter's output.
  // this copies back the region ivars and meta-data
  
  this->GraftOutput(threshFilter->GetOutput());

}



template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivativePos(const OutputImageRegionType& outputRegionForThread, int threadId)
{

  //  cout<<"being generating data"<<endl;
  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ConstNeighborhoodIterator<TInputImage> nit;
  ConstNeighborhoodIterator<TInputImage> nit1;

  ConstSmartNeighborhoodIterator<TInputImage> bit;
  ConstSmartNeighborhoodIterator<TInputImage> bit1;

  ImageRegionIterator<TOutputImage> it;

  void *globalData;

  // Here input is the result from the gaussian filter
  //      input1 is the 2nd derivative result
  //      output is  the gradident of 2nd derivative
  typename OutputImageType::Pointer input1 = m_UpdateBuffer;
  typename OutputImageType::Pointer input = this->GetOutput();

  typename  InputImageType::Pointer output  = m_UpdateBuffer1;
  

  // set iterator radius
  Size<ImageDimension> radius;
  for (i = 0; i < ImageDimension; ++i) radius[i]  = 1;

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;
  fit = faceList.begin();

  // support progress methods/callbacks
  unsigned long ii = 0;
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Process non-boundary face
  nit = ConstNeighborhoodIterator<TInputImage>(radius, input, *fit);
  nit1 = ConstNeighborhoodIterator<TInputImage>(radius, input1, *fit);
  it  = ImageRegionIterator<TOutputImage>(output, *fit);

  nit.GoToBegin();
  nit1.GoToBegin();
  it.GoToBegin();


  InputImagePixelType zero = NumericTraits<InputImagePixelType>::Zero;
  OutputImagePixelType one = NumericTraits<OutputImagePixelType>::One;


  OutputImagePixelType dx[ImageDimension]; 
  
  OutputImagePixelType dx1[ImageDimension];

  OutputImagePixelType directional[ImageDimension];
  OutputImagePixelType derivPos;

  OutputImagePixelType gradMag = zero;

  DerivativeOperator<OutputImagePixelType,ImageDimension> oper;
  NeighborhoodInnerProduct<OutputImageType>  innerProduct;

  int s = 0;
  // Now Process the non-boundary region.
  while( ! nit.IsAtEnd() )
    {
      if ( threadId == 0 && !(ii % updateVisits ) )
        {
          this->UpdateProgress((float)ii++ / (float)totalPixels);
        }
      
      //First calculate the directional derivatives

      for ( int i = 0; i < ImageDimension; i++)
        {
          oper.SetDirection(i);
          oper.SetOrder(1);
          oper.CreateToRadius(1);
          dx[i] = innerProduct(nit, oper);
          gradMag += dx[i] * dx[i];
          
          dx1[i] = innerProduct(nit1, oper);
        }


      derivPos = zero;
      for ( int i = 0; i < ImageDimension; i++)
        {

          //First calculate the directional derivative
          if ( gradMag != zero)
            {
              gradMag = std::sqrt(gradMag);
              directional[i] = dx[i]/gradMag;
            }
          else
            directional[i] = zero;

          //calculate gradient of 2nd derivative

          derivPos += dx1[i] * directional[i];
        }
      
      it.Value() = derivPos;
      
      ++nit;
      ++nit1;
      ++it;
    }
  
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.

  SmartNeighborhoodInnerProduct<OutputImageType>  IP;

  for (++fit; fit != faceList.end(); ++fit)
    { 

      
      bit = ConstSmartNeighborhoodIterator<InputImageType>(radius,
                                                           input, *fit);
      bit1 =ConstSmartNeighborhoodIterator<InputImageType>(radius, 
                                                           input1, *fit);
      it = ImageRegionIterator<OutputImageType>(output, *fit);
      bit.OverrideBoundaryCondition(&nbc);
      bit.GoToBegin();
      bit1.GoToBegin();
      it.GoToBegin();

      while ( ! bit.IsAtEnd() )
        {
          if ( threadId == 0 && !(ii % updateVisits ) )
            {
              this->UpdateProgress((float)ii++ / (float)totalPixels);
            }
          
          for ( int i = 0; i < ImageDimension; i++)
            {
              oper.SetDirection(i);
              oper.SetOrder(1);
              oper.CreateToRadius(1);
              dx[i] = IP(bit, oper);
              gradMag += dx[i] * dx[i];
              
              dx1[i] = IP(bit1, oper);
            }
          
          
          derivPos = zero;
          for ( int i = 0; i < ImageDimension; i++)
            {
              
              //First calculate the directional derivative
              if ( gradMag != zero)
                {
                  gradMag = std::sqrt(gradMag);
                  directional[i] = dx[i]/gradMag;
                }
              else
                directional[i] = zero;
                    
              //calculate gradient of 2nd derivative
              
              derivPos += dx1[i] * directional[i];
            }
          
          it.Value() = derivPos;
          
          ++bit;
          ++bit1;
          ++it;
        }
      
    }
  
  //  cout<<"Finished calculate"<<endl;
  
}



//Calculate the second derivative


template< class TInputImage, class TOutputImage >
void 
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivativePos() 
{
  CannyThreadStruct str;
  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->Compute2ndDerivativePosThreaderCallback, &str);
  
  this->GetMultiThreader()->SingleMethodExecute();
}

template<class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
CannyEdgeDetectionImageFilter<TInputImage, TOutputImage>
::Compute2ndDerivativePosThreaderCallback( void * arg )
{
  CannyThreadStruct *str;
  
  int total, threadId, threadCount;
  
  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;
  
  str = (CannyThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.

  OutputImageRegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);
  
  if (threadId < total)
    {
    str->Filter->ThreadedCompute2ndDerivativePos( splitRegion, threadId);
    }
  
  return ITK_THREAD_RETURN_VALUE;
}






}//end of itk namespace



#endif
