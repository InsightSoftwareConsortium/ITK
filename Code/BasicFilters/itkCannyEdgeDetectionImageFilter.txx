/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCannyEdgeDetectionImageFilter_txx
#define _itkCannyEdgeDetectionImageFilter_txx
#include "itkCannyEdgeDetectionImageFilter.h"

#include "itkDiscreteGaussianImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkGradientMagnitudeImageFilter.h"

namespace itk
{
  
template <class TInputImage, class TOutputImage>
CannyEdgeDetectionImageFilter<TInputImage, TOutputImage>::
CannyEdgeDetectionImageFilter()
{
  unsigned int i;

  for (i = 0; i < ImageDimension; i++)
    {
    m_Variance[i] = 0.0;
    m_MaximumError[i] = 0.01;
    }
  m_OutsideValue = NumericTraits<OutputImagePixelType>::Zero;
  m_Threshold = NumericTraits<OutputImagePixelType>::Zero;
  m_UpperThreshold = NumericTraits<OutputImagePixelType>::Zero;
  m_LowerThreshold = NumericTraits<OutputImagePixelType>::Zero;

  m_UpdateBuffer = OutputImageType::New();
  m_UpdateBuffer1 = OutputImageType::New();

  // Set up neighborhood slices for all the dimensions.
  typename Neighborhood<OutputImagePixelType, ImageDimension>::RadiusType r;
  r.Fill(1);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<OutputImagePixelType, ImageDimension> it;
  it.SetRadius(r);
  
  // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for (i = 0; i< ImageDimension; ++i)
    {
    m_Stride[i] = it.GetStride(i);
    }

  for (i = 0; i< ImageDimension; ++i)
    {
    m_ComputeCannyEdgeSlice[i]
      = std::slice( m_Center - m_Stride[i], 3, m_Stride[i]);
    }
   
  // Allocate the derivative operator.
  m_ComputeCannyEdge1stDerivativeOper.SetDirection(0);
  m_ComputeCannyEdge1stDerivativeOper.SetOrder(1);
  m_ComputeCannyEdge1stDerivativeOper.CreateDirectional();

  m_ComputeCannyEdge2ndDerivativeOper.SetDirection(0);
  m_ComputeCannyEdge2ndDerivativeOper.SetOrder(2);
  m_ComputeCannyEdge2ndDerivativeOper.CreateDirectional();

  //Initialize the list
  m_NodeStore = ListNodeStorageType::New();
  m_NodeList = ListType::New();
}
 
template <class TInputImage, class TOutputImage>
void
CannyEdgeDetectionImageFilter<TInputImage, TOutputImage>
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the input.

  typename TInputImage::ConstPointer input = this->GetInput();

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
  return;  
  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr = 
    const_cast< TInputImage * >( this->GetInput());
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  
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
    OStringStream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivative(const OutputImageRegionType&
                               outputRegionForThread, int threadId)
{
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ImageRegionIterator<TOutputImage> it;

  void *globalData = 0;

  // Here input is the result from the gaussian filter
  //      output is the update buffer.
  typename OutputImageType::Pointer input = this->GetOutput();
  typename  InputImageType::Pointer output  = m_UpdateBuffer;

  // set iterator radius
  Size<ImageDimension> radius; radius.Fill(1);

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  // Process the non-boundady region and then each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    { 
    NeighborhoodType bit(radius, input, *fit);
      
    it = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    
    while ( ! bit.IsAtEnd() )
      {
      it.Value() = ComputeCannyEdge(bit, globalData);
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
      
    }

}

template< class TInputImage, class TOutputImage >
typename CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::OutputImagePixelType
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ComputeCannyEdge(const NeighborhoodType &it,
                   void * itkNotUsed(globalData) ) 
{
  unsigned int i, j;
  NeighborhoodInnerProduct<OutputImageType> innerProduct;

  OutputImagePixelType dx[ImageDimension];
  OutputImagePixelType dxx[ImageDimension];
  OutputImagePixelType dxy[ImageDimension*(ImageDimension-1)/2];
  OutputImagePixelType deriv;
  OutputImagePixelType gradMag;

  //  double alpha = 0.01;

  //Calculate 1st & 2nd order derivative
  for(i = 0; i < ImageDimension; i++)
    {
    dx[i] = innerProduct(m_ComputeCannyEdgeSlice[i], it,
                         m_ComputeCannyEdge1stDerivativeOper); 
    dxx[i] = innerProduct(m_ComputeCannyEdgeSlice[i], it,
                          m_ComputeCannyEdge2ndDerivativeOper);  
    }

  deriv = NumericTraits<OutputImagePixelType>::Zero;
  int k = 0;

  //Calculate the 2nd derivative
  for(i = 0; i < ImageDimension-1; i++)
    {
    for(j = i+1; j < ImageDimension ; j++)
      {
      dxy[k] = 0.25 * it.GetPixel(m_Center - m_Stride[i] - m_Stride[j])
        - 0.25 * it.GetPixel(m_Center - m_Stride[i]+ m_Stride[j])
        -0.25 * it.GetPixel(m_Center + m_Stride[i] - m_Stride[j])
        +0.25 * it.GetPixel(m_Center + m_Stride[i] + m_Stride[j]);

      deriv += 2.0 * dx[i]*dx[j]*dxy[k];
      k++;
      }
    }
  
  gradMag = 0.0001; // alpha * alpha;
  for (i = 0; i < ImageDimension; i++)
    { 
    deriv += dx[i] * dx[i] * dxx[i];
    gradMag += dx[i] * dx[i];
    }
  
  deriv = deriv/gradMag;

  return deriv;  
}

// Calculate the second derivative
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

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{

  //First need to assign upper/lower threshold      
  m_UpperThreshold = m_Threshold;
  m_LowerThreshold = m_Threshold/2.0;

  // Need to allocate output buffer
  typename OutputImageType::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  typename  InputImageType::ConstPointer  input  = this->GetInput();
  typename  OutputImageType::Pointer zeroCross;

  // Create the filters that are needed.
  typename DiscreteGaussianImageFilter<TInputImage, TOutputImage>::Pointer 
    gaussianFilter = DiscreteGaussianImageFilter<TInputImage, TOutputImage>::New();

  typename ZeroCrossingImageFilter<TOutputImage, TOutputImage>::Pointer 
    zeroCrossFilter = ZeroCrossingImageFilter<TOutputImage, TOutputImage>::New();

  typename GradientMagnitudeImageFilter<TOutputImage, TOutputImage>::Pointer
    gradMag = GradientMagnitudeImageFilter<TOutputImage, TOutputImage>::New();

  typename MultiplyImageFilter<TOutputImage, TOutputImage,TOutputImage>::Pointer multFilter 
    = MultiplyImageFilter<TOutputImage, TOutputImage,TOutputImage>::New();

  this->AllocateUpdateBuffer();


  // 1.Apply the Gaussian Filter to the input image.
  gaussianFilter->SetVariance(m_Variance);
  gaussianFilter->SetMaximumError(m_MaximumError);
  gaussianFilter->SetInput(input);
  gaussianFilter->Update();

  // Write the gaussian smoothed image to output
  this->GraftOutput(gaussianFilter->GetOutput());


  //2. Calculate 2nd order directional derivative
  // Calculate the 2nd order directional derivative of the smoothed image and write
  //the result to  the m_UpdateBuffer image.
  this->Compute2ndDerivative();

  //3. Non-maximum suppression
  // Calculate the zero crossings of the 2nd directional derivative and write 
  //the result to output buffer.
  zeroCrossFilter->SetInput(m_UpdateBuffer);
  zeroCrossFilter->Update();
  zeroCross = zeroCrossFilter->GetOutput();
 
  this->Compute2ndDerivativePos();      
  
  //4 Hysteresis Thresholding
  //First get all the edges corresponding to zerocrossings
  multFilter->SetInput1(m_UpdateBuffer1);
  multFilter->SetInput2(zeroCross);
  multFilter->Update();

  this->GraftOutput(multFilter->GetOutput());

  //Then do the double threshoulding upon the edge reponses
  this->HysteresisThresholding();
  
  this->GraftOutput(m_UpdateBuffer);
  
}

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::HysteresisThresholding()
{
  typename OutputImageType::Pointer output = this->GetOutput();
  float value;

  ListNodeType *node;
  //m_UpperThreshold = m_UpperThreshold * (m_GradMax-m_GradMin) + m_GradMin;
  //m_LowerThreshold = m_LowerThreshold * (m_GradMax-m_GradMin) + m_GradMin;

  ImageRegionIterator<TOutputImage> oit(output, output->GetRequestedRegion());
  
  oit.GoToBegin();
  
  ImageRegionIterator<TOutputImage> uit(m_UpdateBuffer,
                                        m_UpdateBuffer->GetRequestedRegion());
  uit.GoToBegin();
  while(!uit.IsAtEnd())
    {
    uit.Value() = 0;
    ++uit;
    }

  while(!oit.IsAtEnd())
    {
    value = oit.Value();

    if(value > m_UpperThreshold){
    node = m_NodeStore->Borrow();
    node->m_Value = oit.GetIndex();
    m_NodeList->PushFront(node);
    FollowEdge(oit.GetIndex());
    }

    //FollowEdge(oit.GetIndex());

    ++oit;
    }

}

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::FollowEdge(IndexType index)
{
  
  typename OutputImageType::Pointer output = this->GetOutput();
  IndexType nIndex;
  IndexType cIndex;
  ListNodeType * node;

  //assign iterator radius
  Size<ImageDimension> radius; radius.Fill(1);

  ConstNeighborhoodIterator<TOutputImage> oit(radius, output, output->GetRequestedRegion());
  
  ImageRegionIteratorWithIndex<TOutputImage> uit(m_UpdateBuffer,
                                                 m_UpdateBuffer->GetRequestedRegion());

  uit.SetIndex(index);
  if(uit.Get() == 1) return;
                                        
  int nSize = m_Center * 2 +1;
  
  while(!m_NodeList->Empty())
    {
    node = m_NodeList->Front();
    cIndex = node->m_Value;
    oit.SetLocation(cIndex);
    uit.SetIndex(cIndex);
    uit.Value() = 1;

    for(int i = 0; i < nSize; i++)
      {
      nIndex = oit.GetIndex(i);
      uit.SetIndex(nIndex);
      if(InBounds(nIndex))
        if(oit.GetPixel(i) > m_LowerThreshold && uit.Value() != 1  )
          {
          node = m_NodeStore->Borrow();
          node->m_Value = nIndex;
          m_NodeList->PushFront(node);
                
          uit.SetIndex(nIndex);
          uit.Value() = 1;
          }
      }

    m_NodeList->PopFront();
    m_NodeStore->Return(node);

    }
}

/*  
template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::FollowEdge(IndexType index)
{

  typename OutputImageType::Pointer output = this->GetOutput();
  float value;
  IndexType nIndex;

  
  //assign iterator radius
  Size<ImageDimension> radius; radius.Fill(1);
  
  ConstNeighborhoodIterator<TOutputImage> oit(radius, output, output->GetRequestedRegion());
  
  ImageRegionIteratorWithIndex<TOutputImage> uit(m_UpdateBuffer,
                                                 m_UpdateBuffer->GetRequestedRegion());
  
  uit.SetIndex(index);
  
  if(uit.Get() ==1) return;
  
  uit.Value() = 1;
  
  int nSize = m_Center * 2 +1;
  
  oit.SetLocation(index);
  
  for(int i = 0; i < nSize; i++)
    {
    nIndex = oit.GetIndex(i);
    uit.SetIndex(nIndex);
    if(InBounds(nIndex))
      if(oit.GetPixel(i) > m_LowerThreshold && uit.Value() != 1  )
        {
        uit.Value() = 1;
        oit.SetLocation(nIndex);
        i = -1;
        }
    }
  
}
*/

template< class TInputImage, class TOutputImage >
bool
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::InBounds(IndexType index)
{
  typename InputImageType::ConstPointer input = this->GetInput();
  typename InputImageType::SizeType sz;
  sz = (input->GetRequestedRegion()).GetSize();
  
  for(unsigned int i = 0; i < ImageDimension; i++)
    {
    if(index[i] < 0 ||
       index[i] >= static_cast<typename IndexType::IndexValueType>(sz[i]))
      {
      return false;
      }
    }
  return true;

}

template< class TInputImage, class TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivativePos(const OutputImageRegionType& outputRegionForThread, int threadId)
{

  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ConstNeighborhoodIterator<TInputImage> bit;
  ConstNeighborhoodIterator<TInputImage> bit1;

  ImageRegionIterator<TOutputImage> it;

  // Here input is the result from the gaussian filter
  //      input1 is the 2nd derivative result
  //      output is the gradient of 2nd derivative
  typename OutputImageType::Pointer input1 = m_UpdateBuffer;
  typename OutputImageType::Pointer input = this->GetOutput();

  typename  InputImageType::Pointer output  = m_UpdateBuffer1;
  

  // set iterator radius
  Size<ImageDimension> radius; radius.Fill(1);

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  InputImagePixelType zero = NumericTraits<InputImagePixelType>::Zero;

  OutputImagePixelType dx[ImageDimension]; 
  OutputImagePixelType dx1[ImageDimension];

  OutputImagePixelType directional[ImageDimension];
  OutputImagePixelType derivPos;

  OutputImagePixelType gradMag;

  // Process the non-boundary region and then each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.

  NeighborhoodInnerProduct<OutputImageType>  IP;

  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    {   
    bit = ConstNeighborhoodIterator<InputImageType>(radius,
                                                    input, *fit);
    bit1 =ConstNeighborhoodIterator<InputImageType>(radius, 
                                                    input1, *fit);
    it = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    bit1.GoToBegin();
    it.GoToBegin();

    while ( ! bit.IsAtEnd()  )
      {
      
      gradMag = 0.0001;
      
      for ( unsigned int i = 0; i < ImageDimension; i++)
        {
        dx[i] = IP(m_ComputeCannyEdgeSlice[i], bit,
                   m_ComputeCannyEdge1stDerivativeOper);
        gradMag += dx[i] * dx[i];
        
        dx1[i] = IP(m_ComputeCannyEdgeSlice[i], bit1,
                    m_ComputeCannyEdge1stDerivativeOper);
        }
      
      gradMag = vcl_sqrt(gradMag);          
      derivPos = zero;
      for ( unsigned int i = 0; i < ImageDimension; i++)
        {
              
        //First calculate the directional derivative

        directional[i] = dx[i]/gradMag;
                               
        //calculate gradient of 2nd derivative
              
        derivPos += dx1[i] * directional[i];
        }
          
      it.Value() = ((derivPos <= zero)) ;
      it.Value() = it.Get() * gradMag;
      ++bit;
      ++bit1;
      ++it;
      progress.CompletedPixel();
      }
      
    }  
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

template <class TInputImage, class TOutputImage>
void 
CannyEdgeDetectionImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Variance: "
     << m_Variance << std::endl;
  os << "MaximumError: "
     << m_MaximumError << std::endl;
  os << indent << "Threshold: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>
    (m_Threshold)
     << std::endl;
  os << indent << "UpperThreshold: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>
    (m_UpperThreshold)
     << std::endl;
  os << indent << "LowerThreshold: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>
    (m_LowerThreshold)
     << std::endl;
  os << indent << "OutsideValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_OutsideValue)
     << std::endl;
  os << "Center: "
     << m_Center << std::endl;
  os << "Stride: "
     << m_Stride << std::endl;
  os << "UpdateBuffer: "
     << m_UpdateBuffer;
  os << "UpdateBuffer1: "
     << m_UpdateBuffer1;
}

}//end of itk namespace
#endif
