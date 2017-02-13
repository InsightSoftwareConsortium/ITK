/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkCannyEdgeDetectionImageFilter_hxx
#define itkCannyEdgeDetectionImageFilter_hxx
#include "itkCannyEdgeDetectionImageFilter.h"

#include "itkZeroCrossingImageFilter.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >::CannyEdgeDetectionImageFilter() :
  m_UpperThreshold( NumericTraits< OutputImagePixelType >::ZeroValue() ),
  m_LowerThreshold( NumericTraits< OutputImagePixelType >::ZeroValue() )
{
  m_Variance.Fill(0.0);
  m_MaximumError.Fill(0.01);

  m_GaussianFilter      = GaussianImageFilterType::New();
  m_MultiplyImageFilter = MultiplyImageFilterType::New();
  m_UpdateBuffer1  = OutputImageType::New();

  // Set up neighborhood slices for all the dimensions.
  typename Neighborhood< OutputImagePixelType, ImageDimension >::RadiusType r;
  r.Fill(1);

  // Dummy neighborhood used to set up the slices
  Neighborhood< OutputImagePixelType, ImageDimension > it;
  it.SetRadius(r);

  // Slice the neighborhood
  m_Center = it.Size() / 2;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    m_Stride[i] = it.GetStride(i);
    }

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    m_ComputeCannyEdgeSlice[i] =
      std::slice(m_Center - m_Stride[i], 3, m_Stride[i]);
    }

  // Allocate the derivative operator
  m_ComputeCannyEdge1stDerivativeOper.SetDirection(0);
  m_ComputeCannyEdge1stDerivativeOper.SetOrder(1);
  m_ComputeCannyEdge1stDerivativeOper.CreateDirectional();

  m_ComputeCannyEdge2ndDerivativeOper.SetDirection(0);
  m_ComputeCannyEdge2ndDerivativeOper.SetOrder(2);
  m_ComputeCannyEdge2ndDerivativeOper.CreateDirectional();

  // Initialize the list
  m_NodeStore = ListNodeStorageType::New();
  m_NodeList = ListType::New();

  m_OutputImage = ITK_NULLPTR;
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the input

  typename TInputImage::ConstPointer input = this->GetInput();

  m_UpdateBuffer1->CopyInformation(input);
  m_UpdateBuffer1->SetRequestedRegion( input->GetRequestedRegion() );
  m_UpdateBuffer1->SetBufferedRegion( input->GetBufferedRegion() );
  m_UpdateBuffer1->Allocate();
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivative(const OutputImageRegionType &
                               outputRegionForThread, ThreadIdType threadId)
{
  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  ImageRegionIterator< TOutputImage > it;

  void *globalData = ITK_NULLPTR;

  // Here input is the result from the gaussian filter output is the update
  // buffer
  typename OutputImageType::Pointer input  = m_GaussianFilter->GetOutput();

  // Set iterator radius
  Size< ImageDimension > radius; radius.Fill(1);

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType::iterator fit;

  // Support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels(), 100, 0.0f, 0.5f);

  // Process the non-boundady region and then each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    NeighborhoodType bit(radius, input, *fit);

    it = ImageRegionIterator< OutputImageType >(this->m_OutputImage, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( !bit.IsAtEnd() )
      {
      it.Value() = ComputeCannyEdge(bit, globalData);
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
typename CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::OutputImagePixelType
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ComputeCannyEdge( const NeighborhoodType & it,
                    void *itkNotUsed(globalData) )
{

  NeighborhoodInnerProduct< OutputImageType > innerProduct;

  OutputImagePixelType dx[ImageDimension];
  OutputImagePixelType dxx[ImageDimension];
  OutputImagePixelType dxy[ImageDimension * ( ImageDimension - 1 ) / 2];

  //  double alpha = 0.01;

  // Calculate 1st & 2nd order derivative
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    dx[i] = innerProduct(m_ComputeCannyEdgeSlice[i], it,
                         m_ComputeCannyEdge1stDerivativeOper);
    dxx[i] = innerProduct(m_ComputeCannyEdgeSlice[i], it,
                          m_ComputeCannyEdge2ndDerivativeOper);
    }

  OutputImagePixelType deriv = NumericTraits< OutputImagePixelType >::ZeroValue();

  int k = 0;
  // Calculate the 2nd derivative
  for ( unsigned int i = 0; i < ImageDimension - 1; i++ )
    {
    for ( unsigned int j = i + 1; j < ImageDimension; j++ )
      {
      dxy[k] = 0.25 * it.GetPixel(m_Center - m_Stride[i] - m_Stride[j])
               - 0.25 * it.GetPixel(m_Center - m_Stride[i] + m_Stride[j])
               - 0.25 * it.GetPixel(m_Center + m_Stride[i] - m_Stride[j])
               + 0.25 * it.GetPixel(m_Center + m_Stride[i] + m_Stride[j]);

      deriv += 2.0 * dx[i] * dx[j] * dxy[k];
      k++;
      }
    }

  OutputImagePixelType gradMag = static_cast<OutputImagePixelType>(0.0001); // alpha * alpha;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    deriv += dx[i] * dx[i] * dxx[i];
    gradMag += dx[i] * dx[i];
    }

  deriv = deriv / gradMag;

  return deriv;
}

// Calculate the second derivative
template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivative()
{
  CannyThreadStruct str;

  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->Compute2ndDerivativeThreaderCallback, &str);

  this->GetMultiThreader()->SingleMethodExecute();
}

template< typename TInputImage, typename TOutputImage >
ITK_THREAD_RETURN_TYPE
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivativeThreaderCallback(void *arg)
{
  CannyThreadStruct *str;

  ThreadIdType total, threadId, threadCount;

  threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  str = (CannyThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  OutputImageRegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);

  if ( threadId < total )
    {
    str->Filter->ThreadedCompute2ndDerivative(splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Use grafting of the input and output of this filter to isolate
  // the mini-pipeline and other modifications from the pipeline.
  typename InputImageType::Pointer input = InputImageType::New();
  input->Graft( const_cast< InputImageType* >(this->GetInput()) );

  // Allocate the output, and graft
  Superclass::AllocateOutputs();
  typename OutputImageType::Pointer output = OutputImageType::New();
  output->Graft( this->GetOutput() );
  this->m_OutputImage = output;

  typename ZeroCrossingImageFilter< TOutputImage, TOutputImage >::Pointer
  zeroCrossFilter = ZeroCrossingImageFilter< TOutputImage, TOutputImage >::New();

  this->AllocateUpdateBuffer();

  // 1.Apply the Gaussian Filter to the input image
  m_GaussianFilter->SetVariance(m_Variance);
  m_GaussianFilter->SetMaximumError(m_MaximumError);
  m_GaussianFilter->SetInput(input);
  // Modify to force excution, due to grafting complications
  m_GaussianFilter->Modified();
  m_GaussianFilter->Update();

  // 2. Calculate 2nd order directional derivative
  // Calculate the 2nd order directional derivative of the smoothed image.
  // The output of this filter will be used to store the directional
  // derivative.
  this->Compute2ndDerivative();

  this->Compute2ndDerivativePos();

  // 3. Non-maximum suppression

  // Calculate the zero crossings of the 2nd directional derivative and write
  // the result to output buffer.
  zeroCrossFilter->SetInput( this->m_OutputImage );
  zeroCrossFilter->Update();

  // 4. Hysteresis Thresholding

  // First get all the edges corresponding to zerocrossings
  m_MultiplyImageFilter->SetInput1(m_UpdateBuffer1);
  m_MultiplyImageFilter->SetInput2( zeroCrossFilter->GetOutput() );

  // To save memory, we will graft the output of the m_GaussianFilter,
  // which is no longer needed, into the m_MultiplyImageFilter.
  m_MultiplyImageFilter->GraftOutput( m_GaussianFilter->GetOutput() );
  m_MultiplyImageFilter->Update();

  // Then do the double threshoulding upon the edge responses
  this->HysteresisThresholding();

  this->GraftOutput( output );
  this->m_OutputImage = ITK_NULLPTR;
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::HysteresisThresholding()
{
  // This is the Zero crossings of the Second derivative multiplied with the
  // gradients of the image. HysteresisThresholding of this image should give
  // the Canny output.
  typename OutputImageType::Pointer input = m_MultiplyImageFilter->GetOutput();
  float value;

  ListNodeType *node;

// fix me
  ImageRegionIterator< TOutputImage > oit( input, input->GetRequestedRegion() );

  oit.GoToBegin();

  ImageRegionIterator< TOutputImage > uit( this->m_OutputImage,
                                           this->m_OutputImage->GetRequestedRegion() );
  uit.GoToBegin();
  while ( !uit.IsAtEnd() )
    {
    uit.Value() = NumericTraits< OutputImagePixelType >::ZeroValue();
    ++uit;
    }

  const OutputImageType *multiplyImageFilterOutput =
    this->m_MultiplyImageFilter->GetOutput();
  while ( !oit.IsAtEnd() )
    {
    value = oit.Value();

    if ( value > m_UpperThreshold )
      {
      node = m_NodeStore->Borrow();
      node->m_Value = oit.GetIndex();
      m_NodeList->PushFront(node);
      FollowEdge( oit.GetIndex(), multiplyImageFilterOutput );
      }

    ++oit;
    }
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::FollowEdge(IndexType index, const OutputImageType *multiplyImageFilterOutput)
{
  // This is the Zero crossings of the Second derivative multiplied with the
  // gradients of the image. HysteresisThresholding of this image should give
  // the Canny output.
  InputImageRegionType inputRegion = multiplyImageFilterOutput->GetRequestedRegion();

  IndexType     nIndex;
  IndexType     cIndex;
  ListNodeType *node;

  // Assign iterator radius
  Size< ImageDimension > radius;
  radius.Fill(1);

  ConstNeighborhoodIterator< TOutputImage > oit( radius,
                                                 multiplyImageFilterOutput,
                                                 multiplyImageFilterOutput->GetRequestedRegion() );
  ImageRegionIteratorWithIndex< TOutputImage > uit( this->m_OutputImage,
                                                    this->m_OutputImage->GetRequestedRegion() );

  uit.SetIndex(index);
  if ( Math::ExactlyEquals(uit.Get(), NumericTraits< OutputImagePixelType >::OneValue()) )
    {
    // Remove the node if we are not going to follow it!
    //
    // Pop the front node from the list and read its index value.
    node = m_NodeList->Front(); // get a pointer to the first node
    m_NodeList->PopFront();     // unlink the front node
    m_NodeStore->Return(node);  // return the memory for reuse
    return;
    }

  int nSize = m_Center * 2 + 1;
  while ( !m_NodeList->Empty() )
    {
    // Pop the front node from the list and read its index value.
    node = m_NodeList->Front(); // Get a pointer to the first node
    cIndex = node->m_Value;     // Read the value of the first node
    m_NodeList->PopFront();     // Unlink the front node
    m_NodeStore->Return(node);  // Return the memory for reuse

    // Move iterators to the correct index position.
    oit.SetLocation(cIndex);
    uit.SetIndex(cIndex);
    uit.Value() = 1;

    // Search the neighbors for new indices to add to the list.
    for ( int i = 0; i < nSize; i++ )
      {
      nIndex = oit.GetIndex(i);
      uit.SetIndex(nIndex);
      if ( inputRegion.IsInside(nIndex) )
        {
        if ( oit.GetPixel(i) > m_LowerThreshold && Math::NotExactlyEquals(uit.Value(), NumericTraits< OutputImagePixelType >::OneValue())  )
          {
          node = m_NodeStore->Borrow();  // Get a new node struct
          node->m_Value = nIndex;        // Set its value
          m_NodeList->PushFront(node);   // Add the new node to the list

          uit.SetIndex(nIndex);
          uit.Value() = NumericTraits< OutputImagePixelType >::OneValue();
          }
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::ThreadedCompute2ndDerivativePos(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  ConstNeighborhoodIterator< TInputImage > bit;
  ConstNeighborhoodIterator< TInputImage > bit1;

  ImageRegionIterator< TOutputImage > it;

  // Here input is the result from the gaussian filter
  //      input1 is the 2nd derivative result
  //      output is the gradient of 2nd derivative
  typename OutputImageType::Pointer input1 = this->m_OutputImage;
  typename OutputImageType::Pointer input = m_GaussianFilter->GetOutput();

  typename  InputImageType::Pointer output  = m_UpdateBuffer1;

  // Set iterator radius
  Size< ImageDimension > radius; radius.Fill(1);

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType::iterator fit;

  // Support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels(), 100, 0.5f, 0.5f);

  InputImagePixelType zero = NumericTraits< InputImagePixelType >::ZeroValue();

  OutputImagePixelType dx[ImageDimension];
  OutputImagePixelType dx1[ImageDimension];

  OutputImagePixelType directional[ImageDimension];
  OutputImagePixelType derivPos;

  OutputImagePixelType gradMag;

  // Process the non-boundary region and then each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.

  NeighborhoodInnerProduct< OutputImageType > IP;

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIterator< InputImageType >(radius,
                                                      input, *fit);
    bit1 = ConstNeighborhoodIterator< InputImageType >(radius,
                                                       input1, *fit);
    it = ImageRegionIterator< OutputImageType >(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    bit1.GoToBegin();
    it.GoToBegin();

    while ( !bit.IsAtEnd() )
      {
      gradMag = 0.0001;

      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        dx[i] = IP(m_ComputeCannyEdgeSlice[i], bit,
                   m_ComputeCannyEdge1stDerivativeOper);
        gradMag += dx[i] * dx[i];

        dx1[i] = IP(m_ComputeCannyEdgeSlice[i], bit1,
                    m_ComputeCannyEdge1stDerivativeOper);
        }

      gradMag = std::sqrt( (double)gradMag );
      derivPos = zero;
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        // First calculate the directional derivative
        directional[i] = dx[i] / gradMag;

        // Calculate gradient of 2nd derivative
        derivPos += dx1[i] * directional[i];
        }

      it.Value() = ( ( derivPos <= zero ) );
      it.Value() = it.Get() * gradMag;
      ++bit;
      ++bit1;
      ++it;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivativePos()
{
  CannyThreadStruct str;

  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->Compute2ndDerivativePosThreaderCallback, &str);

  this->GetMultiThreader()->SingleMethodExecute();
}

template< typename TInputImage, typename TOutputImage >
ITK_THREAD_RETURN_TYPE
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::Compute2ndDerivativePosThreaderCallback(void *arg)
{
  CannyThreadStruct *str;

  ThreadIdType total, threadId, threadCount;

  threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  str = (CannyThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.

  OutputImageRegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);

  if ( threadId < total )
    {
    str->Filter->ThreadedCompute2ndDerivativePos(splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TInputImage, typename TOutputImage >
void
CannyEdgeDetectionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Variance: " << m_Variance << std::endl;
  os << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "UpperThreshold: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_UpperThreshold )
     << std::endl;
  os << indent << "LowerThreshold: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_LowerThreshold )
     << std::endl;
  os << "Center: "
     << m_Center << std::endl;
  os << "Stride: "
     << m_Stride << std::endl;
  itkPrintSelfObjectMacro( GaussianFilter );
  itkPrintSelfObjectMacro( MultiplyImageFilter );
  itkPrintSelfObjectMacro( UpdateBuffer1 );
}
} //end of itk namespace
#endif
