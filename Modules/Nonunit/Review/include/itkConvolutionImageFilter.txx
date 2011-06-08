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

#ifndef __itkConvolutionImageFilter_txx
#define __itkConvolutionImageFilter_txx

#include "itkConvolutionImageFilter.h"

#include "itkImageBase.h"
#include "itkImageKernelOperator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkProgressReporter.h"

#include "vnl/vnl_math.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * http://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 */

namespace itk
{
template< class TInputImage, class TOutputImage >
ConvolutionImageFilter< TInputImage, TOutputImage >
::ConvolutionImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Normalize = false;
}

template< class TInputImage, class TOutputImage >
ConvolutionImageFilter< TInputImage, TOutputImage >
::~ConvolutionImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputRegionType & outputRegionForThread, ThreadIdType threadId)
{
  // setup the progress reporter
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  typedef ConstNeighborhoodIterator< InputImageType >   NeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::RadiusType RadiusType;
  typedef typename RadiusType::SizeValueType            SizeValueType;
  RadiusType radius;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    radius[i] = Math::Floor< SizeValueType >(0.5
                                             * this->GetImageKernelInput()->GetLargestPossibleRegion().GetSize()[i]);
    }

  double scalingFactor = 1.0;
  if ( this->GetNormalize() )
    {
    double                                     sum = 0.0;
    ImageRegionConstIterator< InputImageType > It( this->GetImageKernelInput(),
                                                   this->GetImageKernelInput()->GetLargestPossibleRegion() );
    for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      sum += static_cast< double >( It.Get() );
      }
    if ( sum != 0.0 )
      {
      scalingFactor = 1.0 / sum;
      }
    }

  typedef typename NeighborhoodAlgorithm
  ::ImageBoundaryFacesCalculator< InputImageType > FaceCalculatorType;
  FaceCalculatorType faceCalculator;

  NeighborhoodInnerProduct< InputImageType, InputPixelType, double > innerProduct;

  ImageKernelOperator< InputPixelType, ImageDimension > imageKernelOperator;
  imageKernelOperator.SetImageKernel( const_cast< InputImageType * >(
                                        static_cast< const InputImageType * >(
                                          this->ProcessObject::GetInput(1) ) ) );
  imageKernelOperator.CreateToRadius(radius);

  typename FaceCalculatorType::FaceListType faceList = faceCalculator(
    this->GetInput(0), outputRegionForThread, radius);
  typename FaceCalculatorType::FaceListType::iterator fit;

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    NeighborhoodIteratorType               inIt(radius, this->GetInput(0), *fit);
    ImageRegionIterator< OutputImageType > outIt(this->GetOutput(), *fit);

    for ( inIt.GoToBegin(), outIt.GoToBegin(); !inIt.IsAtEnd();
          ++inIt, ++outIt )
      {
      outIt.Set( static_cast< OutputPixelType >(
                   scalingFactor * innerProduct(inIt, imageKernelOperator) ) );
      progress.CompletedPixel();
      }
    }
}

/**
 * ConvolutionImageFilter needs a smaller 2nd input (the image kernel)
 * requested region than output requested region.  As such,  this filter
 * needs to provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Simply copy the GenerateInputRequestedRegion() function and
  // deal with the image kernel as a special case.
  for ( unsigned int idx = 0; idx < 2; ++idx )
    {
    if ( this->GetInput(idx) )
      {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      typedef ImageBase< ImageDimension > ImageBaseType;
      typename ImageBaseType::ConstPointer constInput =
        dynamic_cast< ImageBaseType const * >(
          this->ProcessObject::GetInput(idx) );

      if ( constInput.IsNull() )
        {
        itkExceptionMacro("Input image " << idx
                                         << " not correctly specified.");
        }

      // Input is an image, cast away the constness so we can set
      // the requested region.
      typename InputImageType::Pointer input =
        const_cast< TInputImage * >( this->GetInput(idx) );

      typename InputImageType::RegionType inputRegion;
      if ( idx == 0 )
        {
        Superclass::CallCopyOutputRegionToInputRegion( inputRegion,
                                                       this->GetOutput()->GetRequestedRegion() );
        }
      else  // the input is the image kernel
        {
        typename InputImageType::RegionType::SizeType inputSize;
        typename InputImageType::RegionType::IndexType inputIndex;
        inputSize = this->GetInput(
          idx)->GetLargestPossibleRegion().GetSize();
        inputIndex = this->GetInput(
          idx)->GetLargestPossibleRegion().GetIndex();
        inputRegion.SetSize(inputSize);
        inputRegion.SetIndex(inputIndex);
        }
      input->SetRequestedRegion(inputRegion);
      }
    }
}

template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Normalize: "  << m_Normalize << std::endl;
  //  NOT REALLY MEMBER DATA. Need to fool PrintSelf check
  //  os << indent << "ImageKernel: "  << m_ImageKernel << std::e0ndl;
}
}
#endif
