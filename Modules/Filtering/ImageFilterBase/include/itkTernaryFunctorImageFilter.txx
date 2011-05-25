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
#ifndef __itkTernaryFunctorImageFilter_txx
#define __itkTernaryFunctorImageFilter_txx

#include "itkTernaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::TernaryFunctorImageFilter() : m_UsePhysicalSpace(false), m_PhysicalSpacesMatch(false)
{
  this->InPlaceOff();
  // this is not a good idea for e.g. itk::DivideImageFilter, and
  // should be overridden in subclasses where appropriate.
  //
  this->m_DefaultValue2 = NumericTraits<Interpolator2OutputPixelType>::Zero;
  this->m_DefaultValue3 = NumericTraits<Interpolator3OutputPixelType>::Zero;
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::SetInput1(const TInputImage1 *image1)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 0, const_cast< TInputImage1 * >( image1 ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::SetInput2(const TInputImage2 *image2)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 1, const_cast< TInputImage2 * >( image2 ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::SetInput3(const TInputImage3 *image3)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 2, const_cast< TInputImage3 * >( image3 ) );
}

/**
 * BeforeThreadedGenerateData function. Validate inputs
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::BeforeThreadedGenerateData()
{
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ( ProcessObject::GetInput(0) ) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ( ProcessObject::GetInput(1) ) );
  Input3ImagePointer inputPtr3 =
    dynamic_cast< const TInputImage3 * >( ( ProcessObject::GetInput(2) ) );

  if ( inputPtr1.IsNull() || inputPtr2.IsNull() || inputPtr3.IsNull() )
    {
    itkExceptionMacro( << "At least one input is missing."
                       << " Input1 is " << inputPtr1.GetPointer() << ", "
                       << " Input2 is " << inputPtr2.GetPointer() << ", "
                       << " Input3 is " << inputPtr3.GetPointer() );
    }
  //
  // if the physical spaces are the same, then you can work in index space.
  this->m_PhysicalSpacesMatch = inputPtr1->GetOrigin() == inputPtr2->GetOrigin() &&
    inputPtr1->GetLargestPossibleRegion() == inputPtr2->GetLargestPossibleRegion() &&
    inputPtr1->GetSpacing() == inputPtr2->GetSpacing() &&
    inputPtr1->GetDirection() == inputPtr2->GetDirection() &&
    inputPtr1->GetOrigin() == inputPtr3->GetOrigin() &&
    inputPtr1->GetLargestPossibleRegion() == inputPtr3->GetLargestPossibleRegion() &&
    inputPtr1->GetSpacing() == inputPtr3->GetSpacing() &&
    inputPtr1->GetDirection() == inputPtr3->GetDirection();
  //
  // don't bother allocating an interpolator if it isn't going to be used.
  if(this->m_UsePhysicalSpace && !this->m_PhysicalSpacesMatch)
    {
    if(this->m_Interpolator2.IsNull())
      {
      this->m_Interpolator2 =
        this->NewDefaultInterpolator2(static_cast<typename TInputImage2::PixelType *>(0));
      }
    if(this->m_Interpolator3.IsNull())
      {
      this->m_Interpolator3 =
        this->NewDefaultInterpolator3(static_cast<typename TInputImage3::PixelType *>(0));
      }
    this->m_Interpolator2->SetInputImage(inputPtr2);
    this->m_Interpolator3->SetInputImage(inputPtr3);
    }
}

/**
 * ThreadedGenerateData function. Performs the pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter< TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second or third input.
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ( ProcessObject::GetInput(0) ) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ( ProcessObject::GetInput(1) ) );
  Input3ImagePointer inputPtr3 =
    dynamic_cast< const TInputImage3 * >( ( ProcessObject::GetInput(2) ) );
  OutputImagePointer outputPtr = this->GetOutput(0);

  ImageRegionIterator< TOutputImage >      outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  outputIt.GoToBegin();
  //
  // simple case -- either default index space mode, or
  // all physical spaces match
  if(!this->m_UsePhysicalSpace || this->m_PhysicalSpacesMatch)
    {
    ImageRegionConstIterator< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
    ImageRegionConstIterator< TInputImage2 > inputIt2(inputPtr2, outputRegionForThread);
    ImageRegionConstIterator< TInputImage3 > inputIt3(inputPtr3, outputRegionForThread);

    inputIt1.GoToBegin();
    inputIt2.GoToBegin();
    inputIt3.GoToBegin();
    while ( !inputIt1.IsAtEnd() )
      {
      outputIt.Set( m_Functor( inputIt1.Get(), inputIt2.Get(), inputIt3.Get() ) );
      ++inputIt1;
      ++inputIt2;
      ++inputIt3;
      ++outputIt;
      progress.CompletedPixel(); // potential exception thrown here
      }
    }
  else
    {
    ImageRegionConstIteratorWithIndex< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
    inputIt1.GoToBegin();

    while ( !inputIt1.IsAtEnd() )
      {
      typename TInputImage1::IndexType index1 = inputIt1.GetIndex();
      typename TInputImage2::PointType pt;
      inputPtr1->TransformIndexToPhysicalPoint(index1,pt);
      Interpolator2OutputPixelType interpVal2;
      Interpolator3OutputPixelType interpVal3;
      if(this->m_Interpolator2->IsInsideBuffer(pt))
        {
        interpVal2 = this->m_Interpolator2->Evaluate(pt);
        }
      else
        {
        interpVal2 = this->m_DefaultValue2;
        }
      if(this->m_Interpolator3->IsInsideBuffer(pt))
        {
        interpVal3 = this->m_Interpolator3->Evaluate(pt);
        }
      else
        {
        interpVal3 = this->m_DefaultValue3;
        }

      outputIt.Set( m_Functor( inputIt1.Get(), interpVal2, interpVal3 ) );
      ++inputIt1;
      ++outputIt;
      progress.CompletedPixel(); // potential exception thrown here
      }
    }
}
} // end namespace itk

#endif
