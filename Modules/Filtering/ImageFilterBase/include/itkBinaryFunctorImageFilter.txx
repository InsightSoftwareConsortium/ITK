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
#ifndef __itkBinaryFunctorImageFilter_txx
#define __itkBinaryFunctorImageFilter_txx

#include "itkBinaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::BinaryFunctorImageFilter() : m_UsePhysicalSpace(false), m_PhysicalSpacesMatch(false)
{
  this->SetNumberOfRequiredInputs(2);
  this->InPlaceOff();
  /* this is not a good idea for e.g. itk::DivideImageFilter, and
   * should be overridden in subclasses where appropriate.
   */
  this->m_DefaultValue = NumericTraits<InterpolatorOutputPixelType>::Zero;
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput1(const TInputImage1 *image1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 0, const_cast< TInputImage1 * >( image1 ) );
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput1(const DecoratedInput1ImagePixelType * input1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 0, const_cast< DecoratedInput1ImagePixelType * >( input1 ) );
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput1(const Input1ImagePixelType & input1)
{
  itkDebugMacro("setting input1 to " << input1);
  typename DecoratedInput1ImagePixelType::Pointer newInput = DecoratedInput1ImagePixelType::New();
  newInput->Set(input1);
  this->SetInput1(newInput);
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetConstant1(const Input1ImagePixelType & input1)
{
  this->SetInput1(input1);
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
const typename BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >::Input1ImagePixelType &
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::GetConstant1() const
{
  itkDebugMacro("Getting constant 1");
  const DecoratedInput1ImagePixelType *input = dynamic_cast< const DecoratedInput1ImagePixelType * >(
      this->ProcessObject::GetInput(0) );
  if( input == NULL )
    {
    itkExceptionMacro(<<"Constant 1 is not set");
    }
  return input->Get();
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput2(const TInputImage2 *image2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 1, const_cast< TInputImage2 * >( image2 ) );
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput2(const DecoratedInput2ImagePixelType * input2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 1, const_cast< DecoratedInput2ImagePixelType * >( input2 ) );
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput2(const Input2ImagePixelType & input2)
{
  itkDebugMacro("setting input2 to " << input2);
  typename DecoratedInput2ImagePixelType::Pointer newInput = DecoratedInput2ImagePixelType::New();
  newInput->Set(input2);
  this->SetInput2(newInput);
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetConstant2(const Input2ImagePixelType & input2)
{
  this->SetInput2(input2);
}

template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
const typename BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >::Input2ImagePixelType &
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::GetConstant2() const
{
  itkDebugMacro("Getting constant 2");
  const DecoratedInput2ImagePixelType *input = dynamic_cast< const DecoratedInput2ImagePixelType * >(
      this->ProcessObject::GetInput(1) );
  if( input == NULL )
    {
    itkExceptionMacro(<<"Constant 2 is not set");
    }
  return input->Get();
}


template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::GenerateOutputInformation()
{
  const DataObject * input = NULL;
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ProcessObject::GetInput(0) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ProcessObject::GetInput(1) );

  if ( this->GetNumberOfInputs() >= 2 )
    {
    if( inputPtr1 )
      {
      input = inputPtr1;
      }
    else if( inputPtr2 )
      {
      input = inputPtr2;
      }
    else
      {
      return;
      }

    for ( unsigned int idx = 0; idx < this->GetNumberOfOutputs(); ++idx )
      {
      DataObject * output = this->GetOutput(idx);
      if ( output )
        {
        output->CopyInformation(input);
        }
      }
    }
}

/** Set up interpolator if necessary before threaded execution */
template< class TInputImage1, class TInputImage2, class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::BeforeThreadedGenerateData()
{
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ProcessObject::GetInput(0) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ProcessObject::GetInput(1) );
  //
  // Physical space computation only matters if we're using two
  // images, and not an image and a constant.
  if(inputPtr1.IsNotNull() && inputPtr2.IsNotNull())
    {
    //
    // if the physical spaces are the same, then you can work in index space.
    this->m_PhysicalSpacesMatch = inputPtr1->GetOrigin() == inputPtr2->GetOrigin() &&
      inputPtr1->GetLargestPossibleRegion() == inputPtr2->GetLargestPossibleRegion() &&
      inputPtr1->GetSpacing() == inputPtr2->GetSpacing() &&
      inputPtr1->GetDirection() == inputPtr2->GetDirection();
    //
    // don't bother allocating an interpolator if it isn't going to be used.
    if(this->m_UsePhysicalSpace && !this->m_PhysicalSpacesMatch)
      {
      if(this->m_Interpolator.IsNull())
        {
        this->m_Interpolator =
          this->NewDefaultInterpolator(static_cast<typename TInputImage2::PixelType *>(0));
        }
      this->m_Interpolator->SetInputImage(inputPtr2);
      }
    }
}

/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template< class TInputImage1, class TInputImage2, class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ProcessObject::GetInput(0) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ProcessObject::GetInput(1) );
  OutputImagePointer outputPtr = this->GetOutput(0);

  if( inputPtr1 && inputPtr2 )
    {
    ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);
    outputIt.GoToBegin();
    //
    // simple case -- either default index space mode, or
    // all physical spaces match
    if(!this->m_UsePhysicalSpace || this->m_PhysicalSpacesMatch)
      {
      ImageRegionConstIterator< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
      ImageRegionConstIterator< TInputImage2 > inputIt2(inputPtr2, outputRegionForThread);


      ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

      inputIt1.GoToBegin();
      inputIt2.GoToBegin();

      while ( !inputIt1.IsAtEnd() )
        {
        outputIt.Set( m_Functor( inputIt1.Get(), inputIt2.Get() ) );
        ++inputIt2;
        ++inputIt1;
        ++outputIt;
        progress.CompletedPixel(); // potential exception thrown here
        }
      }
    else
      {
      ImageRegionConstIteratorWithIndex< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
      inputIt1.GoToBegin();
      while( !inputIt1.IsAtEnd())
        {
        typename TInputImage1::IndexType index1 = inputIt1.GetIndex();
        typename TInputImage2::PointType pt;
        inputPtr1->TransformIndexToPhysicalPoint(index1,pt);
        typename InterpolatorType::OutputType interpVal;
        if(this->m_Interpolator->IsInsideBuffer(pt))
          {
          interpVal = this->m_Interpolator->Evaluate(pt);
          }
        else
          {
          interpVal = this->m_DefaultValue;
          }
        outputIt.Set( this->m_Functor(inputIt1.Get(), interpVal ) );
        ++inputIt1;
        ++outputIt;
        }
      }
    }
  else if( inputPtr1 )
    {
    ImageRegionConstIterator< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
    ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);
    const Input2ImagePixelType & input2Value = this->GetConstant2();
    ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

    inputIt1.GoToBegin();
    outputIt.GoToBegin();

    while ( !inputIt1.IsAtEnd() )
      {
      outputIt.Set( m_Functor( inputIt1.Get(), input2Value ) );
      ++inputIt1;
      ++outputIt;
      progress.CompletedPixel(); // potential exception thrown here
      }
    }
  else if( inputPtr2 )
    {
    ImageRegionConstIterator< TInputImage2 > inputIt2(inputPtr2, outputRegionForThread);
    ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);
    const Input1ImagePixelType & input1Value = this->GetConstant1();
    ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

    inputIt2.GoToBegin();
    outputIt.GoToBegin();

    while ( !inputIt2.IsAtEnd() )
      {
      outputIt.Set( m_Functor( input1Value, inputIt2.Get() ) );
      ++inputIt2;
      ++outputIt;
      progress.CompletedPixel(); // potential exception thrown here
      }
    }
  else
    {
    itkGenericExceptionMacro(<<"At most one of the inputs can be a constant.");
    }
}
} // end namespace itk

#endif
