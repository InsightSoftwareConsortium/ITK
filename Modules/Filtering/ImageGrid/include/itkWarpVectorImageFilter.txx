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
#ifndef __itkWarpVectorImageFilter_txx
#define __itkWarpVectorImageFilter_txx
#include "itkWarpVectorImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Default constructor.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::WarpVectorImageFilter()
{
  // Setup the number of required inputs
  this->SetNumberOfRequiredInputs(2);

  // Setup default values
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_OutputDirection.SetIdentity();

  for ( unsigned int i = 0; i < PixelDimension; i++ )
    {
    m_EdgePaddingValue[i] = 0;
    }

  // Setup default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator =
    static_cast< InterpolatorType * >( interp.GetPointer() );
}

/**
 * Standard PrintSelf method.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;
  os << indent << "EdgePaddingValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_EdgePaddingValue )
     << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
}

/** Set the output image spacing. */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetOutputSpacing(const double *spacing)
{
  SpacingType s(spacing);

  this->SetOutputSpacing(s);
}

/** Set the output image origin. */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetOutputOrigin(const double *origin)
{
  PointType p(origin);

  this->SetOutputOrigin(p);
}

/** Set deformation field as Inputs[1] for this ProcessObject. */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetDeformationField(
  const DeformationFieldType *field)
{
  // const cast is needed because the pipeline is not const-correct.
  DeformationFieldType *input =
    const_cast< DeformationFieldType * >( field );

  this->ProcessObject::SetNthInput(1, input);
}

/**
 * Set deformation field as Inputs[1] for this ProcessObject
 * (non const for backward compatibility)
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetDeformationField(
  DeformationFieldType *field)
{
  this->ProcessObject::SetNthInput(1, field);
}

/**
 * Return a pointer to the deformation field.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
typename WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::DeformationFieldType *
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::GetDeformationField(void)
{
  return static_cast< DeformationFieldType * >
         ( this->ProcessObject::GetInput(1) );
}

/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::BeforeThreadedGenerateData()
{
  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );
}

/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType threadId)
{
  InputImageConstPointer  inputPtr = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();
  DeformationFieldPointer fieldPtr = this->GetDeformationField();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // iterator for the output image
  ImageRegionIteratorWithIndex< OutputImageType > outputIt(
    outputPtr, outputRegionForThread);

  // iterator for the deformation field
  ImageRegionIterator< DeformationFieldType > fieldIt(
    fieldPtr, outputRegionForThread);

  IndexType        index;
  PointType        point;
  DisplacementType displacement;
  PixelType        outputValue;

  while ( !outputIt.IsAtEnd() )
    {
    // get the output image index
    index = outputIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, point);

    // get the required displacement
    displacement = fieldIt.Get();

    // compute the required input image point
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      point[j] += displacement[j];
      }

    // get the interpolated value
    if ( m_Interpolator->IsInsideBuffer(point) )
      {
      typedef typename InterpolatorType::OutputType OutputType;
      const OutputType interpolatedValue = m_Interpolator->Evaluate(point);

      for ( unsigned int k = 0; k < PixelDimension; k++ )
        {
        outputValue[k] = static_cast< ValueType >( interpolatedValue[k] );
        }

      outputIt.Set(outputValue);
      }
    else
      {
      outputIt.Set(m_EdgePaddingValue);
      }
    ++outputIt;
    ++fieldIt;
    progress.CompletedPixel();
    }
}

template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the input image
  InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // just propagate up the output requested region for the
  // deformation field.
  DeformationFieldPointer fieldPtr = this->GetDeformationField();
  OutputImagePointer      outputPtr = this->GetOutput();
  if ( fieldPtr )
    {
    fieldPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }
}

template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpVectorImageFilter< TInputImage, TOutputImage, TDeformationField >
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  OutputImagePointer outputPtr = this->GetOutput();

  outputPtr->SetSpacing(m_OutputSpacing);
  outputPtr->SetOrigin(m_OutputOrigin);
  outputPtr->SetDirection(m_OutputDirection);

  DeformationFieldPointer fieldPtr = this->GetDeformationField();
  if ( fieldPtr )
    {
    outputPtr->SetLargestPossibleRegion( fieldPtr->
                                         GetLargestPossibleRegion() );
    }
}
} // end namespace itk

#endif
