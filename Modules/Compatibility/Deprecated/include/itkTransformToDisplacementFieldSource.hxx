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
#ifndef itkTransformToDisplacementFieldSource_hxx
#define itkTransformToDisplacementFieldSource_hxx

#include "itkTransformToDisplacementFieldSource.h"

#include "itkIdentityTransform.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TOutputImage, typename TTransformPrecisionType >
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::TransformToDisplacementFieldSource()
{
  this->m_OutputSpacing.Fill(1.0);
  this->m_OutputOrigin.Fill(0.0);
  this->m_OutputDirection.SetIdentity();

  SizeType size;
  size.Fill(0);
  this->m_OutputRegion.SetSize(size);

  IndexType index;
  index.Fill(0);
  this->m_OutputRegion.SetIndex(index);

  this->m_Transform =
    IdentityTransform< TTransformPrecisionType, ImageDimension >::New();
} // end Constructor

/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputRegion: " << this->m_OutputRegion << std::endl;
  os << indent << "OutputSpacing: " << this->m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << this->m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << this->m_OutputDirection << std::endl;
  os << indent << "Transform: " << this->m_Transform.GetPointer() << std::endl;
} // end PrintSelf()

/**
 * Set the output image size.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::SetOutputSize(const SizeType & size)
{
  this->m_OutputRegion.SetSize(size);
}

/**
 * Get the output image size.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
const typename TransformToDisplacementFieldSource< TOutputImage,
                                                  TTransformPrecisionType >
::SizeType &
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::GetOutputSize()
{
  return this->m_OutputRegion.GetSize();
}

/**
 * Set the output image index.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::SetOutputIndex(const IndexType & index)
{
  this->m_OutputRegion.SetIndex(index);
}

/**
 * Get the output image index.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
const typename TransformToDisplacementFieldSource< TOutputImage,
                                                  TTransformPrecisionType >
::IndexType &
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::GetOutputIndex()
{
  return this->m_OutputRegion.GetIndex();
}

/**
 * Set the output image spacing.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::SetOutputSpacing(const double *spacing)
{
  SpacingType s;
  for(unsigned int i = 0; i < TOutputImage::ImageDimension; ++i)
    {
    s[i] = static_cast<SpacePrecisionType>(spacing[i]);
    }

  this->SetOutputSpacing(s);
} // end SetOutputSpacing()

/**
 * Set the output image origin.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::SetOutputOrigin(const double *origin)
{
  OriginType p(origin);

  this->SetOutputOrigin(p);
}

/** Helper method to set the output parameters based on this image */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::SetOutputParametersFromImage(const ImageBaseType *image)
{
  if ( !image )
    {
    itkExceptionMacro(<< "Cannot use a null image reference");
    }

  this->SetOutputOrigin( image->GetOrigin() );
  this->SetOutputSpacing( image->GetSpacing() );
  this->SetOutputDirection( image->GetDirection() );
  this->SetOutputRegion( image->GetLargestPossibleRegion() );
} // end SetOutputParametersFromImage()

/**
 * Set up state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be set up before ThreadedGenerateData
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::BeforeThreadedGenerateData(void)
{
  if ( !this->m_Transform )
    {
    itkExceptionMacro(<< "Transform not set");
    }
} // end BeforeThreadedGenerateData()

/**
 * ThreadedGenerateData
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType threadId)
{
  // Check whether we can use a fast path for resampling. Fast path
  // can be used if the transformation is linear. Transform respond
  // to the IsLinear() call.
  if ( this->m_Transform->IsLinear() )
    {
    this->LinearThreadedGenerateData(outputRegionForThread, threadId);
    return;
    }

  // Otherwise, we use the normal method where the transform is called
  // for computing the transformation of every point.
  this->NonlinearThreadedGenerateData(outputRegionForThread, threadId);
} // end ThreadedGenerateData()

template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::NonlinearThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType threadId)
{
  // Get the output pointer
  OutputImagePointer outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIteratorType;
  OutputIteratorType outIt(outputPtr, outputRegionForThread);

  // Define a few variables that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;         // Coordinates of output pixel
  PointType transformedPoint;    // Coordinates of transformed pixel
  PixelType deformation;         // the difference

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  // Walk the output region
  outIt.GoToBegin();
  while ( !outIt.IsAtEnd() )
    {
    // Determine the index of the current output pixel
    outputPtr->TransformIndexToPhysicalPoint(outIt.GetIndex(), outputPoint);

    // Compute corresponding input pixel position
    transformedPoint = this->m_Transform->TransformPoint(outputPoint);

    // Compute the deformation
    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      deformation[i] = static_cast< PixelValueType >(
        transformedPoint[i] - outputPoint[i] );
      }

    // Set it
    outIt.Set(deformation);

    // Update progress and iterator
    progress.CompletedPixel();
    ++outIt;
    }
} // end NonlinearThreadedGenerateData()

template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::LinearThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType threadId)
{
  // Get the output pointer
  OutputImagePointer outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageLinearIteratorWithIndex< TOutputImage > OutputIteratorType;
  OutputIteratorType outIt(outputPtr, outputRegionForThread);

  outIt.SetDirection(0);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType transformedPoint;    // Coordinates of transformed pixel
  PixelType deformation;         // the difference

  IndexType index;

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  // Determine the position of the first pixel in the scanline
  outIt.GoToBegin();
  index = outIt.GetIndex();
  outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);

  // Compute corresponding transformed pixel position
  transformedPoint = this->m_Transform->TransformPoint(outputPoint);

  // Compare with the ResampleImageFilter

  // Compute delta
  PointType outputPointNeighbour;
  PointType transformedPointNeighbour;
  typedef typename PointType::VectorType VectorType;
  VectorType delta;
  ++index[0];
  outputPtr->TransformIndexToPhysicalPoint(index, outputPointNeighbour);
  transformedPointNeighbour = this->m_Transform->TransformPoint(
    outputPointNeighbour);
  delta = transformedPointNeighbour - transformedPoint
          - ( outputPointNeighbour - outputPoint );

  // loop over the vector image
  while ( !outIt.IsAtEnd() )
    {
    // Get current point
    index = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);

    // Compute transformed point
    transformedPoint = this->m_Transform->TransformPoint(outputPoint);

    while ( !outIt.IsAtEndOfLine() )
      {
      // Compute the deformation
      for ( unsigned int i = 0; i < ImageDimension; ++i )
        {
        deformation[i] = static_cast< PixelValueType >(
          transformedPoint[i] - outputPoint[i] );
        }

      // Set it
      outIt.Set(deformation);

      // Update stuff
      progress.CompletedPixel();
      ++outIt;
      transformedPoint += delta;
      }

    outIt.NextLine();
    }
} // end LinearThreadedGenerateData()

/**
 * Inform pipeline of required output region
 */
template< typename TOutputImage, typename TTransformPrecisionType >
void
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::GenerateOutputInformation(void)
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();
  if ( !outputPtr )
    {
    return;
    }

  outputPtr->SetLargestPossibleRegion(m_OutputRegion);

  outputPtr->SetSpacing(m_OutputSpacing);
  outputPtr->SetOrigin(m_OutputOrigin);
  outputPtr->SetDirection(m_OutputDirection);
} // end GenerateOutputInformation()

/**
 * Verify if any of the components has been modified.
 */
template< typename TOutputImage, typename TTransformPrecisionType >
ModifiedTimeType
TransformToDisplacementFieldSource< TOutputImage, TTransformPrecisionType >
::GetMTime(void) const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if ( this->m_Transform )
    {
    if ( latestTime < this->m_Transform->GetMTime() )
      {
      latestTime = this->m_Transform->GetMTime();
      }
    }

  return latestTime;
} // end GetMTime()
} // end namespace itk

#endif // end #ifndef _itkTransformToDisplacementFieldSource_hxx
