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
#ifndef itkTransformToDisplacementFieldFilter_hxx
#define itkTransformToDisplacementFieldFilter_hxx

#include "itkTransformToDisplacementFieldFilter.h"

#include "itkIdentityTransform.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"

namespace itk
{

template< typename TOutputImage, typename TParametersValueType>
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::TransformToDisplacementFieldFilter():
  m_UseReferenceImage( false )
{
  this->m_OutputSpacing.Fill(1.0);
  this->m_OutputOrigin.Fill(0.0);
  this->m_OutputDirection.SetIdentity();

  this->m_Size.Fill(0);
  this->m_OutputStartIndex.Fill(0);

  this->SetNumberOfRequiredInputs( 1 );
  this->SetPrimaryInputName( "Transform" );

  //  #1 "ReferenceImage" optional
  Self::AddOptionalInputName("ReferenceImage",1);

}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Size: " << this->m_Size << std::endl;
  os << indent << "OutputStartIndex: " << this->m_OutputStartIndex << std::endl;
  os << indent << "OutputSpacing: " << this->m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << this->m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << this->m_OutputDirection << std::endl;
  os << indent << "UseReferenceImage: ";
  if( this->m_UseReferenceImage )
    {
    os << "On" << std::endl;
    }
  else
    {
    os << "Off" << std::endl;
    }
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::SetOutputSpacing(const SpacePrecisionType *spacing)
{
  SpacingType ss(spacing);
  this->SetOutputSpacing(ss);
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::SetOutputOrigin(const SpacePrecisionType *origin)
{
  OriginType pp(origin);
  this->SetOutputOrigin(pp);
}

template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::SetInput( const TransformInputType * input )
{
  if( input != itkDynamicCastInDebugMode< TransformInputType * >( this->ProcessObject::GetPrimaryInput() ) )
    {
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 0, const_cast< TransformInputType * >( input ) );
    this->Modified();
    }
}

template< typename TOutputImage, typename TParametersValueType>
const typename TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>::TransformInputType *
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::GetInput() const
{
  return itkDynamicCastInDebugMode< const TransformInputType * >( this->GetPrimaryInput() );
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::GenerateOutputInformation(void)
{
  OutputImageType * output = this->GetOutput();
  if ( !output )
    {
    return;
    }

  const ReferenceImageBaseType *referenceImage = this->GetReferenceImage();

  // Set the size of the output region
  if ( m_UseReferenceImage && referenceImage )
    {
    output->SetLargestPossibleRegion(
      referenceImage->GetLargestPossibleRegion() );
    }
  else
    {
    typename TOutputImage::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize(m_Size);
    outputLargestPossibleRegion.SetIndex(m_OutputStartIndex);
    output->SetLargestPossibleRegion(outputLargestPossibleRegion);
    }

  // Set spacing and origin
  if ( m_UseReferenceImage && referenceImage )
    {
    output->SetSpacing( referenceImage->GetSpacing() );
    output->SetOrigin( referenceImage->GetOrigin() );
    output->SetDirection( referenceImage->GetDirection() );
    }
  else
    {
    output->SetSpacing(m_OutputSpacing);
    output->SetOrigin(m_OutputOrigin);
    output->SetDirection(m_OutputDirection);
    }
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::ThreadedGenerateData( const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId )
{
  const TransformType * transform = this->GetInput()->Get();
  // Check whether we can use a fast path for resampling. Fast path
  // can be used if the transformation is linear. Transform respond
  // to the IsLinear() call.
  if ( transform->IsLinear() )
    {
    this->LinearThreadedGenerateData(outputRegionForThread, threadId);
    return;
    }

  // Otherwise, we use the normal method where the transform is called
  // for computing the transformation of every point.
  this->NonlinearThreadedGenerateData(outputRegionForThread, threadId);
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::NonlinearThreadedGenerateData( const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId )
{
  // Get the output pointer
  OutputImageType * output = this->GetOutput();
  const TransformType * transform = this->GetInput()->Get();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIteratorType;
  OutputIteratorType outIt( output, outputRegionForThread );

  // Define a few variables that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;         // Coordinates of output pixel
  PointType transformedPoint;    // Coordinates of transformed pixel
  PixelType displacement;         // the difference

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Walk the output region
  outIt.GoToBegin();
  while ( !outIt.IsAtEnd() )
    {
    // Determine the index of the current output pixel
    output->TransformIndexToPhysicalPoint( outIt.GetIndex(), outputPoint );

    // Compute corresponding input pixel position
    transformedPoint = transform->TransformPoint( outputPoint );

    displacement = transformedPoint - outputPoint;

    // Set it
    outIt.Set( displacement );

    // Update progress and iterator
    progress.CompletedPixel();
    ++outIt;
    }
}


template< typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter< TOutputImage, TParametersValueType>
::LinearThreadedGenerateData( const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId )
{
  // Get the output pointer
  OutputImageType * output = this->GetOutput();
  const TransformType * transform = this->GetInput()->Get();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageLinearIteratorWithIndex< TOutputImage > OutputIteratorType;
  OutputIteratorType outIt( output, outputRegionForThread );

  outIt.SetDirection(0);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType transformedPoint;    // Coordinates of transformed pixel
  PixelType displacement;         // the difference

  IndexType index;

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Determine the position of the first pixel in the scanline
  outIt.GoToBegin();
  index = outIt.GetIndex();
  output->TransformIndexToPhysicalPoint( index, outputPoint );

  // Compute corresponding transformed pixel position
  transformedPoint = transform->TransformPoint( outputPoint );

  // Compare with the ResampleImageFilter

  // Compute delta
  PointType outputPointNeighbour;
  PointType transformedPointNeighbour;
  typedef typename PointType::VectorType VectorType;
  VectorType delta;
  ++index[0];
  output->TransformIndexToPhysicalPoint( index, outputPointNeighbour );
  transformedPointNeighbour = transform->TransformPoint( outputPointNeighbour );
  delta = transformedPointNeighbour - transformedPoint - ( outputPointNeighbour - outputPoint );

  // loop over the vector image
  while ( !outIt.IsAtEnd() )
    {
    // Get current point
    index = outIt.GetIndex();
    output->TransformIndexToPhysicalPoint( index, outputPoint );

    // Compute transformed point
    transformedPoint = transform->TransformPoint( outputPoint );

    while ( !outIt.IsAtEndOfLine() )
      {
      displacement = transformedPoint - outputPoint;

      // Set it
      outIt.Set( displacement );

      // Update stuff
      progress.CompletedPixel();
      ++outIt;
      transformedPoint += delta;
      }

    outIt.NextLine();
    }
}

} // end namespace itk

#endif
