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
#ifndef itkWarpImageFilter_hxx
#define itkWarpImageFilter_hxx
#include "itkWarpImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageAlgorithm.h"
#include "itkNumericTraits.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkProgressReporter.h"
#include "itkContinuousIndex.h"
#include "itkMath.h"
namespace itk
{
/**
 * Default constructor.
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::WarpImageFilter()
{
  // #0 implicit "Primary" input required
  // #1 "DisplacementField" required
  Self::AddRequiredInputName("DisplacementField", 1);


  // Setup default values
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_OutputDirection.SetIdentity();
  m_OutputSize.Fill(0);
  m_EdgePaddingValue = NumericTraits< PixelType >::ZeroValue(m_EdgePaddingValue);
  m_OutputStartIndex.Fill(0);
  // Setup default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator =
    static_cast< InterpolatorType * >( interp.GetPointer() );

  m_DefFieldSameInformation = false;
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;
  os << indent << "OutputSize: " << m_OutputSize << std::endl;
  os << indent << "OutputStartIndex: " << m_OutputStartIndex << std::endl;
  os << indent << "EdgePaddingValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_EdgePaddingValue )
     << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
}

/**
 * Set the output image spacing.
 *
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::SetOutputSpacing(
  const double *spacing)
{
  SpacingType s;
  for(unsigned int i = 0; i < TInputImage::ImageDimension; ++i)
    {
    s[i] = static_cast< typename SpacingType::ValueType >(spacing[i]);
    }
  this->SetOutputSpacing(s);
}

/**
 * Set the output image origin.
 *
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::SetOutputOrigin(
  const double *origin)
{
  PointType p(origin);

  this->SetOutputOrigin(p);
}

/** Helper method to set the output parameters based on this image */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::SetOutputParametersFromImage(const ImageBaseType *image)
{
  this->SetOutputOrigin ( image->GetOrigin() );
  this->SetOutputSpacing ( image->GetSpacing() );
  this->SetOutputDirection ( image->GetDirection() );
  this->SetOutputStartIndex ( image->GetLargestPossibleRegion().GetIndex() );
  this->SetOutputSize ( image->GetLargestPossibleRegion().GetSize() );
}

template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::VerifyInputInformation()
{
  if (ImageDimension != GetDisplacementField()->GetNumberOfComponentsPerPixel())
    {
    itkExceptionMacro("Expected number of components of displacement field to match image dimensions!");
    }
}

/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::BeforeThreadedGenerateData()
{
  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }
  const DisplacementFieldType *fieldPtr = this->GetDisplacementField();

  unsigned int numberOfComponents = DefaultConvertPixelTraits<PixelType>::GetNumberOfComponents(m_EdgePaddingValue);

  if( numberOfComponents != this->GetInput()->GetNumberOfComponentsPerPixel() )
    {
    PixelComponentType zeroComponent = NumericTraits<PixelComponentType>::ZeroValue(zeroComponent);
    numberOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
    NumericTraits<PixelType>::SetLength(m_EdgePaddingValue,numberOfComponents);

    for( unsigned int n = 0; n < numberOfComponents; ++n )
      {
      DefaultConvertPixelTraits<PixelType>::SetNthComponent(n,m_EdgePaddingValue,zeroComponent);
      }
    }

  if( NumericTraits<PixelType>::GetLength(m_EdgePaddingValue) != this->GetInput()->GetNumberOfComponentsPerPixel() )
    {
    // Assume EdgePaddingValue has not been set externally
    // initialize it here with ZeroValue, when we know the number of components
    const PixelType& pixel = this->GetInput()->GetPixel( this->GetInput()->GetBufferedRegion().GetIndex() );
    m_EdgePaddingValue = NumericTraits<PixelType>::ZeroValue( pixel );
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );

  if ( !m_DefFieldSameInformation )
    {
    m_StartIndex = fieldPtr->GetBufferedRegion().GetIndex();
    for ( unsigned i = 0; i < ImageDimension; i++ )
      {
      m_EndIndex[i] = m_StartIndex[i]
                      + fieldPtr->GetBufferedRegion().GetSize()[i] - 1;
      }
    }
}

/**
 * Setup state of filter after multi-threading.
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::AfterThreadedGenerateData()
{
  // Disconnect input image from interpolator
  m_Interpolator->SetInputImage(ITK_NULLPTR);
}

template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::EvaluateDisplacementAtPhysicalPoint(const PointType & point, DisplacementType & output)
{
    this->EvaluateDisplacementAtPhysicalPoint(point, this->GetDisplacementField(), output);
}

template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::EvaluateDisplacementAtPhysicalPoint(
  const PointType & point,
  const DisplacementFieldType * fieldPtr,
  DisplacementType & output)
{
  ContinuousIndex< double, ImageDimension > index;
  fieldPtr->TransformPhysicalPointToContinuousIndex(point, index);
  unsigned int dim;  // index over dimension
  /**
   * Compute base index = closest index below point
   * Compute distance from point to base index
   */
  IndexType baseIndex;
  IndexType neighIndex;
  double    distance[ImageDimension];

  for ( dim = 0; dim < ImageDimension; dim++ )
    {
    baseIndex[dim] = Math::Floor< IndexValueType >(index[dim]);

    if ( baseIndex[dim] >=  m_StartIndex[dim] )
      {
      if ( baseIndex[dim] <  m_EndIndex[dim] )
        {
        distance[dim] = index[dim] - static_cast< double >( baseIndex[dim] );
        }
      else
        {
        baseIndex[dim] = m_EndIndex[dim];
        distance[dim] = 0.0;
        }
      }
    else
      {
      baseIndex[dim] = m_StartIndex[dim];
      distance[dim] = 0.0;
      }
    }

  /**
   * Interpolated value is the weight some of each of the surrounding
   * neighbors. The weight for each neighbour is the fraction overlap
   * of the neighbor pixel with respect to a pixel centered on point.
   */
  output.Fill(0);

  double       totalOverlap = 0.0;
  unsigned int numNeighbors(1 << TInputImage::ImageDimension);

  for ( unsigned int counter = 0; counter < numNeighbors; counter++ )
    {
    double       overlap = 1.0;    // fraction overlap
    unsigned int upper = counter;  // each bit indicates upper/lower neighbour

    // get neighbor index and overlap fraction
    for ( dim = 0; dim < ImageDimension; dim++ )
      {
      if ( upper & 1 )
        {
        neighIndex[dim] = baseIndex[dim] + 1;
        overlap *= distance[dim];
        }
      else
        {
        neighIndex[dim] = baseIndex[dim];
        overlap *= 1.0 - distance[dim];
        }

      upper >>= 1;
      }

    // get neighbor value only if overlap is not zero
    if ( overlap )
      {
      const DisplacementType input = fieldPtr->GetPixel(neighIndex);
      const unsigned int displacementComponent = NumericTraits<DisplacementType>::GetLength(input);
      for ( unsigned int k = 0; k < displacementComponent; ++k )
        {
        output[k] += overlap * static_cast< double >( input[k] );
        }
      totalOverlap += overlap;
      }

    if ( totalOverlap == 1.0 )
      {
      // finished
      break;
      }
    }
}

/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType threadId)
{
  OutputImageType             *outputPtr = this->GetOutput();
  const DisplacementFieldType *fieldPtr = this->GetDisplacementField();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // iterator for the output image
  ImageRegionIteratorWithIndex< OutputImageType > outputIt(
    outputPtr, outputRegionForThread);
  IndexType        index;
  PointType        point;
  DisplacementType displacement;
  NumericTraits<DisplacementType>::SetLength(displacement,ImageDimension);
  if ( this->m_DefFieldSameInformation )
    {
    // iterator for the deformation field
    ImageRegionConstIterator< DisplacementFieldType >
    fieldIt(fieldPtr, outputRegionForThread);

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
        PixelType value =
          static_cast< PixelType >( m_Interpolator->Evaluate(point) );
        outputIt.Set(value);
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
  else
    {
    while ( !outputIt.IsAtEnd() )
      {
      // get the output image index
      index = outputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint(index, point);

      this->EvaluateDisplacementAtPhysicalPoint(point, fieldPtr, displacement);
      // compute the required input image point
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        point[j] += displacement[j];
        }

      // get the interpolated value
      if ( m_Interpolator->IsInsideBuffer(point) )
        {
        PixelType value =
          static_cast< PixelType >( m_Interpolator->Evaluate(point) );
        outputIt.Set(value);
        }
      else
        {
        outputIt.Set(m_EdgePaddingValue);
        }
      ++outputIt;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the input image
  InputImageType * inputPtr =
    const_cast< InputImageType * >( this->GetInput() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // If the output and the deformation field have the same
  // information, just propagate up the output requested region for the
  // deformation field. Otherwise, it is non-trivial to determine
  // the smallest region of the deformation field that fully
  // contains the physical space covered by the output's requested
  // region, se we do the easy thing and request the largest possible region
  DisplacementFieldType *fieldPtr =
    const_cast<DisplacementFieldType *>(this->GetDisplacementField());
  const OutputImageType *outputPtr = this->GetOutput();
  if ( fieldPtr != ITK_NULLPTR )
    {
    // tolerance for origin and spacing depends on the size of pixel
    // tolerance for direction is a fraction of the unit cube.
    const SpacePrecisionType coordinateTol = this->GetCoordinateTolerance() * outputPtr->GetSpacing()[0]; // use first dimension spacing

    this->m_DefFieldSameInformation =
       (outputPtr->GetOrigin().GetVnlVector().is_equal(fieldPtr->GetOrigin().GetVnlVector(), coordinateTol))
    && (outputPtr->GetSpacing().GetVnlVector().is_equal(fieldPtr->GetSpacing().GetVnlVector(), coordinateTol))
    && (outputPtr->GetDirection().GetVnlMatrix().as_ref().is_equal(fieldPtr->GetDirection().GetVnlMatrix(), this->GetDirectionTolerance()));

    if (this->m_DefFieldSameInformation)
      {
      fieldPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
      }
    else
      {
      typedef typename TDisplacementField::RegionType DisplacementRegionType;

      DisplacementRegionType fieldRequestedRegion = ImageAlgorithm::EnlargeRegionOverBox(outputPtr->GetRequestedRegion(),
                                                                                         outputPtr,
                                                                                         fieldPtr);
      fieldPtr->SetRequestedRegion( fieldRequestedRegion );
      }
    if ( !fieldPtr->VerifyRequestedRegion() )
      {
      fieldPtr->SetRequestedRegion( fieldPtr->GetLargestPossibleRegion() );
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TDisplacementField >
void
WarpImageFilter< TInputImage, TOutputImage, TDisplacementField >
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  OutputImageType * outputPtr = this->GetOutput();

  outputPtr->SetSpacing(m_OutputSpacing);
  outputPtr->SetOrigin(m_OutputOrigin);
  outputPtr->SetDirection(m_OutputDirection);

  const DisplacementFieldType* fieldPtr = this->GetDisplacementField();
  if ( this->m_OutputSize[0] == 0
       && fieldPtr != ITK_NULLPTR )
    {
    outputPtr->SetLargestPossibleRegion( fieldPtr->
                                         GetLargestPossibleRegion() );
    }
  else
    {
    OutputImageRegionType region;
    region.SetSize(this->m_OutputSize);
    region.SetIndex(this->m_OutputStartIndex);
    outputPtr->SetLargestPossibleRegion(region);
    }
}
} // end namespace itk

#endif
