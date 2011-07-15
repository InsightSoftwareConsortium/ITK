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
#ifndef __itkWarpImageFilter_hxx
#define __itkWarpImageFilter_hxx
#include "itkWarpImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkContinuousIndex.h"
#include "vnl/vnl_math.h"
namespace itk
{
/**
 * Default constructor.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::WarpImageFilter()
{
  // Setup the number of required inputs
  this->SetNumberOfRequiredInputs(2);

  // Setup default values
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_OutputDirection.SetIdentity();
  m_OutputSize.Fill(0);
  m_EdgePaddingValue = NumericTraits< PixelType >::Zero;
  m_OutputStartIndex.Fill(0);
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
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
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
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetOutputSpacing(
  const double *spacing)
{
  SpacingType s(spacing);

  this->SetOutputSpacing(s);
}

/**
 * Set the output image origin.
 *
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetOutputOrigin(
  const double *origin)
{
  PointType p(origin);

  this->SetOutputOrigin(p);
}

/** Helper method to set the output parameters based on this image */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetOutputParametersFromImage(const ImageBaseType *image)
{
  this->SetOutputOrigin ( image->GetOrigin() );
  this->SetOutputSpacing ( image->GetSpacing() );
  this->SetOutputDirection ( image->GetDirection() );
  this->SetOutputStartIndex ( image->GetLargestPossibleRegion().GetIndex() );
  this->SetOutputSize ( image->GetLargestPossibleRegion().GetSize() );
}

/**
 * Set deformation field as Inputs[1] for this ProcessObject.
 *
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::SetDeformationField(
  const DeformationFieldType *field)
{
  // const cast is needed because the pipeline is not const-correct.
  DeformationFieldType *input =
    const_cast< DeformationFieldType * >( field );

  this->ProcessObject::SetNthInput(1, input);
}

/**
 * Return a pointer to the deformation field.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
typename WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::DeformationFieldType *
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::GetDeformationField(void)
{
  return static_cast< DeformationFieldType * >
         ( this->ProcessObject::GetInput(1) );
}

template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::VerifyInputInformation()
{
  // Default superclass implementation ensures that input images
  // occupy same physical space. This is not needed for this filter
}

/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::BeforeThreadedGenerateData()
{
  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }
  DeformationFieldPointer fieldPtr = this->GetDeformationField();

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );
  typename DeformationFieldType::RegionType defRegion =
    fieldPtr->GetLargestPossibleRegion();
  typename OutputImageType::RegionType outRegion =
    this->GetOutput()->GetLargestPossibleRegion();
  m_DefFieldSizeSame = outRegion == defRegion;
  if ( !m_DefFieldSizeSame )
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
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::AfterThreadedGenerateData()
{
  // Disconnect input image from interpolator
  m_Interpolator->SetInputImage(NULL);
}

template< class TInputImage, class TOutputImage, class TDeformationField >
typename WarpImageFilter< TInputImage,
                          TOutputImage,
                          TDeformationField >::DisplacementType
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::EvaluateDeformationAtPhysicalPoint(const PointType & point)
{
  DeformationFieldPointer fieldPtr = this->GetDeformationField();

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
  DisplacementType output;
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
      const DisplacementType input =
        fieldPtr->GetPixel(neighIndex);
      for ( unsigned int k = 0; k < DisplacementType::Dimension; k++ )
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
  return ( output );
}

/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
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
  IndexType        index;
  PointType        point;
  DisplacementType displacement;
  if ( this->m_DefFieldSizeSame )
    {
    // iterator for the deformation field
    ImageRegionIterator< DeformationFieldType >
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

      displacement = this->EvaluateDeformationAtPhysicalPoint(point);
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

template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
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
  if ( fieldPtr.IsNotNull() )
    {
    fieldPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    if ( !fieldPtr->VerifyRequestedRegion() )
      {
      fieldPtr->SetRequestedRegion( fieldPtr->GetLargestPossibleRegion() );
      }
    }
}

template< class TInputImage, class TOutputImage, class TDeformationField >
void
WarpImageFilter< TInputImage, TOutputImage, TDeformationField >
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  OutputImagePointer outputPtr = this->GetOutput();

  outputPtr->SetSpacing(m_OutputSpacing);
  outputPtr->SetOrigin(m_OutputOrigin);
  outputPtr->SetDirection(m_OutputDirection);

  DeformationFieldPointer fieldPtr = this->GetDeformationField();
  if ( this->m_OutputSize[0] == 0
       && fieldPtr.IsNotNull() )
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
