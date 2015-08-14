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
#ifndef itkExtractOrthogonalSwath2DImageFilter_hxx
#define itkExtractOrthogonalSwath2DImageFilter_hxx
#include "itkExtractOrthogonalSwath2DImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::SetSpacing(const double *spacing)
{
  unsigned int i;

  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals(spacing[i], m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < ImageDimension )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::SetSpacing(const float *spacing)
{
  unsigned int i;

  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)spacing[i], m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < ImageDimension )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template< typename TImage >
const double *
ExtractOrthogonalSwath2DImageFilter< TImage >
::GetSpacing() const
{
  return m_Spacing;
}

//----------------------------------------------------------------------------
template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::SetOrigin(const double *origin)
{
  unsigned int i;

  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals(origin[i], m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < ImageDimension )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      m_Origin[i] = origin[i];
      }
    }
}

template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::SetOrigin(const float *origin)
{
  unsigned int i;

  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)origin[i], m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < ImageDimension )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      m_Origin[i] = origin[i];
      }
    }
}

template< typename TImage >
const double *
ExtractOrthogonalSwath2DImageFilter< TImage >
::GetOrigin() const
{
  return m_Origin;
}

//----------------------------------------------------------------------------

template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::GenerateOutputInformation(void)
{
  ImagePointer outputPtr     = this->GetOutput(0);

  ImageRegionType outputRegion;
  ImageIndexType  index;

  index.Fill(0);
  outputRegion.SetSize(this->m_Size);
  outputRegion.SetIndex(index);
  outputPtr->SetLargestPossibleRegion(outputRegion);
  outputPtr->SetSpacing(this->m_Spacing);
  outputPtr->SetOrigin(this->m_Origin);
}

/**
 * GenerateData Performs the reflection
 */
template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >
::GenerateData(void)
{
  ImageConstPointer inputImagePtr = this->GetImageInput();
  PathConstPointer  inputPathPtr  = this->GetPathInput();
  ImagePointer      outputPtr     = this->GetOutput(0);

  // Generate the output image
  ImageRegionType outputRegion = outputPtr->GetRequestedRegion();

  outputPtr->SetBufferedRegion(outputRegion);
  outputPtr->Allocate();

  // support progress methods/callbacks
  ProgressReporter progress( this, 0,  outputRegion.GetNumberOfPixels() );

  typedef ImageRegionIteratorWithIndex< ImageType >           OutputIterator;
  typedef LinearInterpolateImageFunction< ImageType, double > InterpolatorType;

  ImageIndexType                      index;
  double                              orthogonalOffset;
  PathInputType                       pathInput;
  ContinuousIndex<SpacePrecisionType> continousIndex;
  PathVectorType                      pathDerivative;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetInputImage(inputImagePtr);

  // Iterate through the output image
  OutputIterator outputIt( outputPtr, outputPtr->GetRequestedRegion() );
  for ( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt )
    {
    index = outputIt.GetIndex();

    // what position along the path coresponds to this column of the swath?
    pathInput = inputPathPtr->StartOfInput()
                + double( inputPathPtr->EndOfInput() - inputPathPtr->StartOfInput() )
                * double(index[0]) / double(m_Size[0]);

    // What is the orghogonal offset from the path in the input image for this
    // particular index in the output swath image?
    // Vertically centered swath pixels lie on the path in the input image.
    orthogonalOffset = index[1] - int(m_Size[1] / 2); // use signed arithmatic

    // Make continousIndex point to the source pixel in the input image
    continousIndex = inputPathPtr->Evaluate(pathInput);
    pathDerivative = inputPathPtr->EvaluateDerivative(pathInput);
    pathDerivative.Normalize();
    continousIndex[0] -= orthogonalOffset * pathDerivative[1];
    continousIndex[1] += orthogonalOffset * pathDerivative[0];

    // set the swath pixel to the interpolated input pixel
    if ( !interpolator->IsInsideBuffer(continousIndex) )
      {
      //itkExceptionMacro(<<"Requested input index ["<<continousIndex
      //                  <<"] is not in the input image" );
      outputIt.Set(m_DefaultPixelValue);
      progress.CompletedPixel();
      continue;
      }

    // prevent small interpolation error from rounding-down integer types
    if ( NumericTraits< ImagePixelType >::is_integer )
      {
      outputIt.Set( static_cast< ImagePixelType >( 0.5
                                                   + interpolator->EvaluateAtContinuousIndex(continousIndex) ) );
      }
    else
      {
      outputIt.Set( static_cast< ImagePixelType >(
                      interpolator->EvaluateAtContinuousIndex(continousIndex) ) );
      }

    progress.CompletedPixel();
    }
}

template< typename TImage >
void
ExtractOrthogonalSwath2DImageFilter< TImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size:  " << m_Size << std::endl;
  os << indent << "DefaultPixelValue:  "
     << static_cast< typename NumericTraits< ImagePixelType >::PrintType >( m_DefaultPixelValue )
     << std::endl;
}
} // end namespace itk

#endif
