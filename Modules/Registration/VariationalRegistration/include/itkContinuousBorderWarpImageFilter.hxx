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
#ifndef itkContinuousBorderWarpImageFilter_hxx
#define itkContinuousBorderWarpImageFilter_hxx
#include "itkContinuousBorderWarpImageFilter.h"

namespace itk
{


/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template <typename TInputImage, typename TOutputImage, typename TDisplacementField>
void
ContinuousBorderWarpImageFilter<TInputImage, TOutputImage, TDisplacementField>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  InputImageConstPointer     inputPtr = this->GetInput();
  OutputImagePointer         outputPtr = this->GetOutput();
  const TDisplacementField * fieldPtr = this->GetDisplacementField();

  // iterator for the output image
  ImageRegionIteratorWithIndex<OutputImageType> outputIt(outputPtr, outputRegionForThread);

  using ContinuousIndexType = typename InterpolatorType::ContinuousIndexType;

  IndexType           index;
  PointType           point;
  DisplacementType    displacement;
  ContinuousIndexType contIndex;

  IndexType startIndex = this->GetInterpolator()->GetStartIndex();
  IndexType endIndex = this->GetInterpolator()->GetEndIndex();

  NumericTraits<DisplacementType>::SetLength(displacement, ImageDimension);

  // Hack because m_DefFieldSizeSame cannot be accessed.
  if (fieldPtr->GetLargestPossibleRegion() == outputPtr->GetLargestPossibleRegion())
  {
    // iterator for the deformation field
    ImageRegionConstIterator<DisplacementFieldType> fieldIt(fieldPtr, outputRegionForThread);

    while (!outputIt.IsAtEnd())
    {
      // get the output image index
      index = outputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint(index, point);

      // get the required displacement
      displacement = fieldIt.Get();

      // compute the required input image point
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        point[j] += displacement[j];
      }

      // project point into image region
      inputPtr->TransformPhysicalPointToContinuousIndex(point, contIndex);

      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        if (contIndex[j] < startIndex[j])
        {
          contIndex[j] = startIndex[j];
        }
        if (contIndex[j] > endIndex[j])
        {
          contIndex[j] = endIndex[j];
        }
      }

      auto value = static_cast<PixelType>(this->GetInterpolator()->EvaluateAtContinuousIndex(contIndex));
      outputIt.Set(value);

      ++outputIt;
      ++fieldIt;
    }
  }
  else
  {
    itkExceptionMacro(<< "Not implemented");
    //    // get the output image index
    //    index = outputIt.GetIndex();
    //    outputPtr->TransformIndexToPhysicalPoint( index, point );
    //
    //    // get the required displacement
    //    displacement = this->EvaluateDeformationAtPhysicalPoint( point );
    //
    //    // compute the required input image point
    //    for( unsigned int j = 0; j < ImageDimension; j++ )
    //      {
    //      point[j] += displacement[j];
    //      }
    //
    //    // project point into image region
    //    fieldPtr->TransformPhysicalPointToContinuousIndex( point, contIndex );
    //    bool changedIndex = false;
    //    for( unsigned int j = 0; j < ImageDimension; j++ )
    //      {
    //      if( contIndex[j] < startContIndex[j] )
    //        {
    //        contIndex[j] = startContIndex[j];
    //        changedIndex = true;
    //        }
    //      if( contIndex[j] > endContIndex[j] )
    //        {
    //        contIndex[j] = endContIndex[j];
    //        changedIndex = true;
    //        }
    //      }
    //    if( changedIndex )
    //      {
    //      fieldPtr->TransformContinuousIndexToPhysicalPoint( contIndex, point );
    //      }
    //
    //    // get the interpolated value
    //    PixelType value = static_cast< PixelType > ( this->GetInterpolator()->Evaluate( point ) );
    //    outputIt.Set( value );
    //
    //    ++outputIt;
  }
}

} // end namespace itk

#endif
