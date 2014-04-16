/*=========================================================================

 Program:   Insight Segmentation & Registration Toolkit
 Module:    $RCSfile$
 Language:  C++
 Date:      $Date$
 Version:   $Revision$

 Copyright (c) Insight Software Consortium. All rights reserved.
 See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

 This software is distributed WITHOUT ANY WARRANTY; without even
 the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 PURPOSE.  See the above copyright notices for more information.

 =========================================================================*/
#ifndef __itkContinuousBorderWarpImageFilter_txx
#define __itkContinuousBorderWarpImageFilter_txx
#include "itkContinuousBorderWarpImageFilter.h"

namespace itk
{


/**
 * Compute the output for the region specified by outputRegionForThread.
 */
template <class TInputImage, class TOutputImage, class TDisplacementField>
void
ContinuousBorderWarpImageFilter<TInputImage, TOutputImage, TDisplacementField>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  InputImageConstPointer   inputPtr = this->GetInput();
  OutputImagePointer       outputPtr = this->GetOutput();
  DisplacementFieldPointer fieldPtr = this->GetDisplacementField();

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // iterator for the output image
  ImageRegionIteratorWithIndex<OutputImageType> outputIt(outputPtr, outputRegionForThread);

  typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;

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
    ImageRegionIterator<DisplacementFieldType> fieldIt(fieldPtr, outputRegionForThread);

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

      bool changedIndex = false;
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        if (contIndex[j] < startIndex[j])
        {
          contIndex[j] = startIndex[j];
          changedIndex = true;
        }
        if (contIndex[j] > endIndex[j])
        {
          contIndex[j] = endIndex[j];
          changedIndex = true;
        }
      }

      PixelType value = static_cast<PixelType>(this->GetInterpolator()->EvaluateAtContinuousIndex(contIndex));
      outputIt.Set(value);

      ++outputIt;
      ++fieldIt;
      progress.CompletedPixel();
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
    //    progress.CompletedPixel();
  }
}

} // end namespace itk

#endif
