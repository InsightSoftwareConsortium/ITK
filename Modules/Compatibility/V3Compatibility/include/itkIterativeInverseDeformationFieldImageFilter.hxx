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
#ifndef itkIterativeInverseDeformationFieldImageFilter_hxx
#define itkIterativeInverseDeformationFieldImageFilter_hxx

#include "itkIterativeInverseDeformationFieldImageFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
//----------------------------------------------------------------------------
// Constructor
template< typename TInputImage, typename TOutputImage >
IterativeInverseDeformationFieldImageFilter< TInputImage, TOutputImage >::IterativeInverseDeformationFieldImageFilter()
{
  m_NumberOfIterations = 5;
  m_StopValue = 0;
  m_Time = 0;
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void IterativeInverseDeformationFieldImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  const unsigned int ImageDimension = InputImageType::ImageDimension;
  TimeType           time;

  time.Start(); //time measurement

  InputImageConstPointer inputPtr = this->GetInput(0);
  OutputImagePointer     outputPtr = this->GetOutput(0);

  // some checks
  if ( inputPtr.IsNull() )
    {
    itkExceptionMacro("\n Input is missing.");
    }

  // calculate a first guess
  // (calculate negative deformation field and apply it to itself)
  InputImagePointer negField = InputImageType::New();
  negField->SetRegions( inputPtr->GetLargestPossibleRegion() );
  negField->SetOrigin( inputPtr->GetOrigin() );
  negField->SetSpacing( inputPtr->GetSpacing() );
  negField->SetDirection( inputPtr->GetDirection() );
  negField->Allocate();

  InputConstIterator InputIt = InputConstIterator( inputPtr, inputPtr->GetRequestedRegion() );
  InputIterator      negImageIt = InputIterator( negField, negField->GetRequestedRegion() );

  for ( negImageIt.GoToBegin(); !negImageIt.IsAtEnd(); ++negImageIt )
    {
    negImageIt.Set( -InputIt.Get() );
    ++InputIt;
    }

  outputPtr->SetRegions( inputPtr->GetRequestedRegion() );
  outputPtr->SetOrigin( inputPtr->GetOrigin() );
  outputPtr->SetSpacing( inputPtr->GetSpacing() );
  outputPtr->SetDirection( inputPtr->GetDirection() );
  outputPtr->Allocate();

  typename VectorWarperType::Pointer vectorWarper = VectorWarperType::New();
  typename FieldInterpolatorType::Pointer VectorInterpolator = FieldInterpolatorType::New();
  vectorWarper->SetInput(negField);
  vectorWarper->SetInterpolator(VectorInterpolator);
  vectorWarper->SetOutputOrigin( inputPtr->GetOrigin() );
  vectorWarper->SetOutputSpacing( inputPtr->GetSpacing() );
  vectorWarper->SetOutputDirection( inputPtr->GetDirection() );
  vectorWarper->SetDeformationField(negField);
  vectorWarper->GraftOutput(outputPtr);
  vectorWarper->UpdateLargestPossibleRegion();

  // If the number of iterations is zero, just output the first guess
  // (negative deformable field applied to itself)
  if ( m_NumberOfIterations == 0 )
    {
    this->GraftOutput( vectorWarper->GetOutput() );
    }
  else
    {
    // calculate the inverted field
    InputImagePointType         mappedPoint, newPoint;
    OutputImagePointType        point, originalPoint, newRemappedPoint;
    OutputImageIndexType        index;
    OutputImagePixelType        displacement, outputValue;
    FieldInterpolatorOutputType forwardVector;
    double                      spacing = inputPtr->GetSpacing()[0];
    double                      smallestError = 0;
    int                         stillSamePoint;
    InputImageRegionType        region = inputPtr->GetLargestPossibleRegion();
    unsigned int                numberOfPoints = 1;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      numberOfPoints *= region.GetSize()[i];
      }

    ProgressReporter progress( this, 0,
                               inputPtr->GetLargestPossibleRegion().GetNumberOfPixels() );
    OutputIterator           OutputIt = OutputIterator( outputPtr, outputPtr->GetRequestedRegion() );
    FieldInterpolatorPointer inputFieldInterpolator = FieldInterpolatorType::New();
    inputFieldInterpolator->SetInputImage(inputPtr);

    InputIt.GoToBegin();
    OutputIt.GoToBegin();
    while ( !OutputIt.IsAtEnd() )
      {
      // get the output image index
      index = OutputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint(index, originalPoint);

      stillSamePoint = 0;
      double step = spacing;

      // get the required displacement
      displacement = OutputIt.Get();

      // compute the required input image point
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        mappedPoint[j] = originalPoint[j] + displacement[j];
        newPoint[j] = mappedPoint[j];
        }

      // calculate the error of the last iteration
      if ( inputFieldInterpolator->IsInsideBuffer(mappedPoint) )
        {
        forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);

        smallestError = 0;
        for ( unsigned int j = 0; j < ImageDimension; j++ )
          {
          smallestError += std::pow(mappedPoint[j] + forwardVector[j] - originalPoint[j], 2);
          }
        smallestError = std::sqrt(smallestError);
        }

      // iteration loop
      for ( unsigned int i = 0; i < m_NumberOfIterations; i++ )
        {
        double tmp;

        if ( stillSamePoint )
          {
          step = step / 2;
          }

        for ( unsigned int k = 0; k < ImageDimension; k++ )
          {
          mappedPoint[k] += step;
          if ( inputFieldInterpolator->IsInsideBuffer(mappedPoint) )
            {
            forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);
            tmp = 0;
            for ( unsigned int l = 0; l < ImageDimension; l++ )
              {
              tmp += std::pow(mappedPoint[l] + forwardVector[l] - originalPoint[l], 2);
              }
            tmp = std::sqrt(tmp);
            if ( tmp < smallestError )
              {
              smallestError = tmp;
              for ( unsigned int l = 0; l < ImageDimension; l++ )
                {
                newPoint[l] = mappedPoint[l];
                }
              }
            }

          mappedPoint[k] -= 2 * step;
          if ( inputFieldInterpolator->IsInsideBuffer(mappedPoint) )
            {
            forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);
            tmp = 0;
            for ( unsigned int l = 0; l < ImageDimension; l++ )
              {
              tmp += std::pow(mappedPoint[l] + forwardVector[l] - originalPoint[l], 2);
              }
            tmp = std::sqrt(tmp);
            if ( tmp < smallestError )
              {
              smallestError = tmp;
              for ( unsigned int l = 0; l < ImageDimension; l++ )
                {
                newPoint[l] = mappedPoint[l];
                }
              }
            }

          mappedPoint[k] += step;
          } //end for loop over image dimension

        stillSamePoint = 1;
        for ( unsigned int j = 0; j < ImageDimension; j++ )
          {
          if ( newPoint[j] != mappedPoint[j] )
            {
            stillSamePoint = 0;
            }
          mappedPoint[j] = newPoint[j];
          }

        if ( smallestError < m_StopValue )
          {
          break;
          }
        } //end iteration loop

      for ( unsigned int k = 0; k < ImageDimension; k++ )
        {
        outputValue[k] = static_cast< OutputImageValueType >( mappedPoint[k] - originalPoint[k] );
        }

      OutputIt.Set(outputValue);

      ++InputIt;
      ++OutputIt;

      progress.CompletedPixel();
      } //end while loop
    }   //end else

  time.Stop();
  m_Time = time.GetMean();
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void IterativeInverseDeformationFieldImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Stop value:           " << m_StopValue << " mm" << std::endl;
  os << indent << "Elapsed time:         " << m_Time << " sec" << std::endl;
  os << std::endl;
}
} // end namespace itk

#endif
