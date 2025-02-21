/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkIterativeInverseDisplacementFieldImageFilter_hxx
#define itkIterativeInverseDisplacementFieldImageFilter_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
//----------------------------------------------------------------------------
// Constructor
template <typename TInputImage, typename TOutputImage>
IterativeInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::IterativeInverseDisplacementFieldImageFilter()
{
  m_NumberOfIterations = 5;
  m_StopValue = 0;
  m_Time = 0;
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
IterativeInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  const unsigned int ImageDimension = InputImageType::ImageDimension;
  TimeType           time;

  time.Start(); // time measurement

  const InputImageConstPointer inputPtr = this->GetInput(0);
  const OutputImagePointer     outputPtr = this->GetOutput(0);

  // some checks
  if (inputPtr.IsNull())
  {
    itkExceptionMacro("\n Input is missing.");
  }

  // calculate a first guess
  // (calculate negative displacement field and apply it to itself)
  const InputImagePointer negField = InputImageType::New();
  negField->SetRegions(inputPtr->GetLargestPossibleRegion());
  negField->SetOrigin(inputPtr->GetOrigin());
  negField->SetSpacing(inputPtr->GetSpacing());
  negField->SetDirection(inputPtr->GetDirection());
  negField->Allocate();

  InputConstIterator InputIt(inputPtr, inputPtr->GetRequestedRegion());

  for (InputIterator negImageIt(negField, negField->GetRequestedRegion()); !negImageIt.IsAtEnd(); ++negImageIt)
  {
    negImageIt.Set(-InputIt.Get());
    ++InputIt;
  }

  outputPtr->SetRegions(inputPtr->GetRequestedRegion());
  outputPtr->SetOrigin(inputPtr->GetOrigin());
  outputPtr->SetSpacing(inputPtr->GetSpacing());
  outputPtr->SetDirection(inputPtr->GetDirection());
  outputPtr->Allocate();

  auto vectorWarper = VectorWarperType::New();
  auto VectorInterpolator = FieldInterpolatorType::New();
  vectorWarper->SetInput(negField);
  vectorWarper->SetInterpolator(VectorInterpolator);
  vectorWarper->SetOutputOrigin(inputPtr->GetOrigin());
  vectorWarper->SetOutputSpacing(inputPtr->GetSpacing());
  vectorWarper->SetOutputDirection(inputPtr->GetDirection());
  vectorWarper->SetDisplacementField(negField);
  vectorWarper->GraftOutput(outputPtr);
  vectorWarper->UpdateLargestPossibleRegion();

  // If the number of iterations is zero, just output the first guess
  // (negative deformable field applied to itself)
  if (m_NumberOfIterations == 0)
  {
    this->GraftOutput(vectorWarper->GetOutput());
  }
  else
  {
    // calculate the inverted field
    const double spacing = inputPtr->GetSpacing()[0];


    const InputImageRegionType region = inputPtr->GetLargestPossibleRegion();
    unsigned int               numberOfPoints = 1;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      numberOfPoints *= region.GetSize()[i];
    }

    ProgressReporter               progress(this, 0, inputPtr->GetLargestPossibleRegion().GetNumberOfPixels());
    OutputIterator                 OutputIt(outputPtr, outputPtr->GetRequestedRegion());
    const FieldInterpolatorPointer inputFieldInterpolator = FieldInterpolatorType::New();
    inputFieldInterpolator->SetInputImage(inputPtr);

    double smallestError = 0;
    InputIt.GoToBegin();
    OutputIt.GoToBegin();
    while (!OutputIt.IsAtEnd())
    {
      // get the output image index
      const OutputImageIndexType index = OutputIt.GetIndex();
      OutputImagePointType       originalPoint;
      outputPtr->TransformIndexToPhysicalPoint(index, originalPoint);

      int    stillSamePoint = 0;
      double step = spacing;

      // get the required displacement
      OutputImagePixelType displacement = OutputIt.Get();

      InputImagePointType newPoint;
      InputImagePointType mappedPoint;
      // compute the required input image point
      for (unsigned int j = 0; j < ImageDimension; ++j)
      {
        mappedPoint[j] = originalPoint[j] + displacement[j];
        newPoint[j] = mappedPoint[j];
      }

      // calculate the error of the last iteration
      if (inputFieldInterpolator->IsInsideBuffer(mappedPoint))
      {
        FieldInterpolatorOutputType forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);

        smallestError = 0;
        for (unsigned int j = 0; j < ImageDimension; ++j)
        {
          smallestError += Math::sqr(mappedPoint[j] + forwardVector[j] - originalPoint[j]);
        }
        smallestError = std::sqrt(smallestError);
      }

      // iteration loop
      for (unsigned int i = 0; i < m_NumberOfIterations; ++i)
      {


        if (stillSamePoint)
        {
          step = step / 2;
        }

        for (unsigned int k = 0; k < ImageDimension; ++k)
        {
          mappedPoint[k] += step;
          if (inputFieldInterpolator->IsInsideBuffer(mappedPoint))
          {
            FieldInterpolatorOutputType forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);
            double                      tmp = 0;
            for (unsigned int l = 0; l < ImageDimension; ++l)
            {
              tmp += Math::sqr(mappedPoint[l] + forwardVector[l] - originalPoint[l]);
            }
            tmp = std::sqrt(tmp);
            if (tmp < smallestError)
            {
              smallestError = tmp;
              for (unsigned int l = 0; l < ImageDimension; ++l)
              {
                newPoint[l] = mappedPoint[l];
              }
            }
          }

          mappedPoint[k] -= 2 * step;
          if (inputFieldInterpolator->IsInsideBuffer(mappedPoint))
          {
            FieldInterpolatorOutputType forwardVector = inputFieldInterpolator->Evaluate(mappedPoint);
            double                      tmp = 0;
            for (unsigned int l = 0; l < ImageDimension; ++l)
            {
              tmp += Math::sqr(mappedPoint[l] + forwardVector[l] - originalPoint[l]);
            }
            tmp = std::sqrt(tmp);
            if (tmp < smallestError)
            {
              smallestError = tmp;
              for (unsigned int l = 0; l < ImageDimension; ++l)
              {
                newPoint[l] = mappedPoint[l];
              }
            }
          }

          mappedPoint[k] += step;
        } // end for loop over image dimension

        stillSamePoint = 1;
        for (unsigned int j = 0; j < ImageDimension; ++j)
        {
          if (Math::NotExactlyEquals(newPoint[j], mappedPoint[j]))
          {
            stillSamePoint = 0;
          }
          mappedPoint[j] = newPoint[j];
        }

        if (smallestError < m_StopValue)
        {
          break;
        }
      } // end iteration loop
      OutputImagePixelType outputValue;
      for (unsigned int k = 0; k < ImageDimension; ++k)
      {
        outputValue[k] = static_cast<OutputImageValueType>(mappedPoint[k] - originalPoint[k]);
      }

      OutputIt.Set(outputValue);

      ++InputIt;
      ++OutputIt;

      progress.CompletedPixel();
    } // end while loop
  } // end else

  time.Stop();
  m_Time = time.GetMean();
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
IterativeInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                   Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Stop value:           " << m_StopValue << " mm" << std::endl;
  os << indent << "Elapsed time:         " << m_Time << " sec" << std::endl;
  os << std::endl;
}
} // end namespace itk

#endif
