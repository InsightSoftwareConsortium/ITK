/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkFixedPointInverseDisplacementFieldImageFilter_hxx
#define itkFixedPointInverseDisplacementFieldImageFilter_hxx

#include "itkFixedPointInverseDisplacementFieldImageFilter.h"
#include <iostream>

namespace itk
{
//----------------------------------------------------------------------------
// Constructor
template <typename TInputImage, typename TOutputImage>
FixedPointInverseDisplacementFieldImageFilter<TInputImage,
                                              TOutputImage>::FixedPointInverseDisplacementFieldImageFilter()
{

  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    m_Size[i] = 0;
  }
}


/**
 * Set the output image spacing.
 */
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::SetOutputSpacing(const double * spacing)
{
  OutputImageSpacingType s(spacing);
  this->SetOutputSpacing(s);
}

/**
 * Set the output image origin.
 */
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::SetOutputOrigin(const double * origin)
{
  OutputImageOriginPointType p(origin);
  this->SetOutputOrigin(p);
}


//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::GenerateData()
{

  const InputImageType * inputPtr = this->GetInput(0);
  OutputImageType *      outputPtr = this->GetOutput(0);

  InputConstIterator InputIt = InputConstIterator(inputPtr, inputPtr->GetRequestedRegion());

  // We allocate a Displacement field that holds the output
  // and initialize it to 0
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  OutputIterator       outputIt = OutputIterator(outputPtr, outputPtr->GetRequestedRegion());
  OutputImagePixelType zero_pt;
  zero_pt.Fill(0);
  outputPtr->FillBuffer(zero_pt);


  // In the fixed point iteration, we will need to access non-grid points.
  // Currently, the best interpolator that is supported by itk for vector
  // images is the linear interpolater.
  typename FieldInterpolatorType::Pointer vectorInterpolator = FieldInterpolatorType::New();
  vectorInterpolator->SetInputImage(inputPtr);


  // Finally, perform the fixed point iteration.
  InputImagePointType  mappedPt;
  OutputImagePointType pt;

  for (unsigned int i = 0; i <= m_NumberOfIterations; i++)
  {
    for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
    {
      const OutputImageIndexType index = outputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint(index, pt);
      const OutputImagePixelType displacement = outputIt.Get();
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        mappedPt[j] = pt[j] + displacement[j];
      }


      if (vectorInterpolator->IsInsideBuffer(mappedPt))
      {
        InterpolatorVectorType val = vectorInterpolator->Evaluate(mappedPt);
        OutputVectorType       outputVector;
        for (unsigned int j = 0; j < ImageDimension; j++)
        {
          outputVector[j] = -val[j];
        }
        outputIt.Set(outputVector);
      }
    }
  }
}


/**
 * Inform pipeline of required output region
 */
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  OutputImageType * outputPtr = this->GetOutput();
  if (!outputPtr)
  {
    return;
  }

  // Set the size of the output region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(m_Size);
  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

  // Set spacing and origin
  outputPtr->SetSpacing(m_OutputSpacing);
  outputPtr->SetOrigin(m_OutputOrigin);

  return;
}


//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  if (!this->GetInput())
  {
    return;
  }

  // get pointers to the input and output
  auto * inputPtr = const_cast<InputImageType *>(this->GetInput());

  // Request the entire input image
  InputImageRegionType inputRegion;
  inputRegion = inputPtr->GetLargestPossibleRegion();
  inputPtr->SetRequestedRegion(inputRegion);
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
FixedPointInverseDisplacementFieldImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                    Indent         indent) const
{

  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations: " << m_NumberOfIterations << std::endl;
  os << std::endl;
}

} // end namespace itk

#endif
