/*=========================================================================
 *
 *  Copyright University of Basel, All rights reserved
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
#ifndef itkFixedPointInverseDeformationFieldImageFilter_hxx
#define itkFixedPointInverseDeformationFieldImageFilter_hxx

#include "itkFixedPointInverseDeformationFieldImageFilter.h"
#include <iostream>

namespace itk
{
//----------------------------------------------------------------------------
// Constructor
template <class TInputImage, class TOutputImage>
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::FixedPointInverseDeformationFieldImageFilter()
  : m_NumberOfIterations(5)
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
template <class TInputImage, class TOutputImage>
void
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::SetOutputSpacing(const double * spacing)
{
  OutputImageSpacingType s(spacing);
  this->SetOutputSpacing(s);
}

/**
 * Set the output image origin.
 */
template <class TInputImage, class TOutputImage>
void
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::SetOutputOrigin(const double * origin)
{
  OutputImageOriginPointType p(origin);
  this->SetOutputOrigin(p);
}


//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::GenerateData()
{

  const unsigned int ImageDimension = InputImageType::ImageDimension;

  InputImageConstPointer inputPtr = this->GetInput(0);
  OutputImagePointer     outputPtr = this->GetOutput(0);


  // some checks
  if (inputPtr.IsNull())
  {
    itkExceptionMacro("\n Input is missing.");
  }
  if (!TInputImage::ImageDimension == TOutputImage::ImageDimension)
  {
    itkExceptionMacro("\n Image Dimensions must be the same.");
  }

  // The initial deformation field is simply the negative input deformation field.
  // Here we create this deformation field.
  InputImagePointer negField = InputImageType::New();
  negField->SetRegions(inputPtr->GetLargestPossibleRegion());
  negField->SetOrigin(inputPtr->GetOrigin());
  negField->SetSpacing(inputPtr->GetSpacing());
  negField->SetDirection(inputPtr->GetDirection());
  negField->Allocate();


  InputConstIterator InputIt = InputConstIterator(inputPtr, inputPtr->GetRequestedRegion());
  InputIterator      negImageIt = InputIterator(negField, negField->GetRequestedRegion());

  for (negImageIt.GoToBegin(), InputIt.GoToBegin(); !negImageIt.IsAtEnd(); ++negImageIt)
  {
    negImageIt.Set(-InputIt.Get());
    ++InputIt;
  }

  // We allocate a deformation field that holds the output
  // and initialize it to 0
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  OutputIterator       outputIt = OutputIterator(outputPtr, outputPtr->GetRequestedRegion());
  OutputImagePixelType zero_pt;
  zero_pt.Fill(0);
  for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
  {
    outputIt.Set(zero_pt);
  }


  // In the fixed point iteration, we will need to access non-grid points.
  // Currently, the best interpolator that is supported by itk for vector
  // images is the linear interpolater.
  typename FieldInterpolatorType::Pointer vectorInterpolator = FieldInterpolatorType::New();
  vectorInterpolator->SetInputImage(negField);


  // Finally, perform the fixed point iteration.
  InputImagePointType  mappedPt;
  OutputImagePointType pt;
  OutputImageIndexType index;
  OutputImagePixelType displacement;

  for (unsigned int i = 0; i <= m_NumberOfIterations; i++)
  {
    for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
    {
      index = outputIt.GetIndex();
      outputPtr->TransformIndexToPhysicalPoint(index, pt);
      displacement = outputIt.Get();
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
          outputVector[j] = val[j];
        }
        outputIt.Set(outputVector);
      }
    }
  }
}


/**
 * Inform pipeline of required output region
 */
template <class TInputImage, class TOutputImage>
void
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  OutputImagePointer outputPtr = this->GetOutput();
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
template <class TInputImage, class TOutputImage>
void
FixedPointInverseDeformationFieldImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                   Indent         indent) const
{

  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations: " << m_NumberOfIterations << std::endl;
  os << std::endl;
}

} // end namespace itk

#endif
