/*=========================================================================
 *
 *  Copyright SINAPSE: Scalable Informatics for Neuroscience, Processing and Software Engineering
 *            The University of Iowa
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
/*
 *  itkResampleInPlaceImageFilter.hxx
 *
 *
 *  Created by Wei Lu on 10/14/10.
 *
 */

#ifndef __itkResampleInPlaceImageFilter_hxx
#define __itkResampleInPlaceImageFilter_hxx

#include "itkResampleInPlaceImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage>
ResampleInPlaceImageFilter<TInputImage, TOutputImage>::ResampleInPlaceImageFilter()
  : m_OutputImage(nullptr)
  , m_RigidTransform(nullptr)
{
  this->SetNumberOfRequiredInputs(1);
}

/**
 * Set/Get input image, required
 */
template <typename TInputImage, typename TOutputImage>
void
ResampleInPlaceImageFilter<TInputImage, TOutputImage>::SetInputImage(const InputImageType * image)
{
  this->SetInput(0, image);
}

template <typename TInputImage, typename TOutputImage>
const typename ResampleInPlaceImageFilter<TInputImage, TOutputImage>::InputImageType *
ResampleInPlaceImageFilter<TInputImage, TOutputImage>::GetInputImage() const
{
  return this->GetInput(0);
}

/**
 * GenerateData Performs the in-place resampling
 */
template <typename TInputImage, typename TOutputImage>
void
ResampleInPlaceImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  if (!this->GetInput())
  {
    itkExceptionMacro(<< "Input image has not been connected");
    return;
  }

  //  SEE HEADER FILE FOR MATH DESCRIPTION

  {
    /** make a cast copied version of the input image **/
    using DuplicatorType = CastImageFilter<InputImageType, OutputImageType>;
    typename DuplicatorType::Pointer CastFilter = DuplicatorType::New();
    CastFilter->SetInput(this->GetInput());
    CastFilter->Update();
    m_OutputImage = CastFilter->GetOutput();
  }

  RigidTransformConstPointer                    FMTxfm = this->m_RigidTransform.GetPointer();
  const typename RigidTransformType::MatrixType inverseRotation(FMTxfm->GetMatrix().GetInverse());

  // Modify the origin and direction info of the image to reflect the transform.
  itk::Vector<double, 3> newOriginVector =
    inverseRotation * (this->GetInput()->GetOrigin().GetVectorFromOrigin() - FMTxfm->GetCenter().GetVectorFromOrigin() -
                       FMTxfm->GetTranslation()) // NewOrigin = [R^-1] * ( O - C - T ) + C
    + FMTxfm->GetCenter().GetVectorFromOrigin();
  itk::Point<double, 3> newOriginPoint;
  for (int i = 0; i < 3; ++i)
  {
    newOriginPoint[i] = newOriginVector[i];
  }
  m_OutputImage->SetOrigin(newOriginPoint);
  m_OutputImage->SetDirection(inverseRotation * this->GetInput()->GetDirection()); // NewDC = [R^-1][DC]

  this->GraftOutput(m_OutputImage);
}

template <typename TInputImage, typename TOutputImage>
void
ResampleInPlaceImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Input transform: " << m_RigidTransform << std::endl;
  os << indent << "Output image: " << m_OutputImage << std::endl;
}
} // end namespace itk

#endif
