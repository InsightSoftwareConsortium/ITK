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

#ifndef itkTransformGeometryImageFilter_hxx
#define itkTransformGeometryImageFilter_hxx

#include "itkCastImageFilter.h"

namespace itk
{


template <typename TInputImage, typename TOutputImage>
TransformGeometryImageFilter<TInputImage, TOutputImage>::TransformGeometryImageFilter()
{
  Self::SetPrimaryInputName("InputImage");

  // "RigidTransform" required ( not numbered )
  Self::AddRequiredInputName("Transform");
}

template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::VerifyPreconditions() ITKv5_CONST
{
  Superclass::VerifyPreconditions();

  TransformConstPointer tx = this->GetTransform();

  if (!tx->IsLinear())
  {
    itkExceptionMacro(<< "Transform set to non-linear transform of type: " << tx->GetNameOfClass());
  }
}


template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{

  Superclass::GenerateOutputInformation();

  OutputImageType * outputPtr = this->GetOutput();

  TransformConstPointer tx = this->GetTransform();
  tx->ApplyToImageMetadata(outputPtr);
}


template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  if (this->GetRunningInPlace())
  {
    // No need to copy the bulk data
    return;
  }
  OutputImageType * output = this->GetOutput();

  auto input = InputImageType::New();
  input->Graft(const_cast<InputImageType *>(this->GetInput()));

  //  SEE HEADER FILE FOR MATH DESCRIPTION

  /** make a cast copied version of the input image **/
  using DuplicatorType = CastImageFilter<InputImageType, OutputImageType>;
  auto castFilter = DuplicatorType::New();
  castFilter->SetInput(input);

  castFilter->GraftOutput(this->GetOutput());
  castFilter->Update();


  // Can't use graft as it will overwrite the new meta-data
  output->SetPixelContainer(castFilter->GetOutput()->GetPixelContainer());
  output->SetBufferedRegion(castFilter->GetOutput()->GetBufferedRegion());
}

} // end namespace itk

#endif
