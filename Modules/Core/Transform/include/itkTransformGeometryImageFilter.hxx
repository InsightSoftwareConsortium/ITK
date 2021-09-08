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

#ifndef itkTransformGeometryImageFilter_hxx
#define itkTransformGeometryImageFilter_hxx

#include "itkTransformGeometryImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{


template <typename TInputImage, typename TOutputImage>
TransformGeometryImageFilter<TInputImage, TOutputImage>::TransformGeometryImageFilter()
{
  Self::SetPrimaryInputName("InputImage");

  // "RigidTransform" required ( not numbered )
  Self::AddRequiredInputName("RigidTransform");
}


template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{

  Superclass::GenerateOutputInformation();

  OutputImageType *      outputPtr = this->GetOutput();
  const InputImageType * inputPtr = this->GetInputImage();

  //  SEE HEADER FILE FOR MATH DESCRIPTION
  RigidTransformConstPointer                    FMTxfm = this->GetRigidTransform();
  const typename RigidTransformType::MatrixType inverseRotation(FMTxfm->GetMatrix().GetInverse());

  // Modify the origin and direction info of the image to reflect the transform.
  Vector<double, 3> newOriginVector =
    inverseRotation * (inputPtr->GetOrigin().GetVectorFromOrigin() - FMTxfm->GetCenter().GetVectorFromOrigin() -
                       FMTxfm->GetTranslation()) // NewOrigin = [R^-1] * ( O - C - T ) + C
    + FMTxfm->GetCenter().GetVectorFromOrigin();
  Point<double, 3> newOriginPoint;
  for (int i = 0; i < 3; ++i)
  {
    newOriginPoint[i] = newOriginVector[i];
  }
  outputPtr->SetOrigin(newOriginPoint);
  outputPtr->SetDirection(inverseRotation * inputPtr->GetDirection()); // NewDC = [R^-1][DC]
}


template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  OutputImageType * output = this->GetOutput();

  auto input = InputImageType::New();
  input->Graft(const_cast<InputImageType *>(this->GetInput()));

  //  SEE HEADER FILE FOR MATH DESCRIPTION

  /** make a cast copied version of the input image **/
  using DuplicatorType = CastImageFilter<InputImageType, OutputImageType>;
  typename DuplicatorType::Pointer castFilter = DuplicatorType::New();
  castFilter->SetInput(input);

  castFilter->GraftOutput(this->GetOutput());
  castFilter->Update();


  // Can't use graft as it will overwrite the new meta-data
  output->SetPixelContainer(castFilter->GetOutput()->GetPixelContainer());
}

} // end namespace itk

#endif
