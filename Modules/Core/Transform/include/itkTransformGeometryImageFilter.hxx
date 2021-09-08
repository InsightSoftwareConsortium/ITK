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

  // initialize transform
}


template <typename TInputImage, typename TOutputImage>
void
TransformGeometryImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  if (!this->GetInput())
  {
    itkExceptionMacro(<< "Input image has not been connected");
    return;
  }

  typename OutputImageType::Pointer outputPtr;

  //  SEE HEADER FILE FOR MATH DESCRIPTION

  {
    /** make a cast copied version of the input image **/
    using DuplicatorType = CastImageFilter<InputImageType, OutputImageType>;
    typename DuplicatorType::Pointer CastFilter = DuplicatorType::New();
    CastFilter->SetInput(this->GetInput());
    CastFilter->Update();
    outputPtr = CastFilter->GetOutput();
  }

  RigidTransformConstPointer                    FMTxfm = this->GetRigidTransform();
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
  outputPtr->SetOrigin(newOriginPoint);
  outputPtr->SetDirection(inverseRotation * this->GetInput()->GetDirection()); // NewDC = [R^-1][DC]

  this->GraftOutput(outputPtr);
}

} // end namespace itk

#endif
