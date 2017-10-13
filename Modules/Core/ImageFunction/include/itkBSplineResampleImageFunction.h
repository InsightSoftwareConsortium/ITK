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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineResampleImageFunction_h
#define itkBSplineResampleImageFunction_h

#include "itkBSplineInterpolateImageFunction.h"

namespace itk
{
/** \class BSplineResampleImageFunction
 * \brief Resample image intensity from a BSpline coefficient image.
 *
 * This class resample the image intensity at a non-integer position
 * from the input BSpline coefficient image.
 *
 * Spline order may be from 0 to 5.
 *
 * In ITK, BSpline coefficient can be generated using a
 * BSplineDecompositionImageFilter. Using this image function in
 * conjunction with ResampleImageFunction allows the reconstruction
 * of the original image at different resolution and size.
 *
 * \sa BSplineInterpolateImageFunction
 * \sa BSplineDecompositionImageFilter
 * \sa ResampleImageFilter
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template< typename TImageType, typename TCoordRep = float >
class BSplineResampleImageFunction:
  public BSplineInterpolateImageFunction<
    TImageType, TCoordRep, typename TImageType::PixelType >
{
public:
  /** Standard class typedefs. */
  typedef BSplineResampleImageFunction Self;
  typedef BSplineInterpolateImageFunction<
    TImageType, TCoordRep, typename TImageType::PixelType >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineResampleImageFunction,
               BSplineInterpolateImageFunction);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Set the input image representing the BSplineCoefficients */
  virtual void SetInputImage(const TImageType *inputData) ITK_OVERRIDE
  {
    // bypass my superclass
    this->InterpolateImageFunction< TImageType, TCoordRep >::SetInputImage(inputData);
    this->m_Coefficients = inputData;
    if ( this->m_Coefficients.IsNotNull() )
      {
      this->m_DataLength = this->m_Coefficients->GetBufferedRegion().GetSize();
      }
  }

protected:
  BSplineResampleImageFunction() {}
  virtual ~BSplineResampleImageFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineResampleImageFunction);
};
} // namespace itk

#endif
