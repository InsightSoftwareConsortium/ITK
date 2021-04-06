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
 *  itkResampleInPlaceImageFilter.h
 *
 *
 *  Created by Wei Lu on 10/14/10.
 *
 */

#ifndef __itkResampleInPlaceImageFilter_h
#define __itkResampleInPlaceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ResampleInPlaceImageFilter
 * \brief Resample an image in place.
 *
 * The current ITK resample image filter will generate a physical memory-
 * modified version of the input image if the input transform is not identity. The
 * abuse use of the filter can be cumbersome in a situation that the image is very
 * large, and there are lots of transform to be superimposed for the input image, and
 * we even don't care about those intermediate transformed images.
 *
 * If all the transforms are rigid, a far superior way to achieve a similar result
 * while minimizing the accumulative resampling errors as well as eliminating the expense
 * on accessing the physical memory of the image is to compose all the
 * transforms before hand if it is possible, and then we only need to resample the
 * input image with the final composed transform once.
 *
 * Here we present a more compact alternative, all information is stored in the header
 * of the image and there is no need to maintain the final transform any longer. ITK
 * image class has innate support for doing this.
 *
 * \param RigidTransform -- Currently must be a VersorRigid3D
 * \param InputImage -- The image to be duplicated and modified to incorporate the
 * rigid transform
 * \return -- An image with the same voxels values as the input, but with differnt
 * physical space representation affected by the rigid transform.
 *
 * The purpose of this code is to generate the new origin and direction
 * that will remove the need for using the transform.
 *
 * Given a set of PhysFixedImagePoints (i.e. from the fixedImage space)
 * those points are converted to PhysMovingImagePoints = TfmF2M( PhysFixedImagePoints )
 * and then MovingContinuousIndexPoints = movingImage->TransformPhysicalPointToContinuousIndex( PhysMovingImagePoints )
 * to get image values.
 *
 * We desire to change the moving image DirectionCosign [DC] and Origin O such that
 * we can compute the:
 * MovingContinuousIndexPoints = newMovingImage->TransformPhysicalPointToContinuousIndex( PhysFixedImagePoints )
 *
 *Image Notations:
 *  DC-DirectionCosign
 *  O-Origin
 *
 *Rigid Transform Notations:
 *  R-Rotation
 *  C-Center Of Rotation
 *  T-Translation
 *
 * TransformPhysicalPointToContinuousIndex:
 * CI = [SP^-1][DC^-1]( PhysMovingImagePoints - O )
 * PhysMovingImagePoints = [R](PhysFixedImagePoints - C) + C + T
 *
 * After substitutions:
 * MovingContinuousIndexPoints = [R^-1][DC][SP] * CI + [R^-1] * O - [R^-1] * C - [R^-1]*T + C
 *                               ----------            --------------------------------------
 *                                 NewDC                    NewOrigin
 * NewDC = [R^-1][DC]
 * NewOrigin = [R^-1] * ( O - C - T ) + C
 *
 * \ingroup GeometricTransforms
 */
template <typename TInputImage, typename TOutputImage>
class ResampleInPlaceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ResampleInPlaceImageFilter);

  /** Standard class type alias */
  using Self = ResampleInPlaceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ResampleInPlaceImageFilter, ImageToImageFilter);

  /** input/output image type alias */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImagePointType = typename InputImageType::PointType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
#endif

  /** Transform type alias */
  using RigidTransformType = VersorRigid3DTransform<double>;
  using RigidTransformConstPointer = typename RigidTransformType::ConstPointer;

  /** Set/Get rigid transform. The default is an identity transform */
  itkSetConstObjectMacro(RigidTransform, RigidTransformType);
  itkGetConstObjectMacro(RigidTransform, RigidTransformType);

  /** Set/Get required input image. (A wrapper to this->Set/GetInput()) */
  void
  SetInputImage(const InputImageType * image);

  const InputImageType *
  GetInputImage() const;

protected:
  ResampleInPlaceImageFilter();
  ~ResampleInPlaceImageFilter() override = default;
  ;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  OutputImagePointer         m_OutputImage;
  RigidTransformConstPointer m_RigidTransform;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkResampleInPlaceImageFilter.hxx"
#endif

#endif
