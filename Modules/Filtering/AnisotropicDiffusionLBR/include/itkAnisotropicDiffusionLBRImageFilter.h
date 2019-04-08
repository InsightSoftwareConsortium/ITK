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
//
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkAnisotropicDiffusionLBRImageFilter_h
#define itkAnisotropicDiffusionLBRImageFilter_h

#include "itkLinearAnisotropicDiffusionLBRImageFilter.h"
#include "itkStructureTensorImageFilter.h"

namespace itk
{

/** \class AnisotropicDiffusionLBRImageFilter
 *
 * \brief Non-linear anisotropic diffusion using lattice basis reduction.
 *
 * This class repeatedly calls the LinearAnisotropicDiffusionLBRImageFilter,
 * with non-linear diffusion tensors built on the fly. These tensors are
 * obtained by computing the image structure tensors, and appropriately
 * modifying their eigenvalues with the method EigenValuesTransform. The
 * latter method is not implemented, and needs to be provided in a subclass,
 * such as CoherenceEnhancingDiffusionImageFilter.
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template< typename TImage, typename TScalar = typename NumericTraits< typename TImage::PixelType >::RealType >
class AnisotropicDiffusionLBRImageFilter : public ImageToImageFilter< TImage, TImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AnisotropicDiffusionLBRImageFilter);

  using Self = AnisotropicDiffusionLBRImageFilter;
  using Superclass = ImageToImageFilter< TImage, TImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /// Method for creation through the object factory.
  itkNewMacro(Self);
  /// Run-time type information (and related methods).
  itkTypeMacro(AnisotropicDiffusionLBRImageFilter, ImageToImageFilter);

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using ScalarType = TScalar;

  using ImageDimensionType = typename ImageType::ImageDimensionType;
  static constexpr ImageDimensionType ImageDimension = ImageType::ImageDimension;

  using TensorType = SymmetricSecondRankTensor< ScalarType, ImageDimension >;
  using TensorImageType = Image< TensorType, ImageDimension >;

  using StructureTensorFilterType = StructureTensorImageFilter<ImageType, TensorImageType>;
  using LinearDiffusionFilterType = LinearAnisotropicDiffusionLBRImageFilter<ImageType, ScalarType>;

  /** Passed to a StructureTensorImageFilter. */
  itkSetMacro(NoiseScale, ScalarType);
  itkGetConstMacro(NoiseScale, ScalarType);
  itkSetMacro(FeatureScale, ScalarType);
  itkGetConstMacro(FeatureScale, ScalarType);

  /** Passed to a LinearAnisotropicDiffusion Filter. */
  itkSetMacro(RatioToMaxStableTimeStep, ScalarType);
  itkGetConstMacro(RatioToMaxStableTimeStep, ScalarType);
  itkSetMacro(MaxTimeStepsBetweenTensorUpdates, int);
  itkGetConstMacro(MaxTimeStepsBetweenTensorUpdates, int);

  itkSetMacro(DiffusionTime, ScalarType);
  itkGetConstMacro(DiffusionTime, ScalarType);

  /** If true, uses unit pixel spacing, and rescales structure
   * tensors for uni maximum trace. */
  itkSetMacro(Adimensionize, bool);
  itkGetConstMacro(Adimensionize, bool);

  using EigenValuesArrayType = typename TensorType::EigenValuesArrayType;
  /** Transformation of the Structure tensor eigenvalues into the diffusion
   * tensor eigenvalues. Needs to be overloaded in a subclass.
   * (Structure tensor eigenvalues are sorted by increasing order for convenience). */
  virtual EigenValuesArrayType EigenValuesTransform(const EigenValuesArrayType &) const
  {
    itkExceptionMacro("Undefined tensor eigenvalues transform");
  }

  virtual typename TensorImageType::Pointer GetLastTensorImage()
  {
    return m_TensorImage;
  }
  using EffectiveTimesAndIterationsType = std::vector< std::pair<ScalarType, int> >;
  itkGetConstReferenceMacro(LinearFilterEffectiveTimesAndIterations, EffectiveTimesAndIterationsType);

protected:
  ScalarType m_NoiseScale;
  ScalarType m_FeatureScale;

  ScalarType m_RatioToMaxStableTimeStep;
  int        m_MaxTimeStepsBetweenTensorUpdates;

  AnisotropicDiffusionLBRImageFilter();
  ~AnisotropicDiffusionLBRImageFilter() override = default;

  typename TensorImageType::Pointer m_TensorImage;

  virtual void ComputeDiffusionTensors(ImageType*);

  ScalarType m_DiffusionTime;
  bool       m_Adimensionize;

  void GenerateData() override;

  EffectiveTimesAndIterationsType m_LinearFilterEffectiveTimesAndIterations;

  struct DiffusionTensorFunctor;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnisotropicDiffusionLBRImageFilter.hxx"
#endif

#endif
