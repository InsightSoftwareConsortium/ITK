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

//
//  Created by Jean-Marie Mirebeau on 05/03/2014.
//
//

#ifndef itkStructureTensorImageFilter_h
#define itkStructureTensorImageFilter_h

#include "itkCastImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \class StructureTensorImageFilter
 *
 * \brief Computes the structure tensor.
 *
 * Implementation of the structure tensor, defined by
 *
 * \f[K_\rho (\nabla u_\sigma \otimes \nabla u_\sigma),\f]
 *
 * where \f$K_\rho\f$ denotes the gaussian kernel of standard deviation \f$\rho\f$,
 * and \f$u_\sigma := K_\sigma * u\f$.
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template <typename TImage,
          typename TTensorImage = Image<SymmetricSecondRankTensor<typename TImage::PixelType, TImage::ImageDimension>,
                                        TImage::ImageDimension>>
class StructureTensorImageFilter : public ImageToImageFilter<TImage, TTensorImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StructureTensorImageFilter);

  using Self = StructureTensorImageFilter;
  using Superclass = ImageToImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /// Method for creation through the object factory.
  itkNewMacro(Self);
  /// Run-time type information (and related methods).
  itkTypeMacro(StructureTensorImageFilter, Superclass);

  using InputImageDimensionType = typename Superclass::InputImageType::ImageDimensionType;
  static constexpr InputImageDimensionType InputImageDimension = Superclass::InputImageType::ImageDimension;

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using TensorImageType = TTensorImage;
  using TensorType = typename TensorImageType::PixelType;
  using ScalarType = typename TensorType::ComponentType;
  using ScalarImageType = Image<ScalarType, InputImageDimension>;

  /// Parameter \f$\sigma\f$ of the structure tensor definition.
  itkSetMacro(NoiseScale, ScalarType);
  /// Parameter \f$\rho\f$ of the structure tensor definition.
  itkSetMacro(FeatureScale, ScalarType);
  /// Rescales all structure tensors by a common factor, so that the maximum trace is 1.
  itkSetMacro(RescaleForUnitMaximumTrace, bool);

  itkGetConstMacro(NoiseScale, ScalarType);
  itkGetConstMacro(FeatureScale, ScalarType);
  itkGetConstMacro(RescaleForUnitMaximumTrace, bool);
  itkGetConstMacro(PostRescaling, ScalarType); /// Global rescaling constant used.

protected:
  void
  GenerateData() override;

  ScalarType m_FeatureScale;
  ScalarType m_NoiseScale;
  bool       m_RescaleForUnitMaximumTrace{ false };
  ScalarType m_PostRescaling;
  bool       m_UseGradientRecursiveGaussianImageFilter{ true };

  struct DispatchBase
  {};
  template <bool>
  struct Dispatch : public DispatchBase
  {};

  void
  IntermediateFilter(const Dispatch<true> &);
  void
                                    IntermediateFilter(const Dispatch<false> &);
  typename TensorImageType::Pointer m_IntermediateResult;

  using CovariantVectorType = CovariantVector<ScalarType, InputImageDimension>;
  using CovariantImageType = Image<CovariantVectorType, InputImageDimension>;

  struct OuterFunctor
  {
    TensorType
    operator()(const CovariantVectorType & u) const
    {
      TensorType m;
      for (InputImageDimensionType i = 0; i < InputImageDimension; ++i)
      {
        for (InputImageDimensionType j = i; j < InputImageDimension; ++j)
        {
          m(i, j) = u[i] * u[j];
        }
      }
      return m;
    }
  };
  struct TraceFunctor
  {
    ScalarType
    operator()(const TensorType & t) const
    {
      return t.GetTrace();
    }
  };
  struct ScaleFunctor
  {
    ScalarType scaling;
    TensorType
    operator()(const TensorType & t) const
    {
      return t * scaling;
    }
  };

  StructureTensorImageFilter();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructureTensorImageFilter.hxx"
#endif

#endif
