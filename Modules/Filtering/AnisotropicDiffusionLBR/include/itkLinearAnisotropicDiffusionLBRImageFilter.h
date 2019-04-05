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

#ifndef itkLinearAnisotropicDiffusionLBRImageFilter_h
#define itkLinearAnisotropicDiffusionLBRImageFilter_h


#include "itkImageToImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \class LinearAnisotropicDiffusionLBRImageFilter
 *
 * \brief Anisotropic diffusion using lattice basis reduction.
 *
 * \f[\partial_t u = {\rm div} (D \nabla u),\f]
 *
 * with Neumann boundary conditions. The numerical scheme is stable and
 * satisfies the maximum principle, even for strongly anisotropic tensors,
 * thanks to an adaptive discretization using arithmetic techniques
 * (Lattice Basis Reduction, LBR).
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template< typename TImage, typename TScalar = typename NumericTraits< typename TImage::PixelType >::RealType >
class LinearAnisotropicDiffusionLBRImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LinearAnisotropicDiffusionLBRImageFilter);

  /** Standard class type alias. */
  using Self = LinearAnisotropicDiffusionLBRImageFilter;
  using Superclass = ImageToImageFilter< TImage, TImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LinearAnisotropicDiffusionLBRImageFilter, ImageToImageFilter);

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;

  using ImageDimensionType = typename ImageType::ImageDimensionType;
  static constexpr ImageDimensionType ImageDimension = ImageType::ImageDimension;

  using ScalarType = TScalar;
  using TensorType = SymmetricSecondRankTensor< ScalarType, ImageDimension >;
  using TensorImageType = Image< TensorType, ImageDimension >;
  using RegionType = ImageRegion< ImageDimension >;

  void SetInputImage(const ImageType* image);
  void SetInputTensor(const TensorImageType* tensorImage);

  void SetMaxDiffusionTime(ScalarType time);
  itkGetConstMacro(DiffusionTime, ScalarType);

  void SetMaxNumberOfTimeSteps(int n);
  itkGetConstMacro(MaxNumberOfTimeSteps, int);

  void SetRatioToMaxStableTimeStep(ScalarType ratio);
  itkGetConstMacro(RatioToMaxStableTimeStep, ScalarType);

  itkGetConstMacro(EffectiveDiffusionTime, ScalarType);
  itkGetConstMacro(EffectiveNumberOfTimeSteps, int);

protected:
  LinearAnisotropicDiffusionLBRImageFilter();
  ~LinearAnisotropicDiffusionLBRImageFilter() override = default;

  typename ImageType::ConstPointer GetInputImage();
  typename TensorImageType::ConstPointer GetInputTensor();

  using IndexType = Index<ImageDimension>;

  // ******* Containers for the stencils used in the discretization
  static const unsigned int HalfStencilSize = (ImageDimension == 2) ? 3 : 6;
  static const unsigned int StencilSize = 2 * HalfStencilSize;

  using StencilCoefficientsType = Vector<ScalarType,HalfStencilSize>;
  using OffsetType = Offset<ImageDimension>;
  using StencilOffsetsType = Vector<OffsetType, HalfStencilSize>;

  using InternalSizeT = int;
  using StencilBufferIndicesType = Vector<InternalSizeT,StencilSize>;


  // *************** Computation *****************
  void GenerateData() override;
  virtual void GenerateStencils(); /// Automatically called by GenerateData
  virtual void ImageUpdateLoop(); /// Automatically called by GenerateData

  using StencilType = std::pair< StencilBufferIndicesType, StencilCoefficientsType >;
  using StencilImageType = Image< StencilType, ImageDimension >;
  typename StencilImageType::Pointer m_StencilImage;

  using ScalarImageType = Image<ScalarType, ImageDimension>;
  typename ScalarImageType::Pointer m_DiagonalCoefficients;

  virtual ScalarType MaxStableTimeStep();

  ScalarType m_DiffusionTime;
  ScalarType m_RatioToMaxStableTimeStep;
  int        m_MaxNumberOfTimeSteps;

  ScalarType m_EffectiveDiffusionTime;
  int        m_EffectiveNumberOfTimeSteps;

  virtual void ImageUpdate(ScalarType delta);
  typename ImageType::Pointer m_PreviousImage;
  typename ImageType::Pointer m_NextImage;

  virtual RegionType GetRequestedRegion(){return GetInputImage()->GetRequestedRegion();}

  InternalSizeT OutsideBufferIndex() const {return NumericTraits<InternalSizeT>::max();}

  struct StencilFunctor;
  struct FunctorType;

  using VectorType = Vector<ScalarType, ImageDimension>;
  static ScalarType ScalarProduct(const TensorType &, const VectorType &, const VectorType &);

};
} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearAnisotropicDiffusionLBRImageFilter.hxx"
#endif

#endif
