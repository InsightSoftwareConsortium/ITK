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
  typedef AnisotropicDiffusionLBRImageFilter   Self;
  typedef ImageToImageFilter< TImage, TImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /// Method for creation through the object factory.
  itkNewMacro(Self);
  /// Run-time type information (and related methods).
  itkTypeMacro(AnisotropicDiffusionLBRImageFilter, Superclass);

  typedef TImage                        ImageType;
  typedef typename ImageType::PixelType PixelType;
  typedef TScalar                       ScalarType;

  static const unsigned int Dimension = ImageType::ImageDimension;

  typedef SymmetricSecondRankTensor< ScalarType, Dimension > TensorType;
  typedef Image< TensorType, Dimension >                     TensorImageType;

  typedef StructureTensorImageFilter<ImageType, TensorImageType>          StructureTensorFilterType;
  typedef LinearAnisotropicDiffusionLBRImageFilter<ImageType, ScalarType> LinearDiffusionFilterType;

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

  typedef typename TensorType::EigenValuesArrayType EigenValuesArrayType;
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
  typedef std::vector< std::pair<ScalarType, int> > EffectiveTimesAndIterationsType;
  itkGetConstReferenceMacro(LinearFilterEffectiveTimesAndIterations, EffectiveTimesAndIterationsType);

protected:
  ScalarType m_NoiseScale;
  ScalarType m_FeatureScale;

  ScalarType m_RatioToMaxStableTimeStep;
  int        m_MaxTimeStepsBetweenTensorUpdates;

  AnisotropicDiffusionLBRImageFilter();
  ~AnisotropicDiffusionLBRImageFilter() ITK_OVERRIDE{}

  typename TensorImageType::Pointer m_TensorImage;

  virtual void ComputeDiffusionTensors(ImageType*);

  ScalarType m_DiffusionTime;
  bool       m_Adimensionize;

  void GenerateData() ITK_OVERRIDE;

  EffectiveTimesAndIterationsType m_LinearFilterEffectiveTimesAndIterations;

  struct DiffusionTensorFunctor;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnisotropicDiffusionLBRImageFilter.hxx"
#endif

#endif
