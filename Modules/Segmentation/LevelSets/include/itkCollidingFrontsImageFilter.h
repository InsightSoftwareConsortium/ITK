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
#ifndef itkCollidingFrontsImageFilter_h
#define itkCollidingFrontsImageFilter_h

#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/** \class CollidingFrontsImageFilter
 *
 * \brief Selects a region of space where two independent fronts run towards
 * each other.
 *
 * The filter can be used to quickly segment anatomical structures (e.g. for
 * level set initialization).
 *
 * The filter uses two instances of FastMarchingUpwindGradientImageFilter to
 * compute the gradients of arrival times of two wavefronts propagating from
 * two sets of seeds. The input of the filter  is used as the speed of the two
 * wavefronts. The output is the dot product between the two gradient vector
 * fields.
 *
 * The filter works on the following basic idea. In the regions where the dot
 * product between the two gradient fields is negative, the two fronts
 * propagate in opposite directions. In the regions where the dot product is
 * positive, the two fronts propagate in the same direction.  This can be used
 * to extract the region of space between two sets of points.
 *
 * If StopOnTargets is On, then each front will stop as soon as all seeds of
 * the other front have been reached. This can markedly speed up the execution
 * of the filter, since wave propagation does not take place on the complete
 * image.
 *
 * Optionally, a connectivity criterion can be applied to the resulting dot
 * product image. In this case, the only negative region in the output image is
 * the one connected to the seeds.
 *
 * \author Luca Antiga Ph.D.  Biomedical Technologies Laboratory,
 *                            Bioengineering Department, Mario Negri Institute, Italy.
 *
 * \ingroup ITKLevelSets
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT CollidingFrontsImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CollidingFrontsImageFilter);

  /** Standard class type aliases. */
  using Self = CollidingFrontsImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CollidingFrontsImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<InputPixelType>::RealType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using SpeedImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImageType = TOutputImage;
  using LevelSetImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** FastMarchingUpwindGradientImageFilter type alias. */
  using FastMarchingUpwindGradientImageFilterType =
    itk::FastMarchingUpwindGradientImageFilter<LevelSetImageType, SpeedImageType>;

  /** Typedef support of level set method types. */
  using PixelType = typename FastMarchingUpwindGradientImageFilterType::PixelType;
  using NodeType = typename FastMarchingUpwindGradientImageFilterType::NodeType;
  using NodeContainer = typename FastMarchingUpwindGradientImageFilterType::NodeContainer;
  using NodeContainerPointer = typename FastMarchingUpwindGradientImageFilterType::NodeContainerPointer;
  using GradientImageType = typename FastMarchingUpwindGradientImageFilterType::GradientImageType;
  using IndexType = typename FastMarchingUpwindGradientImageFilterType::IndexType;

  /** Set the container of Seed Points representing the first initial front.
   * Seed points are represented as a VectorContainer of LevelSetNodes. */
  void
  SetSeedPoints1(NodeContainer * points)
  {
    m_SeedPoints1 = points;
    this->Modified();
  }

  /** Get the container of Seed Points representing the first initial front. */
  NodeContainerPointer
  GetSeedPoints1()
  {
    return m_SeedPoints1;
  }

  /** Set the container of Seed Points representing the second initial front.
   * Seed points are represented as a VectorContainer of LevelSetNodes. */
  void
  SetSeedPoints2(NodeContainer * points)
  {
    m_SeedPoints2 = points;
    this->Modified();
  }

  /** Get the container of Seed Points representing the second initial front. */
  NodeContainerPointer
  GetSeedPoints2()
  {
    return m_SeedPoints2;
  }

  itkSetMacro(NegativeEpsilon, double);
  itkGetConstMacro(NegativeEpsilon, double);

  itkSetMacro(ApplyConnectivity, bool);
  itkGetConstMacro(ApplyConnectivity, bool);
  itkBooleanMacro(ApplyConnectivity);

  itkSetMacro(StopOnTargets, bool);
  itkGetConstMacro(StopOnTargets, bool);
  itkBooleanMacro(StopOnTargets);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  CollidingFrontsImageFilter();
  ~CollidingFrontsImageFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream &, Indent) const override;

private:
  NodeContainerPointer m_SeedPoints1;
  NodeContainerPointer m_SeedPoints2;

  bool m_StopOnTargets;
  bool m_ApplyConnectivity;

  double m_NegativeEpsilon;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCollidingFrontsImageFilter.hxx"
#endif

#endif
