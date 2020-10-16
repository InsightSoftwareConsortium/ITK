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
#ifndef itkStrainImageFilter_h
#define itkStrainImageFilter_h

#include "itkCovariantVector.h"
#include "itkImageToImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkSplitComponentsImageFilter.h"

namespace itk
{

/** \class StrainImageFilter
 *
 * \brief Generate a strain field image from a displacement field image.
 *
 * Internally, a gradient filter (see SetGradientFilter()) is used to calculate
 * displacement gradient tensors.  This filter is used by default on each displacement Vector
 * component. The gradient filter should take a scalar image as input and
 * generate a CovariantVector image as output. Alternatively, if a
 * VectorGradientFilter is set, it is used instead. The VectorGradientFilter
 * should take a Vector image as input and produce a CovariantVector image on
 * each output corresponding to each Vector component.
 *
 * \tparam TInputImage The first template parameter is the input image type. It should
 * be an image of displacement vectors.
 *
 * \tparam TOperatorValueType The second template parameter defines the value
 * type used in the derivative operator (defaults to float).
 *
 * \tparam TOutputValueType The third template parameter defines the value
 * type used for output image (defaults to float).  The output image is defined
 * as a symmetric second rank tensor image whose value type is specified as this
 * third template parameter.
 *
 * Three different types of strains can be calculated, infinitesimal (default), aka
 * engineering strain, which is appropriate for small strains, Green-Lagrangian,
 * which uses a material reference system, and Eulerian-Almansi, which uses a
 * spatial reference system.  This is set with SetStrainForm().
 *
 * \sa TransformToStrainFilter
 *
 * \ingroup Strain
 *
 */
template <typename TInputImage, typename TOperatorValueType = float, typename TOutputValueType = float>
class StrainImageFilter
  : public ImageToImageFilter<
      TInputImage,
      Image<SymmetricSecondRankTensor<TOutputValueType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StrainImageFilter);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputImageType = TInputImage;
  using OutputPixelType = SymmetricSecondRankTensor<TOutputValueType, ImageDimension>;
  using OutputImageType = Image<OutputPixelType, ImageDimension>;
  using OperatorImageType = Image<TOperatorValueType, ImageDimension>;

  /** Standard class type alias. */
  using Self = StrainImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  /** Type of the filter used to calculate the gradients. */
  using GradientOutputPixelType = CovariantVector<TOperatorValueType, ImageDimension>;
  using GradientOutputImageType = Image<GradientOutputPixelType, ImageDimension>;
  using GradientFilterType = ImageToImageFilter<OperatorImageType, GradientOutputImageType>;

  /** Alternate type of filter used to calculate the gradients. */
  using VectorGradientFilterType = ImageToImageFilter<InputImageType, GradientOutputImageType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StrainImageFilter, ImageToImageFilter);

  /** Set the filter used to calculate the gradients internally. The default is
   * an itk::GradientImageFilter. */
  itkSetObjectMacro(GradientFilter, GradientFilterType);
  itkGetConstObjectMacro(GradientFilter, GradientFilterType);

  /** Set the filter used to calculate the gradients internally.  This filter
   * should take a Vector image as input and produce a CovariantVector gradient
   * image on each output corresponding to every Vector component.  If this
   * filter is non-NULL, it is used instead of the GradientFilter. */
  itkSetObjectMacro(VectorGradientFilter, VectorGradientFilterType);
  itkGetConstObjectMacro(VectorGradientFilter, VectorGradientFilterType);

  /**
   * Three different types of strains can be calculated, infinitesimal (default), aka
   * engineering strain, which is appropriate for small strains, Green-Lagrangian,
   * which uses a material reference system, and Eulerian-Almansi, which uses a
   * spatial reference system.  This is set with SetStrainForm(). */
  enum StrainFormType
  {
    INFINITESIMAL = 0,
    GREENLAGRANGIAN = 1,
    EULERIANALMANSI = 2
  };

  itkSetMacro(StrainForm, StrainFormType);
  itkGetConstMacro(StrainForm, StrainFormType);

protected:
  using OutputRegionType = typename OutputImageType::RegionType;

  StrainImageFilter();

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputRegionType & outputRegion) override;

  using InputComponentsImageFilterType = itk::SplitComponentsImageFilter<InputImageType, OperatorImageType>;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  typename InputComponentsImageFilterType::Pointer m_InputComponentsFilter;

  typename GradientFilterType::Pointer m_GradientFilter;

  typename VectorGradientFilterType::Pointer m_VectorGradientFilter;

  StrainFormType m_StrainForm;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStrainImageFilter.hxx"
#endif

#endif
