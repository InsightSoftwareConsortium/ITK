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
#ifndef itkExponentialDisplacementFieldImageFilter_h
#define itkExponentialDisplacementFieldImageFilter_h

#include "itkDivideImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkWarpVectorImageFilter.h"
#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.h"
#include "itkAddImageFilter.h"

namespace itk
{
/** \class ExponentialDisplacementFieldImageFilter
 * \brief Computes a diffeomorphic displacement field as the Lie group
 * exponential of a vector field.
 *
 * ExponentialDisplacementFieldImageFilter takes a 'smooth' vector field
 * as input and computes the displacement field that is its exponential.
 *
 * Given that both the input and output displacement field are represented as
 * discrete images with pixel type vector, the exponential will be only an
 * estimation and will probably not correspond to a perfect exponential.  The
 * precision of the exponential can be improved at the price of increasing the
 * computation time (number of iterations).
 *
 * The method used for computing the exponential displacement field is
 * an iterative scaling and squaring (cf Arsigny et al., "A
 * Log-Euclidean Framework for Statistics on Diffeomorphisms", MICCAI'06).
 *
 *    \f[
 *      exp(\Phi) = exp( \frac{\Phi}{2^N} )^{2^N}
 *    \f]
 *
 *
 * This filter expects both the input and output images to be of pixel type
 * Vector.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \ingroup ITKDisplacementField
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ExponentialDisplacementFieldImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExponentialDisplacementFieldImageFilter);

  /** Standard class type aliases. */
  using Self = ExponentialDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExponentialDisplacementFieldImageFilter, ImageToImageFilter);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputPixelType = typename InputImageType::PixelType;
  using InputPixelRealValueType = typename InputPixelType::RealValueType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Specify the maximum number of iteration. */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetConstMacro(MaximumNumberOfIterations, unsigned int);

  /** If AutomaticNumberOfIterations is off, the number of iterations is
   * given by MaximumNumberOfIterations. If it is on, we try to get
   * the lowest good number (which may not be larger than
   * MaximumNumberOfIterations ) */
  itkSetMacro(AutomaticNumberOfIterations, bool);
  itkGetConstMacro(AutomaticNumberOfIterations, bool);
  itkBooleanMacro(AutomaticNumberOfIterations);

  /** If ComputeInverse is on, the filter will compute the exponential
   * of the opposite (minus) of the input vector field. The output displacement
   * fields computed with ComputeInverse set to on and off respectively
   * therefore represent spatial transformations that are inverses of
   * each other. */
  itkSetMacro(ComputeInverse, bool);
  itkGetConstMacro(ComputeInverse, bool);
  itkBooleanMacro(ComputeInverse);

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int PixelDimension = InputPixelType::Dimension;
  static constexpr unsigned int OutputPixelDimension = OutputPixelType::Dimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename OutputPixelType::ValueType>));
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<ImageDimension, PixelDimension>));
  itkConceptMacro(SameDimensionCheck3, (Concept::SameDimension<ImageDimension, OutputPixelDimension>));
  // End concept checking
#endif

protected:
  ExponentialDisplacementFieldImageFilter();
  ~ExponentialDisplacementFieldImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * GenerateData()
   */
  void
  GenerateData() override;

  using RegionType = typename InputImageType::RegionType;

  using DivideByConstantType =
    DivideImageFilter<InputImageType, itk::Image<InputPixelRealValueType, ImageDimension>, OutputImageType>;

  using CasterType = CastImageFilter<InputImageType, OutputImageType>;

  using VectorWarperType = WarpVectorImageFilter<OutputImageType, OutputImageType, OutputImageType>;

  using FieldInterpolatorType = VectorLinearInterpolateNearestNeighborExtrapolateImageFunction<OutputImageType, double>;

  using AdderType = AddImageFilter<OutputImageType, OutputImageType, OutputImageType>;

  using DivideByConstantPointer = typename DivideByConstantType::Pointer;
  using CasterPointer = typename CasterType::Pointer;
  using VectorWarperPointer = typename VectorWarperType::Pointer;
  using FieldInterpolatorPointer = typename FieldInterpolatorType::Pointer;
  using FieldInterpolatorOutputType = typename FieldInterpolatorType::OutputType;
  using AdderPointer = typename AdderType::Pointer;

private:
  bool         m_AutomaticNumberOfIterations;
  unsigned int m_MaximumNumberOfIterations;

  bool m_ComputeInverse;

  DivideByConstantPointer m_Divider;
  CasterPointer           m_Caster;
  VectorWarperPointer     m_Warper;
  AdderPointer            m_Adder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExponentialDisplacementFieldImageFilter.hxx"
#endif

#endif
