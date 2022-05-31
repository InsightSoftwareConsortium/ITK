/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFixedPointInverseDisplacementFieldImageFilter_h
#define itkFixedPointInverseDisplacementFieldImageFilter_h


#include "itkImageToImageFilter.h"

#include "itkWarpVectorImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkTimeProbe.h"

namespace itk
{

/** \class FixedPointInverseDisplacementFieldImageFilter
 *
 * \brief Computes the inverse of a Displacement field using a fixed point iteration scheme.
 *
 * FixedPointInverseDisplacementFieldImageFilter takes a Displacement field as input and
 * computes the Displacement field that is its inverse. If the input Displacement
 * field was mapping coordinates from a space A into a space B, the output of
 * this filter will map coordinates from the space B into the space A.
 *
 * To compute the inverse of the given Displacement field, the fixed point algorithm by
 * Mingli Chen, Weiguo Lu, Quan Chen, Knneth J. Ruchala and Gusavo H. Olivera
 * described in the paper
 *
\verbatim
"A simple fixed-point approach to invert a Displacement field",
Medical Physics, vol. 35, issue 1, p. 81
\endverbatim
 * is applied.
 *
 * \author Marcel Lüthi, Computer Science Department, University of Basel
 *
 * \ingroup ImageToImageFilter
 * \ingroup FixedPointInverseDisplacementField
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT FixedPointInverseDisplacementFieldImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FixedPointInverseDisplacementFieldImageFilter);

  /** Standard class type alias. */
  using Self = FixedPointInverseDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FixedPointInverseDisplacementFieldImageFilter, ImageToImageFilter);


  /** Some type alias. */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImagePointType = typename InputImageType::PointType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImageSpacingType = typename InputImageType::SpacingType;
  using InputImageIndexType = typename InputImageType::IndexType;


  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using OutputImagePointType = typename OutputImageType::PointType;
  using OutputImageIndexType = typename OutputImageType::IndexType;
  using OutputImageValueType = typename OutputImagePixelType::ValueType;
  using OutputImageSizeType = typename OutputImageType::SizeType;
  using OutputImageSpacingType = typename OutputImageType::SpacingType;
  using OutputImageOriginPointType = typename TOutputImage::PointType;
  using TimeType = TimeProbe;


  /** Number of dimensions. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  using InputConstIterator = ImageRegionConstIterator<InputImageType>;
  using InputIterator = ImageRegionIterator<InputImageType>;
  using OutputConstIterator = ImageRegionConstIterator<OutputImageType>;
  using OutputIterator = ImageRegionIterator<OutputImageType>;

  using VectorWarperType = WarpVectorImageFilter<TOutputImage, TInputImage, TOutputImage>;
  using InterpolatorVectorType = typename NumericTraits<typename TOutputImage::PixelType>::RealType;
  using OutputVectorType = typename TOutputImage::PixelType;
  using FieldInterpolatorType = VectorLinearInterpolateImageFunction<TInputImage, double>;
  using FieldInterpolatorPointer = typename FieldInterpolatorType::Pointer;
  using FieldInterpolatorOutputType = typename FieldInterpolatorType::OutputType;


  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);


  /** Set the size of the output image. */
  itkSetMacro(Size, OutputImageSizeType);
  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, OutputImageSizeType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, OutputImageSpacingType);
  virtual void
  SetOutputSpacing(const double * values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, OutputImageSpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OutputImageOriginPointType);
  virtual void
  SetOutputOrigin(const double * values);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputImageValueType>));
  itkConceptMacro(SameDimensionCheck,
                  (Concept::SameDimension<TInputImage::ImageDimension, TOutputImage::ImageDimension>));

  /** End concept checking */
#endif

protected:
  FixedPointInverseDisplacementFieldImageFilter();
  ~FixedPointInverseDisplacementFieldImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;
  void
  GenerateOutputInformation() override;

  void
  GenerateInputRequestedRegion() override;

  unsigned int m_NumberOfIterations{ 5 };


private:
  OutputImageSizeType        m_Size;          // Size of the output image
  OutputImageSpacingType     m_OutputSpacing; // output image spacing
  OutputImageOriginPointType m_OutputOrigin;  // output image origin
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFixedPointInverseDisplacementFieldImageFilter.hxx"
#endif
#endif
