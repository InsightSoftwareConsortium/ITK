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
#ifndef itkIterativeInverseDisplacementFieldImageFilter_h
#define itkIterativeInverseDisplacementFieldImageFilter_h


#include "itkWarpVectorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkTimeProbe.h"

namespace itk
{
/** \class IterativeInverseDisplacementFieldImageFilter
 * \brief Computes the inverse of a displacement field.
 *
 * IterativeInverseDisplacementFieldImageFilter takes a displacement field as input and
 * computes the displacement field that is its inverse. If the input displacement
 * field was mapping coordinates from a space A into a space B, the output of
 * this filter will map coordinates from the space B into the space A.
 *
 * The algorithm implemented in this filter uses an iterative method for
 * progressively refining the values of the inverse field. Starting from the
 * direct field, at every pixel the direct mapping of this point is found, and
 * a the negative of the current displacement is stored in the inverse field at
 * the nearest pixel. Then, subsequent iterations verify if any of the neighbor pixels
 * provide a better return to the current pixel, in which case its value is taken for
 * updating the vector in the inverse field.
 *
 * This method was discussed in the users-list during February 2004.
 *
 * \author  Corinne Mattmann
 *
 * \ingroup ITKDisplacementField
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT IterativeInverseDisplacementFieldImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IterativeInverseDisplacementFieldImageFilter);

  /** Standard class type aliases. */
  using Self = IterativeInverseDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IterativeInverseDisplacementFieldImageFilter, ImageToImageFilter);

  /** Some type alias. */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImagePointType = typename InputImageType::PointType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImageSpacingType = typename InputImageType::SpacingType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using OutputImagePointType = typename OutputImageType::PointType;
  using OutputImageIndexType = typename OutputImageType::IndexType;
  using OutputImageValueType = typename OutputImagePixelType::ValueType;

  using TimeType = TimeProbe;

  using InputConstIterator = ImageRegionConstIterator<InputImageType>;
  using InputIterator = ImageRegionIterator<InputImageType>;
  using OutputIterator = ImageRegionIterator<OutputImageType>;

  using VectorWarperType = WarpVectorImageFilter<TOutputImage, TInputImage, TOutputImage>;

  using FieldInterpolatorType = VectorLinearInterpolateImageFunction<TInputImage, double>;
  using FieldInterpolatorPointer = typename FieldInterpolatorType::Pointer;
  using FieldInterpolatorOutputType = typename FieldInterpolatorType::OutputType;

  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  // If the error (in mm) between forward and backward mapping is smaller than
  // the StopValue,
  // the algorithm stops.
  // This value can be used to speed up the calculation.
  itkSetMacro(StopValue, double);
  itkGetConstMacro(StopValue, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputImageValueType>));

  itkConceptMacro(SameDimensionCheck,
                  (Concept::SameDimension<TInputImage::ImageDimension, TOutputImage::ImageDimension>));
  // End concept checking
#endif

protected:
  IterativeInverseDisplacementFieldImageFilter();
  ~IterativeInverseDisplacementFieldImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  unsigned int m_NumberOfIterations;

  double m_StopValue;
  double m_Time;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIterativeInverseDisplacementFieldImageFilter.hxx"
#endif

#endif
