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
#ifndef itkHausdorffDistanceImageFilter_h
#define itkHausdorffDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 *\class HausdorffDistanceImageFilter
 * \brief Computes the Hausdorff distance between the set of
 * non-zero pixels of two images.
 *
 *
 * HausdorffDistanceImageFilter computes the distance between the set
 * non-zero pixels of two images using the following formula:
 * \f[ H(A,B) = \max(h(A,B),h(B,A)) \f]
 * where
 * \f[ h(A,B) = \max_{a \in A} \min_{b \in B} \| a - b\| \f] is the directed
 * Hausdorff distance
 * and \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images.
 *
 * In particular, this filter uses the DirectedHausdorffImageFilter inside to
 * compute the two directed distances and then select the largest of the two.
 *
 * The Hausdorff distance measures the degree of mismatch between two sets and
 * behaves like a metric over the set of all closed bounded sets -
 * with properties of identity, symmetry and triangle inequality.
 *
 * This filter requires the largest possible region of the first image
 * and the same corresponding region in the second image.
 * It behaves as filter with two inputs and one output. Thus it can be
 * inserted in a pipeline with other filters. The filter passes the first
 * input through unmodified.
 *
 * This filter is templated over the two input image types. It assume
 * both images have the same number of dimensions.
 *
 * \sa DirectedHausdorffDistanceImageFilter
 *
 * \ingroup MultiThreaded
 * \ingroup ITKDistanceMap
 */
template <typename TInputImage1, typename TInputImage2>
class ITK_TEMPLATE_EXPORT HausdorffDistanceImageFilter : public ImageToImageFilter<TInputImage1, TInputImage1>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HausdorffDistanceImageFilter);

  /** Standard Self type alias */
  using Self = HausdorffDistanceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage1, TInputImage1>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HausdorffDistanceImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImage1Type = TInputImage1;
  using InputImage2Type = TInputImage2;
  using InputImage1Pointer = typename TInputImage1::Pointer;
  using InputImage2Pointer = typename TInputImage2::Pointer;
  using InputImage1ConstPointer = typename TInputImage1::ConstPointer;
  using InputImage2ConstPointer = typename TInputImage2::ConstPointer;

  using RegionType = typename TInputImage1::RegionType;
  using SizeType = typename TInputImage1::SizeType;
  using IndexType = typename TInputImage1::IndexType;

  using InputImage1PixelType = typename TInputImage1::PixelType;
  using InputImage2PixelType = typename TInputImage2::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage1::ImageDimension;

  /** Type to use form computations. */
  using RealType = typename NumericTraits<InputImage1PixelType>::RealType;

  /** Set the first input. */
  void
  SetInput1(const InputImage1Type * image);

  /** Set the second input. */
  void
  SetInput2(const InputImage2Type * image);

  /** Get the first input. */
  const InputImage1Type *
  GetInput1();

  /** Get the second input. */
  const InputImage2Type *
  GetInput2();

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);

  /** Return the computed Hausdorff distance. */
  itkGetConstMacro(HausdorffDistance, RealType);
  itkGetConstMacro(AverageHausdorffDistance, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1HasNumericTraitsCheck, (Concept::HasNumericTraits<InputImage1PixelType>));
  // End concept checking
#endif

protected:
  HausdorffDistanceImageFilter();
  ~HausdorffDistanceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GenerateData. */
  void
  GenerateData() override;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces all of its output
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

private:
  RealType m_HausdorffDistance;
  RealType m_AverageHausdorffDistance;
  bool     m_UseImageSpacing;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHausdorffDistanceImageFilter.hxx"
#endif

#endif
