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
#ifndef itkDirectedHausdorffDistanceImageFilter_h
#define itkDirectedHausdorffDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkCompensatedSummation.h"
#include <mutex>

namespace itk
{
/**
 *\class DirectedHausdorffDistanceImageFilter
 * \brief Computes the directed Hausdorff distance between the set of
 * non-zero pixels of two images.
 *
 * DirectedHausdorffDistanceImageFilter computes the distance between the set
 * non-zero pixels of two images using the following formula:
 * \f[ h(A,B) = \max_{a \in A} \min_{b \in B} \| a - b\| \f]
 * where \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images. It identifies the point \f$ a \in A \f$
 * that is farthest from any point of \f$B\f$ and measures the distance from \f$a\f$
 * to the nearest neighbor in \f$B\f$. Note that this function is not
 * symmetric and hence is not a true distance.
 *
 * In particular, this filter uses the SignedMaurerDistanceMapImageFilter inside to
 * compute distance map from all non-zero pixels in the second image. It then
 * finds the largest distance (in pixels) within the set of all non-zero pixels in the first
 * image.  The largest distance is returned by the method GetDirectedHausdorffDistance().
 *
 * In addition, this filter computes the average Hausdorff distance.
 * This average is defined as the average of all minimum distances with any negative
 * distances set to 0 since they indicate overlapping regions.
 * By definition, the distance map computes the minimum distances.
 * However, since this filter is computed using a signed distance transform, the negative
 * distance values are first set to 0 before calculating the average.
 * The average distance is returned by the method GetAverageHausdorffDistance().
 *
 * Use HausdorffDistanceImageFilter to compute the full Hausdorff distance.
 *
 * This filter requires the largest possible region of the first image
 * and the same corresponding region in the second image.
 * It behaves as filter with
 * two input and one output. Thus it can be inserted in a pipeline with
 * other filters. The filter passes the first input through unmodified.
 *
 * This filter is templated over the two input image type. It assume
 * both image have the same number of dimensions.
 *
 * \sa MaurerDistanceMapImageFilter
 * \sa HausdorffDistanceImageFilter
 *
 * \ingroup MultiThreaded
 * \ingroup ITKDistanceMap
 */
template <typename TInputImage1, typename TInputImage2>
class ITK_TEMPLATE_EXPORT DirectedHausdorffDistanceImageFilter : public ImageToImageFilter<TInputImage1, TInputImage1>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DirectedHausdorffDistanceImageFilter);

  /** Standard Self type alias */
  using Self = DirectedHausdorffDistanceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage1, TInputImage1>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DirectedHausdorffDistanceImageFilter, ImageToImageFilter);

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

  /** Set/Get if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

  /** Return the computed directed Hausdorff distance. */
  itkGetConstMacro(DirectedHausdorffDistance, RealType);
  itkGetConstMacro(AverageHausdorffDistance, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImage1PixelType>));
  // End concept checking
#endif

protected:
  DirectedHausdorffDistanceImageFilter();
  ~DirectedHausdorffDistanceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Pass the input through unmodified. Do this by Grafting in the
   * AllocateOutputs method. */
  void
  AllocateOutputs() override;

  /** Initialize some accumulators before the threads run. */
  void
  BeforeThreadedGenerateData() override;

  /** Do final mean and variance computation from data accumulated in threads.
   */
  void
  AfterThreadedGenerateData() override;

  /** Multi-thread version GenerateData. */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;


  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces all of its output
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

private:
  using DistanceMapType = Image<RealType, Self::ImageDimension>;
  using DistanceMapPointer = typename DistanceMapType::Pointer;


  DistanceMapPointer m_DistanceMap{ nullptr };

  RealType       m_MaxDistance{ NumericTraits<RealType>::ZeroValue() };
  IdentifierType m_PixelCount{};

  using CompensatedSummationType = itk::CompensatedSummation<RealType>;
  CompensatedSummationType m_Sum;

  RealType m_DirectedHausdorffDistance{ NumericTraits<RealType>::ZeroValue() };
  RealType m_AverageHausdorffDistance{ NumericTraits<RealType>::ZeroValue() };
  bool     m_UseImageSpacing{ true };

  std::mutex m_Mutex;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDirectedHausdorffDistanceImageFilter.hxx"
#endif

#endif
