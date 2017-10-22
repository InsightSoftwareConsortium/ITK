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
#ifndef itkMatchCardinalityImageToImageMetric_h
#define itkMatchCardinalityImageToImageMetric_h

/**
 *  TODO: This class needs to be more tightly integrated with the new
 *  multi-threaded ImageToImageMetric.
 */

#include "itkImageToImageMetric.h"
#include "itkPoint.h"
#include <vector>

namespace itk
{
/** \class MatchCardinalityImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the fixed and moving
 * images to be compared.
 *
 * This metric computes cardinality of the set of pixels that match
 * exactly between the moving and fixed images. The spatial
 * correspondance between both images is established through a
 * Transform. Pixel values are taken from the Moving image. Their
 * positions are mapped to the Fixed image and result in general in
 * non-grid position on it. Values at these non-grid position of the
 * Fixed image are interpolated using a user-selected Interpolator.
 *
 * This metric is designed for matching label maps. All pixel
 * mismatches are considered equal whether they are between label 1
 * and label 2 or between label 1 and label 500.  In other words, the
 * magnitude of an individual label mismatch is not relevant, or the
 * occurrence of a label mismatch is important.
 *
 * Given the nature of label maps, a nearest neighbor interpolator is
 * the preferred interpolator.
 *
 * The metric measure can measure the number of pixel matches (pixels
 * with exactly the same label) or pixel mismatches (pixels with
 * different labels). The returned metric value is the number of pixel
 * matches (or mismatches) normalized by the number of pixels
 * considered. The number of pixel considered is a function of the
 * number of pixels in the overlap of the fixed and moving image
 * buffers conditional on any assigned masks.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedImage, typename TMovingImage >
class ITK_TEMPLATE_EXPORT MatchCardinalityImageToImageMetric:
  public ImageToImageMetric< TFixedImage, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef MatchCardinalityImageToImageMetric              Self;
  typedef ImageToImageMetric< TFixedImage, TMovingImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MatchCardinalityImageToImageMetric, ImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::FixedImageType          FixedImageType;
  typedef typename Superclass::MovingImageType         MovingImageType;
  typedef typename Superclass::FixedImageConstPointer  FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer MovingImageConstPointer;
  typedef typename Superclass::FixedImageRegionType    FixedImageRegionType;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType &,
                     DerivativeType & derivative) const ITK_OVERRIDE
  {
    itkWarningMacro(<< "This metric does not provide metric derivatives.");
    derivative.Fill(NumericTraits< typename DerivativeType::ValueType >::ZeroValue());
  }

  /**  Get the value of the metric at a particular parameter
   *  setting. The metric value is the number of pixel matches (or
   *  mis-matches, see SetMeasureMatches()) normalized by the number
   *  of pixels under consideration (within the buffer and if
   *  specified within a mask). In other words, the metric measure the
   *  percentage of pixel matches or mismatches. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /** Set/Get whether this metric measures pixel matches or pixel
   * mismatches. Note the GetValue() returns the number of matches (or
   * mismatches) normalized by the number of pixels considered. In
   * other words, the metric measures the percentage of pixel matches
   * or mismatches. The default is to measure matches
   * (MeasureMatchesOn). */
  itkSetMacro(MeasureMatches, bool);
  itkBooleanMacro(MeasureMatches);
  itkGetConstMacro(MeasureMatches, bool);

  /** Return the multithreader used by this class. */
  MultiThreader * GetMultiThreader()
  { return m_Threader; }

protected:
  MatchCardinalityImageToImageMetric();
  virtual ~MatchCardinalityImageToImageMetric() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Non-const version of GetValue().  This is a hack around various
   * const issues with trying to spawn threads from the const version
   * of GetValue().
   */
  MeasureType GetNonconstValue(const TransformParametersType & parameters);

  /**
   * Thread worker routine to calculate the contribution of the a
   * subregion to the overall metric.  Can only be called from
   * GetValue(). */
  virtual
  void ThreadedGetValue(const FixedImageRegionType & outputRegionForThread,
                        ThreadIdType threadId);

  /** Split the FixedImageRegion into "num" pieces, returning
   * region "i" as "splitRegion". This method is called "num" times. The
   * regions must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the FixedImageRegion,
   * i.e. return value is less than or equal to "num". */
  virtual
  ThreadIdType SplitFixedRegion(ThreadIdType i, int num, FixedImageRegionType & splitRegion);

  /** Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to ThreadedGetValue(). */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback(void *arg);

  /** Internal structure used for passing image data into the threading library
    */
  struct ThreadStruct {
    Pointer Metric;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MatchCardinalityImageToImageMetric);

  bool                         m_MeasureMatches;
  std::vector< MeasureType >   m_ThreadMatches;
  std::vector< SizeValueType > m_ThreadCounts;

  /** Support processing data in multiple threads. Used by subclasses
   * (e.g., ImageSource). */
  MultiThreader::Pointer m_Threader;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMatchCardinalityImageToImageMetric.hxx"
#endif

#endif
