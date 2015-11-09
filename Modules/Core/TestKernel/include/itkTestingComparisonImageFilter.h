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
#ifndef itkTestingComparisonImageFilter_h
#define itkTestingComparisonImageFilter_h

#include "itkArray.h"
#include "itkNumericTraits.h"
#include "itkImageSource.h"

namespace itk
{
namespace Testing
{
/** \class ComparisonImageFilter
 * \brief Implements comparison between two images.
 *
 * This filter is used by the testing system to compute the difference between
 * a valid image and an image produced by the test. The comparison value is
 * computed by visiting all the pixels in the baseline images and comparing
 * their values with the pixel values in the neighborhood of the homologous
 * pixel in the other image.
 *
 * \ingroup IntensityImageFilters   MultiThreaded
 * \ingroup ITKTestKernel
 */
template< typename TInputImage, typename TOutputImage >
class ComparisonImageFilter:
  public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ComparisonImageFilter             Self;
  typedef ImageSource< TOutputImage >       Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComparisonImageFilter, ImageSource);

  /** Some convenient typedefs. */
  typedef TInputImage                                         InputImageType;
  typedef typename InputImageType::PixelType                  InputPixelType;
  typedef TOutputImage                                        OutputImageType;
  typedef typename OutputImageType::PixelType                 OutputPixelType;
  typedef typename OutputImageType::RegionType                OutputImageRegionType;
  typedef typename NumericTraits< OutputPixelType >::RealType RealType;
  typedef typename NumericTraits< RealType >::AccumulateType  AccumulateType;

  /** Set the valid image input.  This will be input 0.  */
  virtual void SetValidInput(const InputImageType *validImage);

  /** Set the test image input.  This will be input 1.  */
  virtual void SetTestInput(const InputImageType *testImage);

  /** Set/Get the maximum distance away to look for a matching pixel.
      Default is 0. */
  itkSetMacro(ToleranceRadius, int);
  itkGetConstMacro(ToleranceRadius, int);

  /** Set/Get the minimum threshold for pixels to be different.
      Default is 0. */
  itkSetMacro(DifferenceThreshold, OutputPixelType);
  itkGetConstMacro(DifferenceThreshold, OutputPixelType);

  /** Set/Get ignore boundary pixels.  Useful when resampling may have
   *    introduced difference pixel values along the image edge
   *    Default = false */
  itkSetMacro(IgnoreBoundaryPixels, bool);
  itkGetConstMacro(IgnoreBoundaryPixels, bool);

  /** Get statistical attributes for those pixels which exceed the
   * tolerance and radius parameters */
  itkGetConstMacro(MinimumDifference, OutputPixelType);
  itkGetConstMacro(MaximumDifference, OutputPixelType);
  itkGetConstMacro(MeanDifference, RealType);
  itkGetConstMacro(TotalDifference, AccumulateType);
  itkGetConstMacro(NumberOfPixelsWithDifferences, SizeValueType);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void SetInput(const TInputImage *image);
  virtual void SetInput(unsigned int, const TInputImage *image);
  const TInputImage * GetInput() const;
  const TInputImage * GetInput(unsigned int idx) const;

protected:
  ComparisonImageFilter();
  virtual ~ComparisonImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** ComparisonImageFilter can be implemented as a multithreaded
   * filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   */
  void ThreadedGenerateData(const OutputImageRegionType & threadRegion,
                            ThreadIdType threadId) ITK_OVERRIDE;

  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  OutputPixelType m_DifferenceThreshold;

  RealType        m_MeanDifference;
  OutputPixelType m_MinimumDifference;
  OutputPixelType m_MaximumDifference;

  AccumulateType m_TotalDifference;

  SizeValueType m_NumberOfPixelsWithDifferences;

  int m_ToleranceRadius;

  Array< AccumulateType >    m_ThreadDifferenceSum;
  Array< SizeValueType >     m_ThreadNumberOfPixels;

  Array< OutputPixelType >    m_ThreadMinimumDifference;
  Array< OutputPixelType >    m_ThreadMaximumDifference;

private:
  ComparisonImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void operator=(const Self &) ITK_DELETE_FUNCTION;

  bool m_IgnoreBoundaryPixels;
};
} // end namespace Testing
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTestingComparisonImageFilter.hxx"
#endif


#endif
