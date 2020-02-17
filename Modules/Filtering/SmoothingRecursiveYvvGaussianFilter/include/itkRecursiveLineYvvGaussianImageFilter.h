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

#ifndef itkRecursiveLineYvvGaussianImageFilter_h
#define itkRecursiveLineYvvGaussianImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkNumericTraits.h"
#include "itkImageRegionSplitterDirection.h"

namespace itk
{
/**
 * \class RecursiveLineYvvGaussianImageFilter
 * \brief 1D recursive Gaussian blur based on Young-Van Vliet's algorithm,
 *  implemented for CPU.
 *
 *  This CPU implementation is more efficient than the GPU implamentation for
 *  smaller images (e.g. 512 and smaller for quadcores at over 3GHz); use
 *  the benchmark tests to establish the size for which this implementation
 *  performs better for your particular hardware configuration.
 *
 *  More information in the Insight Journal publication:
 *  http://hdl.handle.net/10380/3425
 *
 * \ingroup SmoothingRecursiveYvvGaussianFilter
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT RecursiveLineYvvGaussianImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RecursiveLineYvvGaussianImageFilter);

  /** Standard class type alias. */
  using Self = RecursiveLineYvvGaussianImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Type macro that defines a name for this class. */
  itkTypeMacro(RecursiveLineYvvGaussianImageFilter, InPlaceImageFilter);

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Real type to be used in internal computations. RealType in general is
   * templated over the pixel type. (For example for vector or tensor pixels,
   * RealType is a vector or a tensor of doubles.) ScalarRealType is a type
   * meant for scalars.
   */
  using InputPixelType = typename TInputImage::PixelType;
#ifdef WITH_DOUBLE
  using RealType = typename NumericTraits<InputPixelType>::RealType;
  using ScalarRealType = typename NumericTraits<InputPixelType>::ScalarRealType;
#else
  using RealType = typename NumericTraits<InputPixelType>::FloatType;
  using ScalarRealType = typename NumericTraits<InputPixelType>::FloatType;
#endif

  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Type of the input image */
  using InputImageType = TInputImage;

  /** Type of the output image */
  using OutputImageType = TOutputImage;

  /** Get the direction in which the filter is to be applied. */
  itkGetConstMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */
  itkSetMacro(Direction, unsigned int);

  /** Set Input Image. */
  void
  SetInputImage(const TInputImage *);

  /** Get Input Image. */
  const TInputImage *
  GetInputImage();

  /** Set/Get the flag for normalizing the gaussian over scale space.
   When this flag is ON the filter will be normalized in such a way
   that larger sigmas will not result in the image fading away.

   \f[
   \frac{ 1 }{ \sqrt{ 2 \pi } };
   \f]

   When the flag is OFF the normalization will conserve contant the
   integral of the image intensity.
   \f[
   \frac{ 1 }{ \sigma  \sqrt{ 2 \pi } };
   \f]
   For analyzing an image across Scale Space you want to enable
   this flag.  It is disabled by default.  */
  itkSetMacro(NormalizeAcrossScale, bool);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** Set/Get the Sigma, measured in world coordinates, of the Gaussian
   * kernel.  The default is 1.0.  */
  itkGetConstMacro(Sigma, ScalarRealType);
  itkSetMacro(Sigma, ScalarRealType);

protected:
  RecursiveLineYvvGaussianImageFilter();
  ~RecursiveLineYvvGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GenerateData (apply) the filter. */
  void
  BeforeThreadedGenerateData() override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  const ImageRegionSplitterBase *
  GetImageRegionSplitter() const override;

  /** RecursiveLineYvvGaussianImageFilter needs all of the input only in the
   *  "Direction" dimension. Therefore we enlarge the output's
   *  RequestedRegion to this. Then the superclass's
   *  GenerateInputRequestedRegion method will copy the output region
   *  to the input.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * Typically it can be used to approximate a Gaussian or one of its
   * derivatives. Parameter is the spacing along the dimension to
   * filter. */
  virtual void
  SetUp(ScalarRealType spacing);

  /** Apply the Recursive Filter to an array of data.  This method is called
   * for each line of the volume. Parameter "scratch" is a scratch
   * area used for internal computations that is the same size as the
   * parameters "outs" and "data". The scratch area must be allocated
   * outside of this routine (this avoids memory allocation and
   * deallocation in the inner loop of the overall algorithm. */
  void
  FilterDataArray(RealType * outs, const RealType * data, RealType * scratch, unsigned int ln);

protected:
  /** Causal and anti-causal coefficients that multiply the input data. These
    are already divided by B0 */
  ScalarRealType m_B1;
  ScalarRealType m_B2;
  ScalarRealType m_B3;
  ScalarRealType m_B;

  // Initialization matrix for anti-causal pass
  vnl_matrix<ScalarRealType> m_MMatrix;

private:
  /** Direction in which the filter is to be applied
   * this should be in the range [0,ImageDimension-1]. */
  unsigned int m_Direction;

  /** Sigma of the gaussian kernel. */
  ScalarRealType m_Sigma;

  /** Normalize the image across scale space */
  bool                                  m_NormalizeAcrossScale;
  ImageRegionSplitterDirection::Pointer m_ImageRegionSplitter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRecursiveLineYvvGaussianImageFilter.hxx"
#endif

#endif // itkRecursiveLineYvvGaussianImageFilter_h
