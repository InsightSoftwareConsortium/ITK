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
#ifndef itkBinaryMorphologyImageFilter_h
#define itkBinaryMorphologyImageFilter_h

#include <vector>
#include <queue>
#include "itkKernelImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class BinaryMorphologyImageFilter
 * \brief Base class for fast binary dilation and erosion
 *
 * BinaryMorphologyImageFilter is a base class for fast binary
 * morphological operations. The implementation of this class and its
 * subclasses are based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 * Grayscale images can be processed as binary images by selecting a
 * "ForegroundValued" (which subclasses may alias as "DilateValue" or
 * "ErodeValue").  Pixel not matching the foreground value are
 * considered "background".  This is useful in processing segmented
 * images where all pixels in segment #1 have value 1 and pixels in
 * segment #2 have value 2, etc. A particular "segment number" can be
 * processed.  ForegroundValue defaults to the maximum possible value
 * of the PixelType.
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 are candidates for affecting the center pixel.  A
 * reasonable choice of structuring element is
 * itk::BinaryBallStructuringElement.
 *
 *
 * Description of the algorithm:
 * ----------------------------------------------
 * Let's consider the set of the ON elements of the input image as X.
 *
 * Let's consider the structuring element as B = {B0, B1, ..., Bn},
 * where Bi denotes a connected component of B.
 *
 * Let's consider bi, i in [0,n], an arbitrary point of Bi.
 *
 * We use hence the next property in order to compute minkoswki
 * addition ( which will be written (+) ):
 *
 * X (+) B = ( Xb0 UNION Xb1 UNION ... Xbn ) UNION ( BORDER(X) (+) B ),
 *
 * where Xbi is the set X translated with respect to vector bi :
 *
 * Xbi ={ x + bi, x belongs to X }
 *
 * where BORDER(X) is the extracted border of X ( 8 connectivity in
 * 2D, 26 in 3D )
 *
 * Our implementation for dilation is defined as:
 *
 *     X (+) SYM(B) = DILATION(X)_B
 *
 * Where DILATION(X)_B is the dilation of set with structuring element B.
 * Where SYM(B) is the symmetric of the structuring element relatively
 * to its center.
 *
 * This code was contributed by Jerome Schmid from the University of
 * Strasbourg who provided a fast dilation implementation. Gaetan
 * Lehmann from INRA de Jouy-en-Josas then provided a fast erosion
 * implementation based on Jerome's implementation.  The common
 * portions of these two implementations were then placed in this
 * superclass.
 *
 * \sa ImageToImageFilter BinaryErodeImageFilter BinaryDilateImageFilter
 * \ingroup ITKBinaryMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BinaryMorphologyImageFilter : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryMorphologyImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Extract the dimension of the kernel */
  static constexpr unsigned int KernelDimension = TKernel::NeighborhoodDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = BinaryMorphologyImageFilter;
  using Superclass = KernelImageFilter<InputImageType, OutputImageType, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMorphologyImageFilter, ImageToImageFilter);

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputRealType = typename NumericTraits<InputPixelType>::RealType;
  using OffsetType = typename InputImageType::OffsetType;
  using IndexType = typename InputImageType::IndexType;
  using IndexValueType = typename InputImageType::IndexValueType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using InputSizeType = typename InputImageType::SizeType;
  using InputSizeValueType = typename InputImageType::SizeValueType;

  /** Input and output images must be the same dimension. */
  itkConceptMacro(ImageDimensionCheck, (Concept::SameDimension<Self::InputImageDimension, Self::OutputImageDimension>));

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. Subclasses may alias this to
   * DilateValue or ErodeValue. */
  itkSetMacro(ForegroundValue, InputPixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetConstMacro(ForegroundValue, InputPixelType);

  /** Set the value used as "background". Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * to fill the removed pixels.
   */
  itkSetMacro(BackgroundValue, OutputPixelType);

  /** Get the value used as "background". Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * to fill the removed pixels.
   */
  itkGetConstMacro(BackgroundValue, OutputPixelType);

  /** Get/Set the borders as foreground (true) or background (false).
   */
  itkSetMacro(BoundaryToForeground, bool);
  itkGetConstReferenceMacro(BoundaryToForeground, bool);
  itkBooleanMacro(BoundaryToForeground);

  /** Set kernel (structuring element). */
  void
  SetKernel(const KernelType & kernel) override;

protected:
  BinaryMorphologyImageFilter();
  ~BinaryMorphologyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Analyze kernel and prepare data for GenerateData() function */
  void
  AnalyzeKernel();

  /** Type definition of container of neighbourhood index */
  using NeighborIndexContainer = std::vector<OffsetType>;

  /** Type definition of container of container of neighbourhood index */
  using NeighborIndexContainerContainer = std::vector<NeighborIndexContainer>;

  /** Type definition of the container for indices */
  using ComponentVectorType = std::vector<OffsetType>;

  /** Iterator for ComponentVectorType */
  using ComponentVectorConstIterator = typename ComponentVectorType::const_iterator;

  /**
   * Get the difference set for a particular offset */
  NeighborIndexContainer &
  GetDifferenceSet(unsigned int code)
  {
    return m_KernelDifferenceSets[code];
  }

  /**
   * Get an iterator to the start of the connected component vector */
  ComponentVectorConstIterator
  KernelCCVectorBegin()
  {
    return m_KernelCCVector.begin();
  }

  /**
   * Get an iterator to the end of the connected component vector */
  ComponentVectorConstIterator
  KernelCCVectorEnd()
  {
    return m_KernelCCVector.end();
  }

  bool m_BoundaryToForeground;

private:
  /** Pixel value to dilate */
  InputPixelType m_ForegroundValue;

  /** Pixel value for background */
  OutputPixelType m_BackgroundValue;

  /** Difference sets definition */
  NeighborIndexContainerContainer m_KernelDifferenceSets;

  /** For each Connected Component ( CC ) of structuring element we
   * store the position of one element, arbitrary chosen, which belongs
   * to the CC */
  std::vector<OffsetType> m_KernelCCVector;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMorphologyImageFilter.hxx"
#endif

#endif
