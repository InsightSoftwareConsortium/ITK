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
#ifndef itkMirrorPadImageFilter_h
#define itkMirrorPadImageFilter_h

#include "itkPadImageFilter.h"
#include "itkNumericTraits.h"
#include <vector>

namespace itk
{

/**
 *\class MirrorPadImageFilter
 * \brief Increase the image size by padding with replicants of the
 * input image value.
 *
 * MirrorPadImageFilter changes the image bounds of an image. Any
 * added pixels are filled in with a mirrored replica of the input
 * image.  For instance, if the output image needs a pixel that is <b>two
 * pixels to the left of the LargestPossibleRegion</b> of the input image,
 * the value assigned will be from the pixel <b>two pixels inside the
 * left boundary of the LargestPossibleRegion</b>.  The image bounds of
 * the output must be specified.
 *
 * \image html MirrorPadImageFilter.png "Visual explanation of padding regions."
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 *
 * Exponential decay in the bounds is enabled when DecayBase has to be
 * in the range (0.0, 1.0]. When it is 1.0 it is disabled. The decay
 * rate is based on the Manhattan distance.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilter, ConstantPadImageFilter
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/PadAnImageByMirroring,Pad An Image By Mirroring}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT MirrorPadImageFilter : public PadImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MirrorPadImageFilter);

  /** Standard class type aliases. */
  using Self = MirrorPadImageFilter;
  using Superclass = PadImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MirrorPadImageFilter, PadImageFilter);

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  using OutputImageIndexType = typename Superclass::OutputImageIndexType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using OutputImageSizeType = typename Superclass::OutputImageSizeType;
  using InputImageSizeType = typename Superclass::InputImageSizeType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Get/Set the base for exponential decay in mirrored region. */
  itkGetMacro(DecayBase, double);
  itkSetClampMacro(DecayBase, double, NumericTraits<double>::min(), 1.0);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  // End concept checking
#endif

protected:
  MirrorPadImageFilter() = default;
  ~MirrorPadImageFilter() override = default;


  /** Convert from the output index to the input index taking
   * into consideration mirrored and normal regions. */
  void
  ConvertOutputIndexToInputIndex(OutputImageIndexType &  outputIndex,
                                 InputImageIndexType &   inputIndex,
                                 OutputImageRegionType & outputRegion,
                                 InputImageRegionType &  inputRegion,
                                 int *                   oddRegionArray,
                                 IndexValueType &        distanceFromEdge);

  void
  ConvertOutputIndexToInputIndex(OutputImageIndexType &  outputIndex,
                                 InputImageIndexType &   inputIndex,
                                 OutputImageRegionType & outputRegion,
                                 InputImageRegionType &  inputRegion,
                                 int *                   oddRegionArray,
                                 double &                outDecayFactor);

  /** Decide whether test falls within an odd or even number
   * of size regions from base. */
  int
  RegionIsOdd(long base, long test, long size);

  /** MirrorPadImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Given an n dimensional list of input region breakpoints in indices
   * and size (where the current region and maximum region for each dimension
   * is encoded in regIndices and regLimit), choose the next input region. */
  int
  GenerateNextInputRegion(long *                 regIndices,
                          long *                 regLimit,
                          std::vector<long> *    indices,
                          std::vector<long> *    sizes,
                          InputImageRegionType & outputRegion);

  /** Given an n dimensional list of output region breakpoints in indices
   * and size (where the current region and maximum region for each dimension
   * is encoded in regIndices and regLimit), choose the next output region. */
  int
  GenerateNextOutputRegion(long *                  regIndices,
                           long *                  regLimit,
                           std::vector<long> *     indices,
                           std::vector<long> *     sizes,
                           OutputImageRegionType & outputRegion);

  /** Given the start and end indices of a region, determine how many
   * instances of size fit within the region.  The variable offset provides
   * a way to adjust width of the area while forcing alignment to the
   * start or end location. */
  int
  FindRegionsInArea(long start, long end, long size, long offset);

  /** Generate region 0 (inter-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the
   * starting points and the lengths of the output region directly
   * corresponding to the input region.  Padding will be on either
   * side of this region.  The algorithmic complications are necessary
   * to support the streaming interface and multithreading. */
  int
  BuildInterRegions(std::vector<long> & inputRegionStart,
                    std::vector<long> & outputRegionStart,
                    std::vector<long> & inputRegionSizes,
                    std::vector<long> & outputRegionSizes,
                    long                inputIndex,
                    long                outputIndex,
                    long                inputSize,
                    long                outputSize,
                    int                 numRegs,
                    int &               regCtr);

  /** Generate region 1 (pre-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the
   * starting points and the lengths of the output region directly
   * preceding the input region in this dimension.  This may require
   * more than one region be defined if the padding is larger than the
   * size of the input image in this dimension.  Other algorithmic
   * complications are necessary to support the streaming interface
   * and multithreading. */
  int
  BuildPreRegions(std::vector<long> & inputRegionStart,
                  std::vector<long> & outputRegionStart,
                  std::vector<long> & inputRegionSizes,
                  std::vector<long> & outputRegionSizes,
                  long                inputIndex,
                  long                outputIndex,
                  long                inputSize,
                  long                outputSize,
                  int                 numRegs,
                  int &               regCtr);

  /** Generate region 2 (post-region) information.  Based on the indices
   * of the input and the output for this dimension, decide what are the
   * starting points and the lengths of the output region directly
   * succeeding the input region in this dimension.  This may require
   * more than one region be defined if the padding is larger than the
   * size of the input image in this dimension.  Other algorithmic
   * complications are necessary to support the streaming interface
   * and multithreading. */
  int
  BuildPostRegions(std::vector<long> & inputRegionStart,
                   std::vector<long> & outputRegionStart,
                   std::vector<long> & inputRegionSizes,
                   std::vector<long> & outputRegionSizes,
                   long                inputIndex,
                   long                outputIndex,
                   long                inputSize,
                   long                outputSize,
                   int                 numRegs,
                   int &               regCtr);

  /** MirrorPadImageFilter needs a different input requested region than
   * output requested region.  As such, MirrorPadImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   * \sa PadImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

private:
  double m_DecayBase = 1.0;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMirrorPadImageFilter.hxx"
#endif

#endif
