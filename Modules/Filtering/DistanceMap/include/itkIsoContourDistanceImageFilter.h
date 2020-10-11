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
#ifndef itkIsoContourDistanceImageFilter_h
#define itkIsoContourDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNarrowBand.h"
#include "itkNeighborhoodIterator.h"
#include "itkNumericTraits.h"
#include <mutex>

namespace itk
{
/**
 *\class IsoContourDistanceImageFilter
 *  \brief Compute an approximate distance from an interpolated isocontour
 *  to the close grid points.
 *
 * For standard level set algorithms, it is useful to periodically
 * reinitialize the evolving image to prevent numerical accuracy
 * problems in computing derivatives.
 * This reinitialization is done by computing a signed distance map
 * to the current level set.
 * This class provides the first step in this reinitialization by
 * computing an estimate of the distance from the interpolated isocontour
 * to the pixels (or voxels) that are close to it, i.e. for which the
 * isocontour crosses a segment between them and one of their direct
 * neighbors.
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 *
 * Implementation of this class is based on
 * Fast and Accurate Redistancing for Level Set Methods
 *`Krissian K. and Westin C.F.',
 * EUROCAST NeuroImaging Workshop Las Palmas Spain,
 * Ninth International Conference on Computer Aided Systems Theory , pages 48-51, Feb 2003.
 *
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKDistanceMap
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT IsoContourDistanceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IsoContourDistanceImageFilter);

  /** Standard class type aliases. */
  using Self = IsoContourDistanceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsoContourDistanceImageFilter, ImageToImageFilter);

  /**Typedefs from the superclass */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  using PixelType = typename OutputImageType::PixelType;
  using InputPixelType = typename InputImageType::PixelType;
  using PixelRealType = typename NumericTraits<InputPixelType>::RealType;

  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputSizeType = typename InputImageType::SizeType;
  using SizeType = typename OutputImageType::SizeType;

  using InputIndexType = typename InputImageType::IndexType;
  using IndexType = typename OutputImageType::IndexType;

  using InputSpacingType = typename InputImageType::SpacingType;

  /** NarrowBand type alias support */
  using BandNodeType = BandNode<IndexType, PixelType>;
  using NarrowBandType = NarrowBand<BandNodeType>;
  using NarrowBandPointer = typename NarrowBandType::Pointer;
  using RegionType = typename NarrowBandType::RegionType;
  using ConstBandIterator = typename NarrowBandType::ConstIterator;
  using BandIterator = typename NarrowBandType::Iterator;

  /** Set/Get the value of the level set to be located. The default value is
   *  0. */
  itkSetMacro(LevelSetValue, PixelRealType);
  itkGetConstMacro(LevelSetValue, PixelRealType);

  /** Set/Get the value of the level set to be located. The default value is
   *  0. */
  itkSetMacro(FarValue, PixelType);
  itkGetConstMacro(FarValue, PixelType);

  /** Set/Get the narrowbanding flag. By default, narrowbanding is switched
   * off. */
  itkSetMacro(NarrowBanding, bool);
  itkGetConstMacro(NarrowBanding, bool);
  itkBooleanMacro(NarrowBanding);

  /** Set/Get the narrowband. */
  void
  SetNarrowBand(NarrowBandType * ptr);

  NarrowBandPointer
  GetNarrowBand() const
  {
    return m_NarrowBand;
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputPixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<PixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, PixelType>));
  itkConceptMacro(OutputAdditiveOperatorsCheck, (Concept::AdditiveOperators<PixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<PixelType>));
  // End concept checking
#endif

protected:
  IsoContourDistanceImageFilter();
  ~IsoContourDistanceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override
  {
    itkExceptionMacro("This class requires threadId so it must use classic multi-threading model");
  }

  void
  GenerateData() override;

  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ThreaderFullCallback(void * arg);

  void
  ThreadedGenerateDataFull(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void
  ThreadedGenerateDataBand(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void
  BeforeThreadedGenerateData() override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  using InputNeighbordIteratorType = ConstNeighborhoodIterator<InputImageType>;
  using OutputNeighborhoodIteratorType = NeighborhoodIterator<OutputImageType>;

  void
  ComputeValue(const InputNeighbordIteratorType &   inNeigIt,
               OutputNeighborhoodIteratorType &     outNeigIt,
               unsigned int                         center,
               const std::vector<OffsetValueType> & stride);

private:
  PixelRealType m_LevelSetValue;
  PixelType     m_FarValue;

  InputSpacingType m_Spacing;

  bool                    m_NarrowBanding;
  NarrowBandPointer       m_NarrowBand;
  std::vector<RegionType> m_NarrowBandRegion;

  std::mutex m_Mutex;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIsoContourDistanceImageFilter.hxx"
#endif

#endif
