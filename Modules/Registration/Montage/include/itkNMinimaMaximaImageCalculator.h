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
#ifndef itkNMinimaMaximaImageCalculator_h
#define itkNMinimaMaximaImageCalculator_h

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include <mutex>
#include <vector>

namespace itk
{
/** \class NMinimaMaximaImageCalculator
 *  \brief Computes the N highest and/or lowest intensity values of an image.
 *
 * This class is templated over input image type. If only Maxima or
 * Minima are needed, just call ComputeMaxima() or ComputeMinima().
 * Compute() will compute both.
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 * \ingroup Montage
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT NMinimaMaximaImageCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NMinimaMaximaImageCalculator);

  /** Standard class type aliases. */
  using Self = NMinimaMaximaImageCalculator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NMinimaMaximaImageCalculator, Object);

  /** Type definition for the input image. */
  using ImageType = TInputImage;

  /** Pointer type for the image. */
  using ImagePointer = typename TInputImage::Pointer;

  /** Const Pointer type for the image. */
  using ImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using PixelType = typename TInputImage::PixelType;

  /** Image dimensionality */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  // constexpr unsigned ImageDimension = TInputImage::VImageDimension;

  /** Type definition for the input image index type. */
  using IndexType = typename TInputImage::IndexType;

  /** Type definition for the input image region type. */
  using RegionType = typename TInputImage::RegionType;

  /** Sorted vector of minima or maxima. */
  using ValueVector = std::vector<PixelType>;

  /** Sorted vector of pixel indices of minima or maxima. */
  using IndexVector = std::vector<IndexType>;


  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Compute the minimum value of intensity of the input image. */
  void
  ComputeMinima();

  /** Compute the maximum value of intensity of the input image. */
  void
  ComputeMaxima();

  /** Compute the minimum and maximum values of intensity of the input image. */
  void
  Compute();

  /** Return the N minimum intensity values. */
  itkGetConstReferenceMacro(Minima, ValueVector);

  /** Return the N maximum intensity values. */
  itkGetConstReferenceMacro(Maxima, ValueVector);

  /** Return the indices of the N minimum intensity values. */
  itkGetConstReferenceMacro(IndicesOfMinima, IndexVector);

  /** Return the indices of the N maximum intensity values. */
  itkGetConstReferenceMacro(IndicesOfMaxima, IndexVector);

  /** Set the region over which the values will be computed */
  void
  SetRegion(const RegionType & region);

  /** Get/Set the number of extreme intensity values to keep. */
  itkGetConstMacro(N, SizeValueType);
  itkSetMacro(N, SizeValueType);

protected:
  NMinimaMaximaImageCalculator();
  ~NMinimaMaximaImageCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  template <typename TComparator = std::less<PixelType>>
  void
  SortedInsert(ValueVector &     vals,
               IndexVector &     indices,
               const PixelType & val,
               const IndexType & ind,
               TComparator       comp = TComparator());
  void
  InternalCompute();

private:
  ImageConstPointer m_Image;
  ValueVector       m_Minima;
  ValueVector       m_Maxima;
  IndexVector       m_IndicesOfMinima;
  IndexVector       m_IndicesOfMaxima;
  SizeValueType     m_N{ 7 };

  RegionType m_Region;
  bool       m_RegionSetByUser{ false };
  bool       m_ComputeMaxima{ true };
  bool       m_ComputeMinima{ true };
  std::mutex m_Mutex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNMinimaMaximaImageCalculator.hxx"
#endif

#endif /* itkNMinimaMaximaImageCalculator_h */
