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
#ifndef itkSignedDanielssonDistanceMapImageFilter_h
#define itkSignedDanielssonDistanceMapImageFilter_h

#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkSubtractImageFilter.h"

// Simple functor to invert an image for Outside Danielsson distance map
namespace itk
{
namespace Functor
{
template <typename InputPixelType>
class ITK_TEMPLATE_EXPORT InvertIntensityFunctor
{
public:
  InputPixelType
  operator()(InputPixelType input) const
  {
    if (input)
    {
      return NumericTraits<InputPixelType>::ZeroValue();
    }
    else
    {
      return NumericTraits<InputPixelType>::OneValue();
    }
  }
};
} // namespace Functor
} // namespace itk

namespace itk
{
/**
 * \class SignedDanielssonDistanceMapImageFilter
 * \brief This filter computes the signed distance map of the input
 * image as an approximation with pixel accuracy to the Euclidean
 * distance.
 *
 * This class is parameterized over the type of the input image
 * and the type of the output image.
 *
 * For purposes of evaluating the signed distance map, the input is assumed
 * to be binary composed of pixels with value 0 and non-zero.
 *
 * The inside is considered as having negative distances. Outside is treated
 * as having positive distances. To change the convention,
 * use the InsideIsPositive(bool) function.
 *
 * As a convention, the distance is evaluated from the boundary of the ON pixels.
 *
 * The filter returns
 * - A signed distance map with the approximation to the euclidean distance.
 * - A voronoi partition. (See itkDanielssonDistanceMapImageFilter)
 * - A vector map containing the component of the vector relating
 *   the current pixel with the closest point of the closest object
 *   to this pixel. Given that the components of the distance are
 *   computed in "pixels", the vector is represented by an
 *   itk::Offset.  That is, physical coordinates are not used.
 *   (See itkDanielssonDistanceMapImageFilter)
 *
 * This filter internally uses the DanielssonDistanceMap filter.
 * This filter is N-dimensional.
 *
 * \sa itkDanielssonDistanceMapImageFilter
 *
 * \ingroup ImageFeatureExtraction
 *
 * \ingroup ITKDistanceMap
 *
 * \sphinx
 * \sphinxexample{Filtering/DistanceMap/SignedDistanceMapOfBinary,Signed Distance Map Of Binary Image}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage, typename TVoronoiImage = TInputImage>
class ITK_TEMPLATE_EXPORT SignedDanielssonDistanceMapImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SignedDanielssonDistanceMapImageFilter);

  /** Standard class type aliases. */
  using Self = SignedDanielssonDistanceMapImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SignedDanielssonDistanceMapImageFilter, ImageToImageFilter);

  /** Type for input image. */
  using InputImageType = TInputImage;

  /** Type for two of the three output images: the VoronoiMap and the
   * DistanceMap.  */
  using OutputImageType = TOutputImage;

  /** Type for the region of the input image. */
  using RegionType = typename InputImageType::RegionType;

  /** Type for the index of the input image. */
  using IndexType = typename RegionType::IndexType;

  /** Type for the index of the input image. */
  using OffsetType = typename InputImageType::OffsetType;

  /** Type for the pixel type of the input image. */
  using PixelType = typename InputImageType::PixelType;

  /** Type for the size of the input image. */
  using SizeType = typename RegionType::SizeType;

  /** The dimension of the input image. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;

  /** Pointer Type for the vector distance image */
  using VectorImageType = Image<OffsetType, Self::InputImageDimension>;

  /** Pointer Type for input image. */
  using InputImagePointer = typename InputImageType::ConstPointer;

  /** Pointer Type for the output image. */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Pointer Type for the vector distance image. */
  using VectorImagePointer = typename VectorImageType::Pointer;

  using VoronoiImageType = TVoronoiImage;
  using VoronoiImagePointer = typename VoronoiImageType::Pointer;
  using VoronoiPixelType = typename VoronoiImageType::PixelType;

  /** Pointer Type for data object */
  using DataObjectPointer = typename Superclass::DataObjectPointer;

  /** Set if the distance should be squared. */
  itkSetMacro(SquaredDistance, bool);

  /** Get the distance squared. */
  itkGetConstReferenceMacro(SquaredDistance, bool);

  /** Set On/Off if the distance is squared. */
  itkBooleanMacro(SquaredDistance);

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);

  /** Get whether spacing is used. */
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set On/Off whether spacing is used. */
  itkBooleanMacro(UseImageSpacing);

  /** Set if the inside represents positive values in the signed distance
   *  map. By convention ON pixels are treated as inside pixels.           */
  itkSetMacro(InsideIsPositive, bool);

  /** Get if the inside represents positive values in the signed distance map.
   *  See GetInsideIsPositive()  */
  itkGetConstReferenceMacro(InsideIsPositive, bool);

  /** Set if the inside represents positive values in the signed distance
   * map. By convention ON pixels are treated as inside pixels. Default is
   * true.                             */
  itkBooleanMacro(InsideIsPositive);

  /** Get Voronoi Map
   * This map shows for each pixel what object is closest to it.
   * Each object should be labeled by a number (larger than 0),
   * so the map has a value for each pixel corresponding to the label
   * of the closest object.  */
  VoronoiImageType *
  GetVoronoiMap();

  /** Get Distance map image.  The distance map is shown as a gray
   * value image depending on the pixel type of the output image.
   * Regarding the source image, background should be dark (gray value
   * = 0) and object should have a gray value larger than 0.  The
   * minimal distance is calculated on the object frontier, and the
   * output image gives for each pixel its minimal distance from the
   * object (if there is more than one object the closest object is
   * considered). */
  OutputImageType *
  GetDistanceMap();

  /** Get vector field of distances. */
  VectorImageType *
  GetVectorDistanceMap();

  /** This is overloaded to create the VectorDistanceMap output image */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, PixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  // End concept checking
#endif

protected:
  SignedDanielssonDistanceMapImageFilter();
  ~SignedDanielssonDistanceMapImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Compute Danielsson distance map and Voronoi Map. */
  void
  GenerateData() override;

private:
  bool m_SquaredDistance;
  bool m_UseImageSpacing;
  bool m_InsideIsPositive; // ON is treated as inside pixels
};                         // end of SignedDanielssonDistanceMapImageFilter
                           // class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSignedDanielssonDistanceMapImageFilter.hxx"
#endif

#endif
