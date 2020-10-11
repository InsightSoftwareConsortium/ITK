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
#ifndef itkDanielssonDistanceMapImageFilter_h
#define itkDanielssonDistanceMapImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/**
 * \class DanielssonDistanceMapImageFilter
 * \brief This filter computes the distance map of the input image
 * as an approximation with pixel accuracy to the Euclidean distance.
 *
 * \tparam TInputImage Input Image Type
 * \tparam TOutputImage Output Image Type
 * \tparam TVoronoiImage Voronoi Image Type. Note the default value is TInputImage.
 *
 * The input is assumed to contain numeric codes defining objects.
 * The filter will produce as output the following images:
 *
 * \li A <b>Voronoi partition</b> using the same numeric codes as the input.
 * \li A <b>distance map</b> with the approximation to the euclidean distance.
 *   from a particular pixel to the nearest object to this pixel
 *   in the input image.
 * \li A <b>vector map</b> containing the component of the vector relating
 *   the current pixel with the closest point of the closest object
 *   to this pixel. Given that the components of the distance are
 *   computed in "pixels", the vector is represented by an
 *   itk::Offset. That is, physical coordinates are not used.
 *
 * This filter is N-dimensional and known to be efficient
 * in computational time. The algorithm is the N-dimensional version
 * of the 4SED algorithm given for two dimensions in:
 *
 * Danielsson, Per-Erik.  Euclidean Distance Mapping.  Computer
 * Graphics and Image Processing 14, 227-248 (1980).
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKDistanceMap
 */
template <typename TInputImage, typename TOutputImage, typename TVoronoiImage = TInputImage>
class ITK_TEMPLATE_EXPORT DanielssonDistanceMapImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DanielssonDistanceMapImageFilter);

  /** Standard class type aliases. */
  using Self = DanielssonDistanceMapImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using DataObjectPointer = DataObject::Pointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DanielssonDistanceMapImageFilter, ImageToImageFilter);

  /** Type for input image. */
  using InputImageType = TInputImage;

  /** Type for input image pixel.*/
  using InputPixelType = typename InputImageType::PixelType;

  /** Type for the region of the input image. */
  using RegionType = typename InputImageType::RegionType;

  /** Type for the index of the input image. */
  using IndexType = typename RegionType::IndexType;

  /** Type for the index of the input image. */
  using OffsetType = typename InputImageType::OffsetType;

  /** Type for the spacing of the input image. */
  using SpacingType = typename InputImageType::SpacingType;
  using SpacingValueType = typename InputImageType::SpacingValueType;

  /** Type for the size of the input image. */
  using SizeType = typename RegionType::SizeType;

  /** Type for one size element of the input image.*/
  using SizeValueType = typename SizeType::SizeValueType;

  /** Type for two of the three output images: the VoronoiMap and the
   * DistanceMap.  */
  using OutputImageType = TOutputImage;

  /** Type for output image pixel.*/
  using OutputPixelType = typename OutputImageType::PixelType;

  using VoronoiImageType = TVoronoiImage;
  using VoronoiImagePointer = typename VoronoiImageType::Pointer;
  using VoronoiPixelType = typename VoronoiImageType::PixelType;

  /** The dimension of the input and output images. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;

  /** Pointer Type for the vector distance image */
  using VectorImageType = Image<OffsetType, Self::InputImageDimension>;

  /** Pointer Type for input image. */
  using InputImagePointer = typename InputImageType::ConstPointer;

  /** Pointer Type for the output image. */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Pointer Type for the vector distance image. */
  using VectorImagePointer = typename VectorImageType::Pointer;

  /** Set if the distance should be squared. */
  itkSetMacro(SquaredDistance, bool);

  /** Get the distance squared. */
  itkGetConstReferenceMacro(SquaredDistance, bool);

  /** Set On/Off if the distance is squared. */
  itkBooleanMacro(SquaredDistance);

  /** Set if the input is binary. If this variable is set, each
   * nonzero pixel in the input image will be given a unique numeric
   * code to be used by the Voronoi partition.  If the image is binary
   * but you are not interested in the Voronoi regions of the
   * different nonzero pixels, then you need not set this.  */
  itkSetMacro(InputIsBinary, bool);

  /** Get if the input is binary.  See SetInputIsBinary(). */
  itkGetConstReferenceMacro(InputIsBinary, bool);

  /** Set On/Off if the input is binary.  See SetInputIsBinary(). */
  itkBooleanMacro(InputIsBinary);

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);

  /** Get whether spacing is used. */
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set On/Off whether spacing is used. */
  itkBooleanMacro(UseImageSpacing);

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

  /** Standard itk::ProcessObject subclass method. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

#ifdef ITK_USE_CONCEPT_CHECKING
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int VoronoiImageDimension = TVoronoiImage::ImageDimension;

  // Begin concept checking
  itkConceptMacro(InputOutputSameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputVoronoiSameDimensionCheck, (Concept::SameDimension<InputImageDimension, VoronoiImageDimension>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, OutputPixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  // End concept checking
#endif

protected:
  DanielssonDistanceMapImageFilter();
  ~DanielssonDistanceMapImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Compute Danielsson distance map and Voronoi Map. */
  void
  GenerateData() override;

  /** Prepare data. */
  void
  PrepareData();

  /**  Compute Voronoi Map. */
  void
  ComputeVoronoiMap();

  /** Update distance map locally.  Used by GenerateData(). */
  void
  UpdateLocalDistance(VectorImageType *, const IndexType &, const OffsetType &);

private:
  bool m_SquaredDistance;
  bool m_InputIsBinary;
  bool m_UseImageSpacing;

  SpacingType m_InputSpacingCache;

}; // end of DanielssonDistanceMapImageFilter class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDanielssonDistanceMapImageFilter.hxx"
#endif

#endif
