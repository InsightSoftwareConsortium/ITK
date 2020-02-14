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
#ifndef itkFastChamferDistanceImageFilter_h
#define itkFastChamferDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNarrowBand.h"

namespace itk
{
/**
 *\class FastChamferDistanceImageFilter
 * \brief This class compute the signed (positive and negative) chamfer distance in a narrow band
 *
 * \par OVERVIEW
 * This filter computes a Signed Chamfer Distance Map of the input image
 * specially designed to work within the Level Set framework,
 * in the Narrow Band Reinitialization (generally applied after
 * IsoContourDistanceImageFilter ).
 * It can however be used for other purposes.
 *
 * The input is assumed to contain voxels with values higher than
 * the Maximal Computed Distance,
 * or values between -1 and 1 for voxels close to the 0-isosurface
 * from which we compute the distance.
 *
 * This filter is N-dimensional.
 *
 * \par REFERENCES
 * Fast and Accurate Redistancing for Level Set Methods
 * `Krissian K. and Westin C.F.',
 * EUROCAST NeuroImaging Workshop Las Palmas Spain,
 * Ninth International Conference on Computer Aided Systems Theory , pages 48-51, Feb 2003.
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKDistanceMap
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT FastChamferDistanceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastChamferDistanceImageFilter);

  /** Standard class type aliases. */
  using Self = FastChamferDistanceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastChamferDistanceImageFilter, ImageToImageFilter);

  /** Type for input image. */
  using InputImageType = TInputImage;

  /** Type for input image. */
  using OutputImageType = TOutputImage;

  /** Type for the region of the input image. */
  using RegionType = typename InputImageType::RegionType;

  /** Type for the region of the input image. */
  using PixelType = typename InputImageType::PixelType;

  /** Type for the index of the input image. */
  using IndexType = typename RegionType::IndexType;

  /** Type for the index of the input image. */
  using OffsetType = typename InputImageType::OffsetType;

  /** Type for the size of the input image. */
  using SizeType = typename RegionType::SizeType;

  /** The dimension of the input and output images. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;

  /** Pointer Type for input image. */
  using InputImagePointer = typename InputImageType::ConstPointer;

  /** NarrowBand container */
  using BandNodeType = BandNode<IndexType, PixelType>;
  using NarrowBandType = NarrowBand<BandNodeType>;
  using NarrowBandPointer = typename NarrowBandType::Pointer;

  using WeightsType = FixedArray<float, ImageDimension>;

  /** coefficients of the Chamfer distance for each kind of neighbor. */
  itkSetMacro(Weights, WeightsType);
  itkGetConstReferenceMacro(Weights, WeightsType);

  /** Maximal computed distance */
  itkSetMacro(MaximumDistance, float);
  itkGetConstMacro(MaximumDistance, float);

  /** */
  void
  SetRegionToProcess(const RegionType & r);

  RegionType
  GetRegionToProcess() const;

  void
  SetNarrowBand(NarrowBandType * ptr);

  NarrowBandPointer
  GetNarrowBand() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(SameTypeCheck, (Concept::SameType<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(FloatConvertibleToPixelTypeCheck, (Concept::Convertible<float, PixelType>));
  itkConceptMacro(PixelTypeConvertibleToFloatCheck, (Concept::Convertible<PixelType, float>));
  itkConceptMacro(PixelTypeGreaterThanFloatCheck, (Concept::GreaterThanComparable<PixelType, float>));
  itkConceptMacro(PixelTypeLessThanFloatCheck, (Concept::LessThanComparable<PixelType, float>));
  itkConceptMacro(PixelTypeFloatAdditiveOperatorsCheck, (Concept::AdditiveOperators<PixelType, float, float>));
  itkConceptMacro(FloatGreaterThanPixelTypeCheck, (Concept::GreaterThanComparable<float, PixelType>));
  itkConceptMacro(FloatLessThanPixelTypeCheck, (Concept::LessThanComparable<float, PixelType>));
  // End concept checking
#endif

protected:
  FastChamferDistanceImageFilter();
  ~FastChamferDistanceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Compute a Signed Chamfer Distance Map up to the specified maximal
  distance in n dimensions */
  void
  GenerateDataND();

  /** Compute a Signed Chamfer Distance Map up to the specified maximal
  distance */
  void
  GenerateData() override;

private:
  float m_MaximumDistance;

  /** coefficients of the Chamfer distance for each kind of neighbor. */
  WeightsType m_Weights;

  NarrowBandPointer m_NarrowBand;

  /** Region in the image to process.  */
  RegionType m_RegionToProcess;
}; // end of FastChamferDistanceImageFilter class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastChamferDistanceImageFilter.hxx"
#endif

#endif
