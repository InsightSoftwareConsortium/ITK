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
#ifndef itkScalarRegionBasedLevelSetFunction_h
#define itkScalarRegionBasedLevelSetFunction_h

#include "itkRegionBasedLevelSetFunction.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class ScalarRegionBasedLevelSetFunction
 *
 * \brief LevelSet function that computes a speed image based on regional integrals
 *
 * This class implements a level set function that computes the speed image by
 * integrating values on the image domain. NOTE: The convention followed is
 * inside of the level-set function is negative and outside is positive.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ITKReview
 */
template <typename TInputImage, typename TFeatureImage, typename TSharedData>
class ITK_TEMPLATE_EXPORT ScalarRegionBasedLevelSetFunction
  : public RegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarRegionBasedLevelSetFunction);

  using Self = ScalarRegionBasedLevelSetFunction;
  using Superclass = RegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  // itkNewMacro() is not provided since this is an abstract class.

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarRegionBasedLevelSetFunction, RegionBasedLevelSetFunction);

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  using InputImageType = typename Superclass::InputImageType;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputIndexType = typename Superclass::InputIndexType;
  using InputIndexValueType = typename Superclass::InputIndexValueType;
  using InputSizeType = typename Superclass::InputSizeType;
  using InputSizeValueType = typename Superclass::InputSizeValueType;
  using InputRegionType = typename Superclass::InputRegionType;
  using InputPointType = typename Superclass::InputPointType;

  using FeatureImageType = typename Superclass::FeatureImageType;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeaturePixelType = typename Superclass::FeaturePixelType;
  using FeatureIndexType = typename Superclass::FeatureIndexType;
  using FeatureOffsetType = typename Superclass::FeatureOffsetType;

  using ScalarValueType = typename Superclass::ScalarValueType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using RadiusType = typename Superclass::RadiusType;
  using TimeStepType = typename Superclass::TimeStepType;
  using GlobalDataStruct = typename Superclass::GlobalDataStruct;
  using PixelType = typename Superclass::PixelType;
  using VectorType = typename Superclass::VectorType;

  using SharedDataType = typename Superclass::SharedDataType;
  using SharedDataPointer = typename Superclass::SharedDataPointer;

  using ImageIteratorType = ImageRegionIteratorWithIndex<InputImageType>;
  using ConstImageIteratorType = ImageRegionConstIteratorWithIndex<InputImageType>;
  using FeatureImageIteratorType = ImageRegionIteratorWithIndex<FeatureImageType>;
  using ConstFeatureIteratorType = ImageRegionConstIterator<FeatureImageType>;

  using ListPixelType = std::list<unsigned int>;
  using ListPixelConstIterator = typename ListPixelType::const_iterator;
  using ListPixelIterator = typename ListPixelType::iterator;
  using ListImageType = Image<ListPixelType, Self::ImageDimension>;

  /** \brief Performs the narrow-band update of the Heaviside function for each
  voxel. The characteristic function of each region is recomputed (note the
  shared data which contains information from the other level sets). Using the
  new H values, the previous c_i are updated. */
  void
  UpdatePixel(const unsigned int &                idx,
              NeighborhoodIterator<TInputImage> & iterator,
              InputPixelType &                    newValue,
              bool &                              status);

protected:
  ScalarRegionBasedLevelSetFunction()
    : Superclass()
  {}
  ~ScalarRegionBasedLevelSetFunction() override = default;

  ScalarValueType
  ComputeOverlapParameters(const FeatureIndexType & featIndex, ScalarValueType & product) override;

  // update the background and foreground constants for pixel updates
  // Called only when sparse filters are used to prevent iteration through the
  // entire image
  virtual void
  UpdateSharedDataInsideParameters(const unsigned int &     iId,
                                   const FeaturePixelType & iVal,
                                   const ScalarValueType &  iChange) = 0;

  virtual void
  UpdateSharedDataOutsideParameters(const unsigned int &     iId,
                                    const FeaturePixelType & iVal,
                                    const ScalarValueType &  iChange) = 0;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarRegionBasedLevelSetFunction.hxx"
#endif

#endif
