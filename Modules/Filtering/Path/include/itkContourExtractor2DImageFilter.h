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
#ifndef itkContourExtractor2DImageFilter_h
#define itkContourExtractor2DImageFilter_h

#include <deque>
#include <list>
#include <unordered_map>
#include "itkConceptChecking.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionRange.h"
#include "itkImageToPathFilter.h"
#include "itkIndexRange.h"
#include "itkPolyLineParametricPath.h"

namespace itk
{
/**
 *\class ContourExtractor2DImageFilter
 * \brief Computes a list of PolyLineParametricPath objects from the contours in
 * a 2D image.
 *
 * Uses the "marching squares" method to compute a the iso-valued contours of
 * the input 2D image for a given intensity value. Multiple outputs may be
 * produced because an image can have multiple contours at a given level, so it
 * is advised to call GetNumberOfIndexedOutputs() and GetOutput(n) to retrieve all of
 * the contours. The contour value to be extracted can be set with
 * SetContourValue(). Image intensities will be linearly interpolated to provide
 * sub-pixel resolution for the output contours.
 *
 * The marching squares algorithm is a special case of the marching cubes
 * algorithm (Lorensen, William and Harvey E. Cline. Marching Cubes: A High
 * Resolution 3D Surface Construction Algorithm. Computer Graphics (SIGGRAPH 87
 * Proceedings) 21(4) July 1987, p. 163-170). A simple explanation is available
 * here: http://users.polytech.unice.fr/~lingrand/MarchingCubes/algo.html
 *
 * There is an ambiguous case in the marching squares algorithm: if a given
 * 2x2-pixel square has two high-valued and two low-valued pixels, each pair
 * diagonally adjacent. (Note that high-valued and low-valued are with respect
 * to the contour value sought when LabelContours is false. When
 * LabelContours is true, high-valued means the label being traced and
 * low-valued means any other label.) In this case, the default behavior is that
 * the low-valued pixels are connected into the same contour via an isthmus that
 * separates the high-valued pixels into separate contours. To reverse this,
 * call VertexConnectHighPixelsOn(). Note that when LabelContours is true, the
 * default behavior will leave all four pixels in separate contours. In this
 * case, VertexConnectHighPixels equal to true can instead create contours that
 * are crossing barbells.
 *
 * Outputs are not guaranteed to be closed paths: contours which intersect the
 * image edge will be left open. All other paths will be closed. (The
 * closedness of a path can be tested by checking whether the beginning point
 * is the same as the end point.)
 *
 * Produced paths are oriented. Following the path from beginning to end, image
 * intensity values lower than the contour value are to the left of the path and
 * intensity values greater than the contour value are to the right. In other
 * words, the image gradient at a path segment is (approximately) in the direct
 * of that segment rotated right by 90 degrees, because the image intensity
 * values increase from left-to-right across the segment. This means that the
 * generated contours will circle clockwise around "hills" of
 * above-contour-value intensity, and counter-clockwise around "depressions" of
 * below-contour-value intensity. This convention can be reversed by calling
 * ReverseContourOrientationOn().
 *
 * By default values are interpreted as intensities relative to a contour value.
 * First calling LabelContoursOn() changes this behavior to instead interpret each
 * value as a label.  Boundaries are computed for each label separately and all are
 * returned.  The value of LabelContours affects the interpretation of
 * VertexConnectHighPixels; see above.

 * By default the input image's largest possible region will be processed; call
 * SetRequestedRegion() to process a different region, or ClearRequestedRegion()
 * to revert to the default value. Note that the requested regions are usually
 * set on the output; however since paths have no notion of a "region", this
 * must be set at the filter level.
 *
 * This class was contributed to the Insight Journal by Zachary Pincus.
 * https://www.insight-journal.org/browse/publication/72
 *
 * \sa Image
 * \sa Path
 * \sa PolyLineParametricPath
 *
 * \ingroup ITKPath
 *
 * \sphinx
 * \sphinxexample{Filtering/Path/ExtractContoursFromImage,Extract Contours From Image}
 * \endsphinx
 */

template <typename TInputImage>
class ITK_TEMPLATE_EXPORT ContourExtractor2DImageFilter
  : public ImageToPathFilter<TInputImage, PolyLineParametricPath<2>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ContourExtractor2DImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension{ TInputImage::ImageDimension };

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputPathType = PolyLineParametricPath<2>;

  /** Standard class type aliases. */
  using Self = ContourExtractor2DImageFilter;
  using Superclass = ImageToPathFilter<InputImageType, OutputPathType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ContourExtractor2DImageFilter, ImageToPathFilter);

  /** Image and path type alias support */
  using InputImagePointer = typename InputImageType::Pointer;
  using InputIndexType = typename InputImageType::IndexType;
  using InputSizeType = typename InputImageType::SizeType;
  using InputOffsetType = typename InputImageType::OffsetType;
  using InputPixelType = typename InputImageType::PixelType;
  using InputRegionType = typename InputImageType::RegionType;
  using OutputPathPointer = typename OutputPathType::Pointer;
  using VertexListType = typename OutputPathType::VertexListType;
  using VertexListConstPointer = typename VertexListType::ConstPointer;
  using VertexType = typename OutputPathType::VertexType;
  using VertexValueType = typename VertexType::ValueType;

  /** Real type associated to the input pixel type. */
  using InputRealType = typename NumericTraits<InputPixelType>::RealType;

  /** Ranges and iterators for regions */
  using RegionIndexRange = ImageRegionIndexRange<InputImageType::ImageDimension>;
  using RegionRange = ImageRegionRange<InputImageType>;
  using RegionConstRange = ImageRegionRange<const InputImageType>;
  using RegionIterator = ImageRegionIterator<InputImageType>;
  using RegionConstIterator = ImageRegionConstIterator<InputImageType>;

  /** Control the orientation of the contours with reference to the image
   * gradient. (See class documentation.) */
  itkSetMacro(ReverseContourOrientation, bool);
  itkGetConstReferenceMacro(ReverseContourOrientation, bool);
  itkBooleanMacro(ReverseContourOrientation);

  /** Control whether high- or low-valued pixels are vertex-connected.
   * Default is for low-valued pixels to be vertex-connected.
   * (See class documentation.) */
  itkSetMacro(VertexConnectHighPixels, bool);
  itkGetConstReferenceMacro(VertexConnectHighPixels, bool);
  itkBooleanMacro(VertexConnectHighPixels);

  /** Return contours for all distinct labels */
  itkSetMacro(LabelContours, bool);
  itkGetConstReferenceMacro(LabelContours, bool);
  itkBooleanMacro(LabelContours);

  /** Control whether the largest possible input region is used, or if a
   * custom requested region is to be used. */
  void
  SetRequestedRegion(const InputRegionType region);

  itkGetConstReferenceMacro(RequestedRegion, InputRegionType);
  void
  ClearRequestedRegion();

  /** Set/Get the image intensity value that the contours should follow.
   *  This is the equivalent of an iso-value in Marching Squares. */
  itkSetMacro(ContourValue, InputRealType);
  itkGetConstReferenceMacro(ContourValue, InputRealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(DimensionShouldBe2, (Concept::SameDimension<Self::InputImageDimension, 2>));
  itkConceptMacro(InputPixelTypeComparable, (Concept::Comparable<InputPixelType>));
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  ContourExtractor2DImageFilter();
  ~ContourExtractor2DImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** ContourExtractor2DImageFilter manually controls the input requested
   * region via SetRequestedRegion and ClearRequestedRegion, so it must
   * override the superclass method. */
  void
  GenerateInputRequestedRegion() override;

private:
  using LabelsContainer = std::vector<InputPixelType>;
  using LabelsConstIterator = typename LabelsContainer::const_iterator;
  using LabelsIterator = typename LabelsContainer::iterator;

  InputRealType   m_ContourValue;
  bool            m_ReverseContourOrientation;
  bool            m_VertexConnectHighPixels;
  bool            m_LabelContours;
  bool            m_UseCustomRegion;
  InputRegionType m_RequestedRegion;

  // Represent each contour as deque of vertices to facilitate addition of
  // nodes at beginning or end. At the end of the processing, we will copy
  // the contour into a PolyLineParametricPath.
  // We subclass the deque to store an additional bit of information: an
  // identification number for each growing contour. We use this number so
  // that when it becomes necessary to merge two growing contours, we can
  // merge the newer one into the older one. This helps because then we can
  // guarantee that the output contour list is ordered from left to right,
  // top to bottom (in terms of the first pixel of the contour encountered
  // by the marching squares). Currently we make no guarantees that this
  // pixel is the first pixel in the contour list, just that the contours
  // are so ordered in the output. Ensuring this latter condition (first
  // pixel traversed = first pixel in contour) would be possible by either
  // changing the merging rules, which would make the contouring operation
  // slower, or by storing additional data as to which pixel was first.
  class ContourType : public std::deque<VertexType>
  {
  public:
    unsigned int m_ContourNumber;
  };

  // Store all the growing contours in a list. We may need to delete contours
  // from anywhere in the sequence (when we merge them together), so we need to
  // use a list instead of a vector or similar.
  using ContourConstIterator = typename ContourType::const_iterator;
  using ContourContainerType = std::list<ContourType>;
  using ContourContainerIterator = typename ContourContainerType::iterator;
  using ContourContainerConstIterator = typename ContourContainerType::const_iterator;

  // We use a hash to associate the endpoints of each contour with the
  // contour itself. This makes it easy to look up which contour we should add
  // a new arc to.

  // We can't store the contours themselves in the hashtable because we
  // need to have two tables (one to hash from beginpoint -> contour and one
  // for endpoint -> contour), and sometimes will remove a contour from the
  // tables (if it has been closed or merged with another contour). Because
  // sometimes we will need to merge contours, we need to be able to quickly
  // remove contours from our list when they have been merged into
  // another. Thus, we store an iterator pointing to the contour in the list.

  struct VertexHash
  {
    using CoordinateType = typename VertexType::CoordRepType;
    inline std::size_t
    operator()(const VertexType & v) const noexcept
    {
      return std::hash<CoordinateType>{}(v[0]) ^ (std::hash<CoordinateType>{}(v[1]) << 1);
    }
  };
  using VertexToContourContainerIteratorMap = std::unordered_map<VertexType, ContourContainerIterator, VertexHash>;
  using VertexToContourContainerIteratorMapIterator = typename VertexToContourContainerIteratorMap::iterator;
  using VertexToContourContainerIteratorMapConstIterator = typename VertexToContourContainerIteratorMap::const_iterator;
  using VertexToContourContainerIteratorMapKeyValuePair = typename VertexToContourContainerIteratorMap::value_type;

  // Subroutine to create contours for a single label.
  void
  CreateSingleContour(InputPixelType         label,
                      const InputImageType * input,
                      const InputRegionType  extendedRegion,
                      SizeValueType          totalNumberOfPixels,
                      ContourContainerType & contoursOutput);

  // Subroutine to handle the case that the supplied values are
  // labels, which are *not* compared to a contour value.
  void
  GenerateDataForLabels();

  VertexType
  InterpolateContourPosition(InputPixelType  fromValue,
                             InputPixelType  toValue,
                             InputIndexType  fromIndex,
                             InputOffsetType toOffset);

  struct ContourData
  {
    ContourContainerType                m_Contours;
    VertexToContourContainerIteratorMap m_ContourStarts;
    VertexToContourContainerIteratorMap m_ContourEnds;
    SizeValueType                       m_NumberOfContoursCreated = 0;
  };

  void
  AddSegment(const VertexType from, const VertexType to, ContourData & contourData);

  void
  FillOutputs(const std::vector<InputPixelType> &                        allLabels,
              std::unordered_map<InputPixelType, ContourContainerType> & labelsContoursOutput);

  InputPixelType m_UnusedLabel;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkContourExtractor2DImageFilter.hxx"
#endif

#endif
