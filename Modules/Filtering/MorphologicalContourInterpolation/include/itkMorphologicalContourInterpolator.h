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
#ifndef itkMorphologicalContourInterpolator_h
#define itkMorphologicalContourInterpolator_h

#include "itkImageToImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itksys/hash_map.hxx"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryCrossStructuringElement.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkAndImageFilter.h"
#include "itkOrImageFilter.h"
#include "ThreadPool.h"


namespace itk
{

template <typename TImage>
class MorphologicalContourInterpolator : public ImageToImageFilter<TImage, TImage>
{
public:
  /** Standard class typedefs. */
  typedef MorphologicalContourInterpolator   Self;
  typedef ImageToImageFilter<TImage, TImage> Superclass;
  typedef SmartPointer<Self>                 Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Interpolate only this label. Interpolates all labels if set to 0 (default). */
  itkSetMacro(Label, typename TImage::PixelType);

  /** Which label is interpolated. 0 means all labels (default). */
  itkGetMacro(Label, typename TImage::PixelType);

  /** Which label is interpolated. 0 means all labels (default). */
  itkGetConstMacro(Label, typename TImage::PixelType);

  /** Interpolate only along this axis. Interpolates along all axes if set to -1 (default). */
  itkSetMacro(Axis, int);

  /** Axis of interpolation. -1 means interpolation along all axes (default). */
  itkGetMacro(Axis, int);

  /** Axis of interpolation. -1 means interpolation along all axes (default). */
  itkGetConstMacro(Axis, int);

  /** Heuristic alignment of regions for interpolation is faster than optimal alignment.
   *   Default is heuristic. */
  itkSetMacro(HeuristicAlignment, bool);

  /** Heuristic alignment of regions for interpolation is faster than optimal alignment.
   *   Default is heuristic. */
  itkGetMacro(HeuristicAlignment, bool);

  /** Heuristic alignment of regions for interpolation is faster than optimal alignment.
   *   Default is heuristic. */
  itkGetConstMacro(HeuristicAlignment, bool);

  /** Using distance transform instead of repeated dilations to calculate median contour is faster.
   *   Default is to use distance transform. */
  itkSetMacro(UseDistanceTransform, bool);

  /** Using distance transform instead of repeated dilations to calculate median contour is faster.
   *   Default is to use distance transform. */
  itkGetMacro(UseDistanceTransform, bool);

  /** Using distance transform instead of repeated dilations to calculate median contour is faster.
   *   Default is to use distance transform. */
  itkGetConstMacro(UseDistanceTransform, bool);

  /** Use ball instead of default cross structuring element for repeated dilations. */
  void
  SetUseBallStructuringElement(bool useBall)
  {
    m_UseBallStructuringElement = useBall;
    m_ConnectedComponents->SetFullyConnected(useBall);
  }

  /** Use ball instead of default cross structuring element for repeated dilations. */
  itkGetMacro(UseBallStructuringElement, bool);

  /** Use ball instead of default cross structuring element for repeated dilations. */
  itkGetConstMacro(UseBallStructuringElement, bool);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MorphologicalContourInterpolator, ImageToImageFilter);

protected:
  MorphologicalContourInterpolator();
  ~MorphologicalContourInterpolator() {}

  typename TImage::PixelType m_Label;
  int                        m_Axis;
  bool                       m_HeuristicAlignment;
  bool                       m_UseDistanceTransform;
  bool                       m_UseBallStructuringElement;
  bool                       m_StopSpawning;  // stop spawning new threads
  IdentifierType             m_MinAlignIters; // minimum number of iterations in align method
  IdentifierType             m_MaxAlignIters; // maximum number of iterations in align method
  ::ThreadPool *             m_ThreadPool;    // avoid name conflict

  // grafted input and output to prevent unnecessary pipeline modification checks
  typename TImage::Pointer m_Input;
  typename TImage::Pointer m_Output;

  /** Does the real work. */
  virtual void
  GenerateData() ITK_OVERRIDE;

  /** If there is a pixel whose all 4-way neighbors belong the the same label
  except along one axis, and along that axis its neighbors are 0 (background),
  then that axis should be interpolated along. Interpolation is possible
  along more than one axis. */
  void
  DetermineSliceOrientations();

  void
  InterpolateBetweenTwo(int                             axis,
                        TImage *                        out,
                        typename TImage::PixelType      label,
                        typename TImage::IndexValueType i,
                        typename TImage::IndexValueType j,
                        typename TImage::Pointer        iconn,
                        typename TImage::Pointer        jconn);

  /** If interpolation is done along more than one axis,
  the interpolations are merged using a modified "or" rule:
  -if all interpolated images have 0 for a given pixel, the output is 0
  -if just one image has a non-zero label, then that label is chosen
  -if more than one image has a non-zero label, median label is chosen */
  void
  InterpolateAlong(int axis, TImage * out);

  /** Slice i has a region, slice j does not */
  void
  Extrapolate(int                             axis,
              TImage *                        out,
              typename TImage::PixelType      label,
              typename TImage::IndexValueType i,
              typename TImage::IndexValueType j,
              typename TImage::Pointer        iConn,
              typename TImage::PixelType      iRegionId);

  typedef Image<bool, TImage::ImageDimension>                           BoolImageType;
  typedef Image<float, TImage::ImageDimension>                          FloatImageType;
  typedef Image<typename TImage::PixelType, TImage::ImageDimension - 1> SliceType;
  typedef Image<bool, TImage::ImageDimension - 1>                       BoolSliceType;

  typename FloatImageType::Pointer
  MaurerDM(typename BoolImageType::Pointer inImage);

  /** A sequence of conditional dilations starting with begin and reaching end.
  begin and end must cover the same region (size and index the same) */
  std::vector<typename BoolImageType::Pointer>
  GenerateDilationSequence(typename BoolImageType::Pointer begin, typename BoolImageType::Pointer end, int axis);

  /** Finds an interpolating mask for these two aligned masks */
  typename BoolImageType::Pointer
  FindMedianImageDilations(int axis, typename BoolImageType::Pointer iMask, typename BoolImageType::Pointer jMask);

  /** Finds an interpolating mask for these two aligned masks */
  typename BoolImageType::Pointer
  FindMedianImageDistances(int axis, typename BoolImageType::Pointer iMask, typename BoolImageType::Pointer jMask);

  /** Build transition sequence and pick the median */
  void
  Interpolate1to1(int                             axis,
                  TImage *                        out,
                  typename TImage::PixelType      label,
                  typename TImage::IndexValueType i,
                  typename TImage::IndexValueType j,
                  typename TImage::Pointer        iConn,
                  typename TImage::PixelType      iRegionId,
                  typename TImage::Pointer        jConn,
                  typename TImage::PixelType      jRegionId,
                  typename TImage::IndexType      translation,
                  bool                            recursive);

  typedef std::vector<typename TImage::PixelType> PixelList;

  /** Splits the bigger region and does N 1-to-1 interpolations */
  void
  Interpolate1toN(int                             axis,
                  TImage *                        out,
                  typename TImage::PixelType      label,
                  typename TImage::IndexValueType i,
                  typename TImage::IndexValueType j,
                  typename TImage::Pointer        iConn,
                  typename TImage::PixelType      iRegionId,
                  typename TImage::Pointer        jConn,
                  PixelList                       jRegionIds,
                  typename TImage::IndexType      translation);

  typename TImage::Pointer
  TranslateImage(typename TImage::Pointer    image,
                 typename TImage::IndexType  translation,
                 typename TImage::RegionType newRegion);

  /** The returns cardingal of the symmetric distance between images.
  The images must cover the same region */
  IdentifierType
  CardSymDifference(typename BoolImageType::Pointer shape1, typename BoolImageType::Pointer shape2);

  /** Writes into m_Output.
  Copies non-zeroes from m_Input, and fills zeroes from interpolate. */
  void
  CombineInputAndInterpolate(typename TImage::Pointer interpolate);

  /** Returns the centroid of given regions */
  typename TImage::IndexType
  Centroid(typename TImage::Pointer conn, PixelList regionIds);

  /** Calculates maximum intersection region for both slices given a translation.
  Both inputs are modified so that jRegion is a translated version of iRegion. */
  void
  IntersectionRegions(typename TImage::IndexType    translation,
                      typename TImage::RegionType & iRegion,
                      typename TImage::RegionType & jRegion);

  /** Returns number of intersecting pixels */
  IdentifierType
  Intersection(typename TImage::Pointer   iConn,
               typename TImage::PixelType iRegionId,
               typename TImage::Pointer   jConn,
               PixelList                  jRegionIds,
               typename TImage::IndexType translation);

  /** How much j needs to be translated to best align with i */
  typename TImage::IndexType
  Align(int                        axis,
        typename TImage::Pointer   iConn,
        typename TImage::PixelType iRegionId,
        typename TImage::Pointer   jConn,
        PixelList                  jRegionIds);

  typedef FixedArray<bool, TImage::ImageDimension>                      OrientationType;
  typedef itksys::hash_map<typename TImage::PixelType, OrientationType> OrientationsType;
  OrientationsType                                                      m_Orientations;

  typedef itksys::hash_map<typename TImage::PixelType, typename TImage::RegionType> BoundingBoxesType;
  BoundingBoxesType m_BoundingBoxes; // bounding box for each label

  // each label gets a set of slices in which it is present
  typedef std::set<typename TImage::IndexValueType>                  SliceSetType;
  typedef itksys::hash_map<typename TImage::PixelType, SliceSetType> LabeledSlicesType;
  std::vector<LabeledSlicesType>                                     m_LabeledSlices; // one for each axis

  void
  SetLabeledSliceIndices(unsigned int axis, std::vector<typename TImage::IndexValueType> indices);
  void
  SetLabeledSliceIndices(unsigned int axis, SliceSetType indices);
  SliceSetType
  GetLabeledSliceIndices(unsigned int axis);

  // assumes both valid region and valid index
  void
  ExpandRegion(typename TImage::RegionType & region, typename TImage::IndexType index);

  typename TImage::Pointer
  RegionedConnectedComponents(const typename TImage::RegionType region,
                              typename TImage::PixelType        label,
                              IdentifierType &                  objectCount);

  /** seed and mask must cover the same region (size and index the same) */
  typename BoolImageType::Pointer
  Dilate1(typename BoolImageType::Pointer seed, typename BoolImageType::Pointer mask, int axis);

  typedef ExtractImageFilter<TImage, TImage> RoiType;
  typename RoiType::Pointer                  m_RoI;

  typedef BinaryThresholdImageFilter<TImage, BoolImageType> BinarizerType;
  typename BinarizerType::Pointer                           m_Binarizer;

  typedef ConnectedComponentImageFilter<BoolImageType, TImage> ConnectedComponentsType;
  typename ConnectedComponentsType::Pointer                    m_ConnectedComponents;

  typedef BinaryCrossStructuringElement<bool, TImage::ImageDimension> CrossStructuringElementType;
  typedef BinaryBallStructuringElement<bool, TImage::ImageDimension>  BallStructuringElementType;

  typedef BinaryDilateImageFilter<BoolImageType, BoolImageType, CrossStructuringElementType> CrossDilateType;
  typedef BinaryDilateImageFilter<BoolImageType, BoolImageType, BallStructuringElementType>  BallDilateType;

  typedef AndImageFilter<BoolImageType, BoolImageType, BoolImageType> AndFilterType;

  typedef OrImageFilter<BoolImageType> OrType;

  class MatchesID
  {
    typename TImage::PixelType m_ID;

  public:
    MatchesID() {}
    MatchesID(typename TImage::PixelType id)
      : m_ID(id)
    {}
    bool
    operator!=(const MatchesID & other)
    {
      return m_ID != other.m_ID;
    }
    bool
    operator==(const MatchesID & other)
    {
      return m_ID == other.m_ID;
    }
    inline bool
    operator()(const typename TImage::PixelType & val) const
    {
      return val == m_ID;
    }
  };

private:
  MorphologicalContourInterpolator(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;
};
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologicalContourInterpolator.hxx"
#endif

#endif // itkMorphologicalContourInterpolator_h
