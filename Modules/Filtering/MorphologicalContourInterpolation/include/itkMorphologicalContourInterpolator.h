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
#include "itksys/hash_map.hxx"
#include "itkExtractImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"

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

  /** Derived image typedefs. */
  typedef Image<bool, TImage::ImageDimension>                           BoolImageType;
  typedef Image<float, TImage::ImageDimension - 1>                      FloatSliceType;
  typedef Image<typename TImage::PixelType, TImage::ImageDimension - 1> SliceType;
  typedef Image<bool, TImage::ImageDimension - 1>                       BoolSliceType;

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
                        typename SliceType::Pointer     iconn,
                        typename SliceType::Pointer     jconn);

  /** If interpolation is done along more than one axis,
  the interpolations are merged using a modified "or" rule:
  -if all interpolated images have 0 for a given pixel, the output is 0
  -if just one image has a non-zero label, then that label is chosen
  -if more than one image has a non-zero label, last written label is chosen */
  void
  InterpolateAlong(int axis, TImage * out);

  /** Slice i has a region, slice j does not */
  void
  Extrapolate(int                             axis,
              TImage *                        out,
              typename TImage::PixelType      label,
              typename TImage::IndexValueType i,
              typename TImage::IndexValueType j,
              typename SliceType::Pointer     iConn,
              typename TImage::PixelType      iRegionId);

  typename FloatSliceType::Pointer
  MaurerDM(typename BoolSliceType::Pointer inImage);

  /** A sequence of conditional dilations starting with begin and reaching end.
  begin and end must cover the same region (size and index the same) */
  std::vector<typename BoolSliceType::Pointer>
  GenerateDilationSequence(typename BoolSliceType::Pointer begin, typename BoolSliceType::Pointer end);

  /** Finds an interpolating mask for these two aligned masks */
  typename BoolSliceType::Pointer
  FindMedianImageDilations(int                             axis,
                           typename BoolSliceType::Pointer intersection,
                           typename BoolSliceType::Pointer iMask,
                           typename BoolSliceType::Pointer jMask);

  /** Finds an interpolating mask for these two aligned masks */
  typename BoolSliceType::Pointer
  FindMedianImageDistances(int                             axis,
                           typename BoolSliceType::Pointer intersection,
                           typename BoolSliceType::Pointer iMask,
                           typename BoolSliceType::Pointer jMask);

  /** Build transition sequence and pick the median */
  void
  Interpolate1to1(int                             axis,
                  TImage *                        out,
                  typename TImage::PixelType      label,
                  typename TImage::IndexValueType i,
                  typename TImage::IndexValueType j,
                  typename SliceType::Pointer     iConn,
                  typename TImage::PixelType      iRegionId,
                  typename SliceType::Pointer     jConn,
                  typename TImage::PixelType      jRegionId,
                  typename SliceType::IndexType   translation,
                  bool                            recursive);

  typedef std::vector<typename TImage::PixelType> PixelList;

  /** Splits the bigger region and does N 1-to-1 interpolations */
  void
  Interpolate1toN(int                             axis,
                  TImage *                        out,
                  typename TImage::PixelType      label,
                  typename TImage::IndexValueType i,
                  typename TImage::IndexValueType j,
                  typename SliceType::Pointer     iConn,
                  typename TImage::PixelType      iRegionId,
                  typename SliceType::Pointer     jConn,
                  PixelList                       jRegionIds,
                  typename SliceType::IndexType   translation);

  typename SliceType::Pointer
  TranslateImage(typename SliceType::Pointer    image,
                 typename SliceType::IndexType  translation,
                 typename SliceType::RegionType newRegion);

  /** The returns cardingal of the symmetric distance between images.
  The images must cover the same region */
  IdentifierType
  CardSymDifference(typename BoolSliceType::Pointer shape1, typename BoolSliceType::Pointer shape2);

  /** Copied from ImageSource and changed to allocate a cleared buffer. */
  virtual void
  AllocateOutputs() ITK_OVERRIDE;

  /** Overwrites m_Output with non non-zeroes from m_Input. */
  void
  OverlayInput();

  /** Returns the centroid of given regions */
  typename SliceType::IndexType
  Centroid(typename SliceType::Pointer conn, PixelList regionIds);

  /** Calculates maximum intersection region for both slices given a translation.
  Both inputs are modified so that jRegion is a translated version of iRegion. */
  void
  IntersectionRegions(typename SliceType::IndexType    translation,
                      typename SliceType::RegionType & iRegion,
                      typename SliceType::RegionType & jRegion);

  /** Returns number of intersecting pixels */
  IdentifierType
  Intersection(typename SliceType::Pointer   iConn,
               typename TImage::PixelType    iRegionId,
               typename SliceType::Pointer   jConn,
               PixelList                     jRegionIds,
               typename SliceType::IndexType translation);

  /** How much j needs to be translated to best align with i */
  typename SliceType::IndexType
  Align(typename SliceType::Pointer iConn,
        typename TImage::PixelType  iRegionId,
        typename SliceType::Pointer jConn,
        PixelList                   jRegionIds);

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

  typename SliceType::RegionType
  BoundingBox(itk::SmartPointer<SliceType> image);

  // assumes both valid region and valid index
  // it can be invoked with 2D or 3D region, hence additional template parameter
  template <typename T2>
  void
  ExpandRegion(typename T2::RegionType & region, typename T2::IndexType index);

  typename SliceType::Pointer
  RegionedConnectedComponents(const typename TImage::RegionType region,
                              typename TImage::PixelType        label,
                              IdentifierType &                  objectCount);

  /** seed and mask must cover the same region (size and index the same) */
  typename BoolSliceType::Pointer
  Dilate1(typename BoolSliceType::Pointer seed, typename BoolSliceType::Pointer mask);

  typedef ExtractImageFilter<TImage, SliceType> RoiType;
  typename RoiType::Pointer                     m_RoI;

  typedef BinaryThresholdImageFilter<SliceType, BoolSliceType> BinarizerType;
  typename BinarizerType::Pointer                              m_Binarizer;

  typedef ConnectedComponentImageFilter<BoolSliceType, SliceType> ConnectedComponentsType;
  typename ConnectedComponentsType::Pointer                       m_ConnectedComponents;

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
