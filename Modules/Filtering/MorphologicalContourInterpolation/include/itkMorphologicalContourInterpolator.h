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

namespace itk
{
template <class TImage>
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

  /** Run-time type information (and related methods). */
  itkTypeMacro(MorphologicalContourInterpolator, ImageToImageFilter);

protected:
  MorphologicalContourInterpolator()
    : m_Label(0)
    , m_Axis(-1)
  {}
  ~MorphologicalContourInterpolator() {}

  typename TImage::PixelType m_Label;
  int                        m_Axis;

  /** Does the real work. */
  virtual void
  GenerateData() ITK_OVERRIDE;

  /** If there is a pixel whose all 4-way neighbors belong the the same label
  except along one axis, and along that axis its neighbors are 0 (background),
  then that axis should be interpolated along. Interpolation is possible
  along more than one axis. */
  void
  DetermineSliceOrientations();

  /** If interpolation is done along more than one axis,
  the interpolations are merged using a modified "or" rule:
  -if all interpolated images have 0 for a given pixel, the output is 0
  -if just one image has a non-zero label, then that label is chosen
  -if more than one image has a non-zero label, median label is chosen */
  void
  InterpolateAlong(int axis, typename TImage * out);

  typedef itk::FixedArray<bool, TImage::ImageDimension>                 OrientationType;
  typedef itksys::hash_map<typename TImage::PixelType, OrientationType> OrientationsType;

  typedef std::map<typename TImage::PixelType, typename TImage::RegionType> BoundingBoxesType;
  OrientationsType                                                          m_Orientations;
  BoundingBoxesType                                                         m_BoundingBoxes;

  // assumes both valid region and valid index
  void
  ExpandRegion(typename TImage::RegionType & region, typename TImage::IndexType index);

private:
  MorphologicalContourInterpolator(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologicalContourInterpolator.hxx"
#endif


#endif // itkMorphologicalContourInterpolator_h
