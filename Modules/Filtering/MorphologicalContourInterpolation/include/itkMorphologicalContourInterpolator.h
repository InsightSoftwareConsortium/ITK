#ifndef __itkMorphologicalContourInterpolator_h
#define __itkMorphologicalContourInterpolator_h

#include "itkImageToImageFilter.h"
#include <map>
#include <array>

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
  {
    this->m_Label = 0;
    this->m_Axis = -1;
  }
  ~MorphologicalContourInterpolator() {}

  typename TImage::PixelType m_Label;
  int                        m_Axis;

  /** Does the real work. */
  virtual void
  GenerateData();

  void
  DetermineSliceOrientations();
  void
  InterpolateAlong(int axis, typename TImage::Pointer out);

  typedef std::array<bool, TImage::ImageDimension>              OrientationType;
  typedef std::map<typename TImage::PixelType, OrientationType> OrientationsType;

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


#endif // __itkMorphologicalContourInterpolator_h
