#ifndef __itkMorphologicalContourInterpolator_h
#define __itkMorphologicalContourInterpolator_h

#include "itkImageToImageFilter.h"

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

  /** Run-time type information (and related methods). */
  itkTypeMacro(MorphologicalContourInterpolator, ImageToImageFilter);

protected:
  MorphologicalContourInterpolator() { this->m_Label = 0; }
  ~MorphologicalContourInterpolator() {}

  typename TImage::PixelType m_Label;

  /** Does the real work. */
  virtual void
  GenerateData();

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
