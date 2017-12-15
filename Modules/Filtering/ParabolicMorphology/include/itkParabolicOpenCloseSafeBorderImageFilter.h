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
#ifndef itkParabolicOpenCloseSafeBorderImageFilter_h
#define itkParabolicOpenCloseSafeBorderImageFilter_h

#include "itkParabolicOpenCloseImageFilter.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkStatisticsImageFilter.h"

/* this class implements padding and cropping, so we don't just
 * inherit from the OpenCloseImageFitler */

namespace itk
{
template <typename TInputImage, bool doOpen, typename TOutputImage = TInputImage>
class ITK_EXPORT ParabolicOpenCloseSafeBorderImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ParabolicOpenCloseSafeBorderImageFilter       Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ParabolicOpenCloseSafeBorderImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                            InputImageType;
  typedef TOutputImage                                           OutputImageType;
  typedef typename TInputImage::PixelType                        InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType       RealType;
  typedef typename NumericTraits<InputPixelType>::ScalarRealType ScalarRealType;
  typedef typename TOutputImage::PixelType                       OutputPixelType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension> RadiusType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);
  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  // set all of the scales the same
  void
  SetScale(ScalarRealType scale)
  {
    RadiusType s = this->GetScale();

    this->m_MorphFilt->SetScale(scale);
    if (s != this->GetScale())
    {
      this->Modified();
    }
  }

  // different scale for each direction
  void
  SetScale(RadiusType scale)
  {
    if (scale != this->GetScale())
    {
      this->m_MorphFilt->SetScale(scale);
      this->Modified();
    }
  }

  //
  const RadiusType &
  GetScale() const
  {
    return (this->m_MorphFilt->GetScale());
  }

  void
  SetUseImageSpacing(bool B)
  {
    if (B != this->GetUseImageSpacing())
    {
      this->m_MorphFilt->SetUseImageSpacing(B);
      this->Modified();
    }
  }

  bool
  GetUseImageSpacing() const
  {
    return (this->m_MorphFilt->GetUseImageSpacing());
  }

  itkBooleanMacro(UseImageSpacing);

  itkSetMacro(SafeBorder, bool);
  itkGetConstReferenceMacro(SafeBorder, bool);
  itkBooleanMacro(SafeBorder);
  // should add the Get methods

  enum ParabolicAlgorithm
  {
    NOCHOICE = 0,     // decices based on scale - experimental
    CONTACTPOINT = 1, // sometimes faster at low scale
    INTERSECTION = 2  // default
  };
  /**
   * Set/Get the method used. Choices are contact point or
   * intersection. Intersection is the default. Contact point can be
   * faster at small scales.
   */

  itkSetMacro(ParabolicAlgorithm, int);
  itkGetConstReferenceMacro(ParabolicAlgorithm, int);

  /** ParabolicOpenCloseImageFilter must forward the Modified() call to its
    internal filters */
  virtual void
  Modified() const ITK_OVERRIDE;

protected:
  void
  GenerateData() ITK_OVERRIDE;

  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef ParabolicOpenCloseImageFilter<TInputImage, doOpen, TOutputImage> MorphFilterType;
  typedef ConstantPadImageFilter<TInputImage, TInputImage>                 PadFilterType;
  typedef CropImageFilter<TOutputImage, TOutputImage>                      CropFilterType;
  typedef StatisticsImageFilter<InputImageType>                            StatsFilterType;

  ParabolicOpenCloseSafeBorderImageFilter()
  {
    m_MorphFilt = MorphFilterType::New();
    m_PadFilt = PadFilterType::New();
    m_CropFilt = CropFilterType::New();
    m_StatsFilt = StatsFilterType::New();
    m_SafeBorder = true;
    m_ParabolicAlgorithm = INTERSECTION;
  }

  virtual ~ParabolicOpenCloseSafeBorderImageFilter() {}
  int m_ParabolicAlgorithm;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParabolicOpenCloseSafeBorderImageFilter);

  typename MorphFilterType::Pointer m_MorphFilt;
  typename PadFilterType::Pointer   m_PadFilt;
  typename CropFilterType::Pointer  m_CropFilt;
  typename StatsFilterType::Pointer m_StatsFilt;

  bool m_SafeBorder;
  bool m_UseContactPoint;
  bool m_UseIntersection;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkParabolicOpenCloseSafeBorderImageFilter.hxx"
#endif

#endif
