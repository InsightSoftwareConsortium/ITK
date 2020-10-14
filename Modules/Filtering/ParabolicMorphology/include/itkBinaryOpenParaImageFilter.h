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
#ifndef itkBinaryOpenParaImageFilter_h
#define itkBinaryOpenParaImageFilter_h

#include "itkParabolicErodeImageFilter.h"
#include "itkParabolicDilateImageFilter.h"
#include "itkGreaterEqualValImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"

namespace itk
{
/**
 * \class BinaryOpenParaImageFilter
 * \brief Class for binary morphological opening operation.
 *
 * This class uses the parabolic morphology operations to do very
 * efficient erosions by circles/spheres. The operations are efficient
 * because the underlying parabolic operations are separable and
 * the operations are implicitly short circuited in comparison to a
 * full distance transform approach.
 *
 * The basic idea is that a binary erosion or dilation by a circle/sphere
 * can be carried out by thresholding a distance transform. By using
 * the parabolic filters we can avoid computing the entire distance
 * transform and instead only compute the subset we are interested
 * in.
 *
 * Note that the circles and spheres may not be quite what you
 * expect, because this class doesn't explicitly use Bresenham circles
 * as most of the others do. A voxel's centre needs to be less than or
 * equal to the circle radius, rather than any part of the voxel
 * inside the circle.
 *
 * This filter was developed as a result of discussions with
 * M.Starring on the ITK mailing list.
 *
 * Also note that the inputs must be 0/1 not 0/max for pixel type.
 *
 * Core methods described in the InsightJournal article:
 * "Morphology with parabolic structuring elements"
 *
 * http://hdl.handle.net/1926/1370
 * \sa itkParabolicErodeImageFilter
 *
 *
 * \ingroup ParabolicMorphology
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 **/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT BinaryOpenParaImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryOpenParaImageFilter);

  /** Standard class type alias. */
  using Self = BinaryOpenParaImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryOpenParaImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::RealType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  using InternalRealType = typename NumericTraits<PixelType>::FloatType;
  // perhaps a bit dodgy, change to int if you want to do enormous
  // binary operations
  using InternalIntType = short;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using RadiusType = typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension>;

  void
  SetRadius(ScalarRealType radius);

  itkSetMacro(Radius, RadiusType);
  itkGetConstReferenceMacro(Radius, RadiusType);

  void
  SetUseImageSpacing(bool g)
  {
    m_RectErode->SetUseImageSpacing(g);
    m_RectDilate->SetUseImageSpacing(g);
    m_CircErode->SetUseImageSpacing(g);
    m_CircDilate->SetUseImageSpacing(g);
  }

  /**
   * Set/Get whether the erosion is circular/rectangular -
   * default is true (circular)
   */
  itkSetMacro(Circular, bool);
  itkGetConstReferenceMacro(Circular, bool);
  itkBooleanMacro(Circular);

  /** A safe border is added to input image to avoid borders effects
   * and remove it once the closing is done */
  itkSetMacro(SafeBorder, bool);
  itkGetConstReferenceMacro(SafeBorder, bool);
  itkBooleanMacro(SafeBorder);

  /** Image related type alias. */

  /* add in the traits here */
protected:
  void
  GenerateData() override;

  BinaryOpenParaImageFilter();
  ~BinaryOpenParaImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using InternalRealImageType = typename itk::Image<InternalRealType, InputImageType::ImageDimension>;
  using InternalIntImageType = typename itk::Image<InternalIntType, InputImageType::ImageDimension>;
  using CircErodeType = typename itk::ParabolicErodeImageFilter<TInputImage, InternalRealImageType>;
  using RectErodeType = typename itk::ParabolicErodeImageFilter<TInputImage, InternalIntImageType>;
  using CircDilateType = typename itk::ParabolicDilateImageFilter<OutputImageType, InternalRealImageType>;
  using RectDilateType = typename itk::ParabolicDilateImageFilter<OutputImageType, InternalRealImageType>;

  using CCastTypeA = typename itk::GreaterEqualValImageFilter<InternalRealImageType, OutputImageType>;
  using CCastTypeB = typename itk::BinaryThresholdImageFilter<InternalRealImageType, OutputImageType>;

  using RCastTypeA = typename itk::GreaterEqualValImageFilter<InternalIntImageType, OutputImageType>;
  using RCastTypeB = typename itk::BinaryThresholdImageFilter<InternalRealImageType, OutputImageType>;

private:
  RadiusType m_Radius;
  bool       m_Circular;
  bool       m_SafeBorder;

  typename CircErodeType::Pointer  m_CircErode;
  typename CircDilateType::Pointer m_CircDilate;

  typename CCastTypeA::Pointer m_CircCastA;
  typename CCastTypeB::Pointer m_CircCastB;

  typename RectErodeType::Pointer  m_RectErode;
  typename RectDilateType::Pointer m_RectDilate;

  typename RCastTypeA::Pointer m_RectCastA;
  typename RCastTypeB::Pointer m_RectCastB;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryOpenParaImageFilter.hxx"
#endif

#endif //__itkBinaryOpenParaImageFilter_h
