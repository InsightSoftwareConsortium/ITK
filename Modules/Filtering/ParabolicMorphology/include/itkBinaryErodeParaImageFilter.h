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
#ifndef itkBinaryErodeParaImageFilter_h
#define itkBinaryErodeParaImageFilter_h

#include "itkParabolicErodeImageFilter.h"
#include "itkGreaterEqualValImageFilter.h"

namespace itk
{
/**
 * \class BinaryErodeParaImageFilter
 * \brief Class for binary morphological erosion operation.
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
 * Also note that the inputs must be 0/1 not 0/max for pixel type.
 *
 * This filter was developed as a result of discussions with
 * M.Starring on the ITK mailing list.
 *
 * Core methods described in the InsightJournal article:
 * "Morphology with parabolic structuring elements"
 *
 * http://hdl.handle.net/1926/1370
 *
 * \sa itkParabolicErodeImageFilter
 *
 *
 * \ingroup ParabolicMorphology
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 **/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT BinaryErodeParaImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>

{
public:
  /** Standard class typedefs. */
  typedef BinaryErodeParaImageFilter                    Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryErodeParaImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                       InputImageType;
  typedef TOutputImage                                      OutputImageType;
  typedef typename TInputImage::PixelType                   PixelType;
  typedef typename NumericTraits<PixelType>::RealType       RealType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;
  typedef typename TOutputImage::PixelType                  OutputPixelType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  typedef typename NumericTraits<PixelType>::FloatType InternalRealType;
  // perhaps a bit dodgy, change to int if you want to do enormous
  // binary operations
  typedef short InternalIntType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  typedef typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension> RadiusType;

  void
  SetRadius(ScalarRealType radius);

  itkSetMacro(Radius, RadiusType);
  itkGetConstReferenceMacro(Radius, RadiusType);

  void
  SetUseImageSpacing(bool g)
  {
    m_RectPara->SetUseImageSpacing(g);
    m_CircPara->SetUseImageSpacing(g);
  }

  /**
   * Set/Get whether the erosion is circular/rectangular -
   * default is true (circular)
   */
  itkSetMacro(Circular, bool);
  itkGetConstReferenceMacro(Circular, bool);
  itkBooleanMacro(Circular);
  /** Image related typedefs. */

  /* add in the traits here */
  virtual void
  Modified() const;

protected:
  void
  GenerateData(void);

  BinaryErodeParaImageFilter();
  virtual ~BinaryErodeParaImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  typedef typename itk::Image<InternalRealType, InputImageType::ImageDimension>            InternalRealImageType;
  typedef typename itk::Image<InternalIntType, InputImageType::ImageDimension>             InternalIntImageType;
  typedef typename itk::ParabolicErodeImageFilter<TInputImage, InternalRealImageType>      CircParabolicType;
  typedef typename itk::ParabolicErodeImageFilter<TInputImage, InternalIntImageType>       RectParabolicType;
  typedef typename itk::GreaterEqualValImageFilter<InternalRealImageType, OutputImageType> CCastType;
  typedef typename itk::GreaterEqualValImageFilter<InternalIntImageType, OutputImageType>  RCastType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryErodeParaImageFilter);

  RadiusType m_Radius;
  bool       m_Circular;

  typename CircParabolicType::Pointer m_CircPara;
  typename CCastType::Pointer         m_CircCast;

  typename RectParabolicType::Pointer m_RectPara;
  typename RCastType::Pointer         m_RectCast;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryErodeParaImageFilter.hxx"
#endif

#endif //__itkBinaryErodeParaImageFilter_h
