/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMorphologicalSignedDistanceTransformImageFilter_h
#define itkMorphologicalSignedDistanceTransformImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkProgressReporter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkParabolicErodeImageFilter.h"
#include "itkParabolicDilateImageFilter.h"
#include "itkMorphSDTHelperImageFilter.h"

namespace itk
{
/**
 * \class MorphologicalSignedDistanceTransformImageFilter
 * \brief Signed distance transform of a mask using parabolic morphological
 * methods
 *
 * Morphological erosions using a parabolic structuring element can be
 * used to compute a distance transform of a mask by setting the
 * "Outside" value to 0 and the "inside" value to +infinity (or beyond
 * the maximum possible value). The
 * output of the parabolic erosion are the square of the distance to
 * the nearest zero valued voxel. Thus we can compute the distance
 * transform by taking the sqrt of the erosion.
 *
 * The output pixel type needs to support values as large as the
 * square of the largest value of the distance - just use float to be
 * safe.
 *
 * The inside is considered to have negative distances. Use
 * InsideIsPositive(bool) to change.
 *
 * There are also OutsideValue methods which can be used in similar
 * ways.
 *
 * Otherwise it is meant to have an interface to the other
 * DistanceTransforms filters.
 *
 * Core methods described in the InsightJournal article:
 * "Morphology with parabolic structuring elements"
 *
 * https://hdl.handle.net/1926/1370
 *
 *
 * \ingroup ParabolicMorphology
 *
 * \author Richard Beare, Monash University, Department of Medicine,
 * Melbourne, Australia. <Richard.Beare@monash.edu>
 *
 **/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT MorphologicalSignedDistanceTransformImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MorphologicalSignedDistanceTransformImageFilter);

  /** Standard class type alias. */
  using Self = MorphologicalSignedDistanceTransformImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MorphologicalSignedDistanceTransformImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputPixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<InputPixelType>::RealType;
  using ScalarRealType = typename NumericTraits<InputPixelType>::ScalarRealType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Image related type alias. */
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** a type to represent the "kernel radius" */
  using RadiusType = typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension>;

  virtual void
  Modified() const;

  /** this describes the input mask - default value 0 - we compute the
  distance from all voxels with value not equal to "OutsideValue" to
  the nearest voxel with value "OutsideValue" */
  itkSetMacro(OutsideValue, InputPixelType);
  itkGetConstReferenceMacro(OutsideValue, InputPixelType);

  /** Set On/Off whether spacing is used. */
  itkBooleanMacro(UseImageSpacing);

  /** Set if the inside represents positive values in the signed distance
   *  map. By convention ON pixels are treated as inside pixels.           */
  itkSetMacro(InsideIsPositive, bool);

  /** Get if the inside represents positive values in the signed distance map.
   *  See GetInsideIsPositive()  */
  itkGetConstReferenceMacro(InsideIsPositive, bool);

  /** Set if the inside represents positive values in the signed distance
   * map. By convention ON pixels are treated as inside pixels. Default is
   * true.                             */
  itkBooleanMacro(InsideIsPositive);
  /** Is the transform in world or voxel units - default is world */
  void
  SetUseImageSpacing(bool uis)
  {
    m_Erode->SetUseImageSpacing(uis);
    m_Dilate->SetUseImageSpacing(uis);
    this->Modified();
  }

  enum ParabolicAlgorithm
  {
    NOCHOICE = 0,     // decices based on scale - experimental
    CONTACTPOINT = 1, // sometimes faster at low scale
    INTERSECTION = 2  // default
  };

  /**
   * Set/Get the method used. Choices are contact point or
   * intersection. Intersection is the default. Contact point can be
   * faster at small scales. This is very unlikely to be the case for
   * a distance transform.
   */

  itkSetMacro(ParabolicAlgorithm, int);
  itkGetConstReferenceMacro(ParabolicAlgorithm, int);

  const bool &
  GetUseImageSpacing()
  {
    return m_Erode->GetUseImageSpacing();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimension,
                  (Concept::SameDimension<itkGetStaticConstMacro(InputImageDimension),
                                          itkGetStaticConstMacro(OutputImageDimension)>));

  itkConceptMacro(Comparable, (Concept::Comparable<InputPixelType>));

  /** End concept checking */
#endif
protected:
  MorphologicalSignedDistanceTransformImageFilter();
  virtual ~MorphologicalSignedDistanceTransformImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Generate Data */
  void
  GenerateData(void);

  int m_ParabolicAlgorithm;

  // do everything in the output image type, which should have high precision
  using ThreshType = typename itk::BinaryThresholdImageFilter<InputImageType, OutputImageType>;
  using ErodeType = typename itk::ParabolicErodeImageFilter<OutputImageType, OutputImageType>;
  using DilateType = typename itk::ParabolicDilateImageFilter<OutputImageType, OutputImageType>;
  using HelperType = typename itk::MorphSDTHelperImageFilter<OutputImageType, OutputImageType>;

private:
  InputPixelType               m_OutsideValue;
  bool                         m_InsideIsPositive;
  typename ErodeType::Pointer  m_Erode;
  typename DilateType::Pointer m_Dilate;
  typename ThreshType::Pointer m_Thresh;
  typename HelperType::Pointer m_Helper;
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologicalSignedDistanceTransformImageFilter.hxx"
#endif

#endif
