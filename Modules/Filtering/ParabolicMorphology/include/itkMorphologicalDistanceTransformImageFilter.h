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
#ifndef itkMorphologicalDistanceTransformImageFilter_h
#define itkMorphologicalDistanceTransformImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkProgressReporter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkParabolicErodeImageFilter.h"
#include "itkSqrtImageFilter.h"

namespace itk
{
/**
 * \class MorphologicalDistanceTransformImageFilter
 * \brief Distance transform of a mask using parabolic morphological
 * methods
 *
 * Morphological erosions using a parabolic structuring element can be
 * used to compute a distance transform of a mask by setting the
 * "Outside" value to 0 and the "inside" value to +infinity. The
 * output of the parabolic erosion are the square of the distance to
 * the nearest zero valued voxel. Thus we can compute the distance
 * transform by taking the sqrt of the erosion.
 *
 * The output pixel type needs to support values as large as the
 * square of the largest value of the distance - just use float to be
 * safe.
 *
 * Core methods described in the InsightJournal article:
 * "Morphology with parabolic structuring elements"
 *
 * http://hdl.handle.net/1926/1370
 *
 * \ingroup ParabolicMorphology
 *
 * \author Richard Beare, Monash University, Department of Medicine,
 * Melbourne, Australia. <Richard.Beare@monash.edu>
 *
 **/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT MorphologicalDistanceTransformImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MorphologicalDistanceTransformImageFilter);

  /** Standard class type alias. */
  using Self = MorphologicalDistanceTransformImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MorphologicalDistanceTransformImageFilter, ImageToImageFilter);

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
  void
  Modified() const override;

  /** this describes the input mask - default value 0 - we compute the
  distance from all voxels with value not equal to "OutsideValue" to
  the nearest voxel with value "OutsideValue" */
  itkSetMacro(OutsideValue, InputPixelType);
  itkGetConstReferenceMacro(OutsideValue, InputPixelType);

  /** Is the transform in world or voxel units - default is world */
  void
  SetUseImageSpacing(bool uis)
  {
    m_Erode->SetUseImageSpacing(uis);
  }

  const bool &
  GetUseImageSpacing()
  {
    return m_Erode->GetUseImageSpacing();
  }

  itkSetMacro(SqrDist, bool);
  itkGetConstReferenceMacro(SqrDist, bool);
  itkBooleanMacro(SqrDist);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimension,
                  (Concept::SameDimension<itkGetStaticConstMacro(InputImageDimension),
                                          itkGetStaticConstMacro(OutputImageDimension)>));

  itkConceptMacro(Comparable, (Concept::Comparable<InputPixelType>));

  /** End concept checking */
#endif
protected:
  MorphologicalDistanceTransformImageFilter();
  ~MorphologicalDistanceTransformImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  // do everything in the output image type, which should have high precision
  using ThreshType = typename itk::BinaryThresholdImageFilter<InputImageType, OutputImageType>;
  using ErodeType = typename itk::ParabolicErodeImageFilter<OutputImageType, OutputImageType>;
  using SqrtType = typename itk::SqrtImageFilter<OutputImageType, OutputImageType>;

private:
  InputPixelType               m_OutsideValue;
  typename ErodeType::Pointer  m_Erode;
  typename ThreshType::Pointer m_Thresh;
  typename SqrtType::Pointer   m_Sqrt;
  bool                         m_SqrDist;
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologicalDistanceTransformImageFilter.hxx"
#endif

#endif
