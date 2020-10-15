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
#ifndef itkNeighborhoodConnectedImageFilter_h
#define itkNeighborhoodConnectedImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class NeighborhoodConnectedImageFilter
 * \brief Label pixels that are connected to a seed and lie within a neighborhood
 *
 * NeighborhoodConnectedImageFilter labels pixels with ReplaceValue that
 * are connected to an initial Seed AND whose neighbors all lie within a
 * Lower and Upper threshold range.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKRegionGrowing
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NeighborhoodConnectedImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NeighborhoodConnectedImageFilter);

  /** Standard class type aliases. */
  using Self = NeighborhoodConnectedImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(NeighborhoodConnectedImageFilter, ImageToImageFilter);

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using InputImageSizeType = typename InputImageType::SizeType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clear the seeds */
  void
  ClearSeeds();

  /** Set seed point. */
  void
  SetSeed(const IndexType & seed);

  /** Add a seed point */
  void
  AddSeed(const IndexType & seed);

  /** Set/Get the lower threshold. The default is 0. */
  itkSetMacro(Lower, InputImagePixelType);
  itkGetConstMacro(Lower, InputImagePixelType);

  /** Set/Get the upper threshold. The default is the largest possible
   *  value for the InputPixelType. */
  itkSetMacro(Upper, InputImagePixelType);
  itkGetConstMacro(Upper, InputImagePixelType);

  /** Set/Get value to replace thresholded pixels. Pixels that lie *
   *  within Lower and Upper (inclusive) will be replaced with this
   *  value. The default is 1. */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetConstMacro(ReplaceValue, OutputImagePixelType);

  /** Set the radius of the neighborhood used for a mask. */
  itkSetMacro(Radius, InputImageSizeType);

  /** Get the radius of the neighborhood used to compute the median */
  itkGetConstReferenceMacro(Radius, InputImageSizeType);

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  NeighborhoodConnectedImageFilter();
  ~NeighborhoodConnectedImageFilter() override = default;
  std::vector<IndexType> m_Seeds;

  InputImagePixelType m_Lower;
  InputImagePixelType m_Upper;

  OutputImagePixelType m_ReplaceValue;

  InputImageSizeType m_Radius;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNeighborhoodConnectedImageFilter.hxx"
#endif

#endif
