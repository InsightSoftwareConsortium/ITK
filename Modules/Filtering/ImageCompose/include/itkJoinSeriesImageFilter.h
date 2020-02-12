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
#ifndef itkJoinSeriesImageFilter_h
#define itkJoinSeriesImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class JoinSeriesImageFilter
 * \brief Join N-D images into an (N+1)-D image
 *
 * This filter is templated over the input image type and the output image
 * type. The pixel type of them must be the same and the input dimension
 * must be less than the output dimension.
 * When the input images are N-dimensional, they are joined in order and
 * the size of the N+1'th dimension of the output is same as the number of
 * the inputs. The spacing and the origin (where the first input is placed)
 * for the N+1'th dimension is specified in this filter. The output image
 * informations for the first N dimensions are taken from the first input.
 * Note that all the inputs should have the same information.
 *
 * \ingroup GeometricTransform
 * \ingroup MultiThreaded
 * \ingroup Streamed
 *
 * \author Hideaki Hiraki
 *
 * Contributed in the users list
 * http://public.kitware.com/pipermail/insight-users/2004-February/006542.html
 *
 * \ingroup ITKImageCompose
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT JoinSeriesImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JoinSeriesImageFilter);

  /** Standard class type aliases. */
  using Self = JoinSeriesImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JoinSeriesImageFilter, ImageToImageFilter);

  /** Compiler can't inherit type alias? */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Compiler can't inherit ImageDimension enumeration? */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get spacing of the new dimension */
  itkSetMacro(Spacing, double);
  itkGetConstMacro(Spacing, double);

  /** Set/Get origin of the new dimension */
  itkSetMacro(Origin, double);
  itkGetConstMacro(Origin, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  JoinSeriesImageFilter();
  ~JoinSeriesImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Override VerifyInputInformation() to add the additional check
   * that all inputs have the same number of components.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override;

  /** Overrides GenerateOutputInformation() in order to produce
   * an image which has a different information than the first input.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** Overrides GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model of different input requested regions
   * than the output requested region.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** JoinSeriesImageFilter can be implemented as a multithreaded filter.
   * \sa ImageSource::ThreadedGenerateData(),
   *     ImageSource::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  /** IndexValueType is used to switch among the inputs and
   * is used as the index value of the new dimension */
  using IndexValueType = unsigned int;

  double m_Spacing{ 1.0 };
  double m_Origin{ 0.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkJoinSeriesImageFilter.hxx"
#endif

#endif
