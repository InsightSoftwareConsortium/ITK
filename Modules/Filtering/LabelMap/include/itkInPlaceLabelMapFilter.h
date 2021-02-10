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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkInPlaceLabelMapFilter_h
#define itkInPlaceLabelMapFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
/**
 *\class InPlaceLabelMapFilter
 * \brief Base class for filters that takes an image as input and overwrites
 * that image as the output
 *
 * InPlaceLabelMapFilter is the base class for all process objects whose
 * output image data is constructed by overwriting the input image
 * data. In other words, the output bulk data is the same block of
 * memory as the input bulk data.  This filter provides the mechanisms
 * for in place image processing while maintaining general pipeline
 * mechanics. InPlaceLabelMapFilters use less memory than standard
 * ImageToImageFilters because the input buffer is reused as the
 * output buffer.  However, this benefit does not come without a cost.
 * Since the filter overwrites its input, the ownership of the bulk
 * data is transitioned from the input data object to the output data
 * object.  When a data object has multiple consumers with one
 * of the consumers being an in place filter, the in place filter
 * effectively destroys the bulk data for the data object. Upstream
 * filters will then have to re-execute to regenerate the data object's
 * bulk data for the remaining consumers.
 *
 * Since an InPlaceLabelMapFilter reuses the input bulk data memory for the
 * output bulk data memory, the input image type must match the output
 * image type.  If the input and output image types are not identical,
 * the filter reverts to a traditional ImageToImageFilter behaviour
 * where an output image is allocated.  In place operation can also be
 * controlled (when the input and output image type match) via the
 * methods InPlaceOn() and InPlaceOff().
 *
 * Subclasses of InPlaceLabelMapFilter must take extra care in how they
 * manage memory using (and perhaps overriding) the implementations of
 * ReleaseInputs() and AllocateOutputs() provided here.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://www.insight-journal.org/browse/publication/176
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 *  INRA de Jouy-en-Josas, France.
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT InPlaceLabelMapFilter : public LabelMapFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InPlaceLabelMapFilter);

  /** Standard class type aliases. */
  using Self = InPlaceLabelMapFilter;
  using Superclass = LabelMapFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InPlaceLabelMapFilter, LabelMapFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Superclass type alias. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using LabelObjectType = typename InputImageType::LabelObjectType;

  using PixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using RegionType = typename InputImageType::RegionType;

  using TOutputImage = TInputImage;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** In place operation can be turned on and off. This only has an
   * effect when the input and output image type match. */
  itkSetMacro(InPlace, bool);
  itkGetMacro(InPlace, bool);
  itkBooleanMacro(InPlace);

  /** Can the filter run in place? To do so, the filter's first input
   * and output must have the same dimension and pixel type. This
   * method can be used in conjunction with the InPlace ivar to
   * determine whether a particular use of the filter is really
   * running in place. Some filters may be able to optimize their
   * operation if the InPlace is true and CanRunInPlace is true. */
  bool
  CanRunInPlace() const
  {
    return true; // used to test if TInputImage == TOutputImage. But
                 // if you look above, the superclass declaration
                 // specifies
                 // LabelMapFilter<TInputImage,TOutputImage> so there's
                 // no way this couldn't be true.
  }

protected:
  InPlaceLabelMapFilter() = default;
  ~InPlaceLabelMapFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** The GenerateData method normally allocates the buffers for all
   * of the outputs of a filter. Since InPlaceLabelMapFilter's can use an
   * overwritten version of the input for its output, the output
   * buffer should not be allocated. When possible, we graft the input
   * to the filter to the output.  If an InPlaceFilter has multiple
   * outputs, then it would need to override this method to graft one
   * of its outputs and allocate the remaining. If a filter is
   * threaded (i.e. it provides an implementation of
   * ThreadedGenerateData()), this method is called automatically. If
   * an InPlaceFilter is not threaded (i.e. it provides an
   * implementation of GenerateData()), then this method (or
   * equivalent) must be called in GenerateData(). */
  void
  AllocateOutputs() override;

  /**
   * Return the output label collection image, instead of the input as in the default
   * implementation
   */
  InputImageType *
  GetLabelMap() override
  {
    return this->GetOutput();
  }

private:
  bool m_InPlace{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInPlaceLabelMapFilter.hxx"
#endif

#endif
