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
#ifndef itkImageToVideoFilter_h
#define itkImageToVideoFilter_h

#include "itkImage.h"
#include "itkVideoSource.h"
#include "itkVideoStream.h"
#include "itkTemporalRegion.h"
#include "itkMacro.h"

namespace itk
{

/**
 * \class ImageToVideoFilter
 * \brief Converts Image to VideoStream representation with a temporal axis
 *
 * ImageToVideoFilter is a ProcessObject for converting an Image or VectorImage to a VideoStream.
 * The user provides an input image of at least two dimensions and specifies one axis to interpret
 * as the temporal axis. Image slices along the designated axis are grafted into
 * video frames in the output VideoStream. The filter assumes that temporal samples are axis-aligned
 * (time cannot be rotated) and the temporal accessor index matches its spatial index (no permutation).
 *
 * Other than the specified temporal axis, axis order is preserved and orientation and spacing
 * information is copied over to each VideoStream frame from the original Image.
 * Image orientation is understood as having LPS (left-posterior-superior) interpretation.
 *
 * ImageToVideoFilter inherits from VideoSource to indicate that its output is a VideoStream.
 * However, many methods are overridden to properly handle Image rather than VideoStream
 * input to ImageToVideoFilter.
 *
 * \ingroup ITKVideoCore
 */
template <typename TInputImage,
          typename TOutputVideoStream =
            VideoStream<Image<typename TInputImage::PixelType, TInputImage::ImageDimension - 1>>>
class ITK_TEMPLATE_EXPORT ImageToVideoFilter : public VideoSource<TOutputVideoStream>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToVideoFilter);

  /** Standard class type aliases */
  using InputImageType = TInputImage;
  using OutputVideoStreamType = TOutputVideoStream;
  using Self = ImageToVideoFilter<InputImageType, OutputVideoStreamType>;
  using Superclass = VideoSource<OutputVideoStreamType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Output type alias */
  using OutputFrameType = typename Superclass::OutputFrameType;
  using OutputFrameSpatialRegionType = typename Superclass::OutputFrameSpatialRegionType;
  using OutputTemporalRegionType = typename TOutputVideoStream::TemporalRegionType;

  /** Input type alias */
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageType::IndexType;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToVideoFilter, VideoSource);

  /** Set the input Image for this process object */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputImageType * image);

  virtual void
  SetInput(unsigned int idx, const InputImageType * videoStream);

  /** Get the input Image for this process object */
  const InputImageType *
  GetInput() const;

  const InputImageType *
  GetInput(unsigned int idx) const;

  /** Allow the user to specify the axis in the input image that will
   *  correspond to the temporal axis in the output temporal object. */
  itkGetMacro(FrameAxis, IndexValueType);
  itkSetMacro(FrameAxis, IndexValueType);

protected:
  ImageToVideoFilter();
  ~ImageToVideoFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Get a non-const version of the input for internal use when setting
   * input's requested regions. This is the only time input should be modified
   */
  InputImageType *
  GetInput();

  InputImageType *
  GetInput(unsigned int idx);

  /** Set up the output VideoStream via spatial and temporal regions
   * derived from the spatial regions of the input Image.
   */
  void
  GenerateOutputInformation() override;

  /** Override the Superclass::UpdateOutputInformation method
   * so that the temporal output region is defined entirely within
   * the GenerateOutputInformation method.
   */
  void
  UpdateOutputInformation() override;

  /** Generate the requested regions in the output VideoStream
   *  from the size of the available input Image.
   */
  void
  GenerateOutputRequestedRegion(DataObject * output) override;

  /** Override the default implementation of GenerateInputRequestedRegion from
   * VideoSource so that we only get spatial regions from the image input.
   */
  void
  GenerateInputRequestedRegion() override;

  /** Graft pixel data from input image onto output video frames */
  void
  GenerateData() override;

private:
  /** Index representing the axis accessor index to use for slicing the input Image
   *  into frames for the output VideoStream.
   *  Default to slowest-moving axis in the input Image.
   */
  IndexValueType m_FrameAxis{ TInputImage::ImageDimension - 1 };

}; // end class ImageToVideoFilter

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToVideoFilter.hxx"
#endif

#endif // itkImageToVideoFilter_h
