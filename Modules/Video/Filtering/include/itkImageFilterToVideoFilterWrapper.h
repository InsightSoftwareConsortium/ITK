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
#ifndef itkImageFilterToVideoFilterWrapper_h
#define itkImageFilterToVideoFilterWrapper_h

#include "itkVideoToVideoFilter.h"

namespace itk
{

/** \class ImageFilterToVideoFilterWrapper
 * \brief Wrap an ImageToImageFilter as a VideoToVideoFilter that operates on
 * a single frame at a time
 *
 * This filter wrapper allows all of the standard ITK image filters to be used
 * in a video pipeline. This is done by instantiating the image filter, setting
 * its parameters, and then using the SetImageFilter() method of this wrapper
 * to use the filter to process each in a video pipeline. An instance of this
 * wrapper must be templated over the appropriate image filter type.
 *
 * \ingroup ITKVideoFiltering
 */
template<typename TImageToImageFilter>
class ITK_TEMPLATE_EXPORT ImageFilterToVideoFilterWrapper :
  public VideoToVideoFilter<
          itk::VideoStream<typename TImageToImageFilter::InputImageType>,
          itk::VideoStream<typename TImageToImageFilter::OutputImageType> >
{
public:

  /** Standard class typedefs */
  typedef TImageToImageFilter                                ImageFilterType;
  typedef typename ImageFilterType::InputImageType           InputFrameType;
  typedef typename ImageFilterType::OutputImageType          OutputFrameType;
  typedef itk::VideoStream< InputFrameType >                 InputVideoStreamType;
  typedef itk::VideoStream< OutputFrameType >                OutputVideoStreamType;

  typedef ImageFilterToVideoFilterWrapper< ImageFilterType > Self;
  typedef VideoToVideoFilter< InputVideoStreamType,
                              OutputVideoStreamType >        Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;
  typedef WeakPointer< const Self >                          ConstWeakPointer;

  itkNewMacro(Self);

  itkTypeMacro(ImageFilterToVideoFilterWrapper, VideoToVideoFilter);

  /** Set the filter to use in the interal pipeline */
  itkSetObjectMacro(ImageFilter, ImageFilterType);
  itkGetModifiableObjectMacro(ImageFilter, ImageFilterType);

protected:

  /** Constructor and Destructor */
  ImageFilterToVideoFilterWrapper();
  virtual ~ImageFilterToVideoFilterWrapper() ITK_OVERRIDE {}

  /** PrintSelf */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Since we just set up a mini image pipeline inside, we override
   * TemporalStreamingGenerateData*/
  virtual void TemporalStreamingGenerateData() ITK_OVERRIDE;

  /** Pointer to filter to use for internal filter */
  typename ImageFilterType::Pointer m_ImageFilter;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageFilterToVideoFilterWrapper);


};  // end class ImageFilterToVideoFilterWrapper

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFilterToVideoFilterWrapper.hxx"
#endif

#endif
