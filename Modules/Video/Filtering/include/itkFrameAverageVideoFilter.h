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
#ifndef itkFrameAverageVideoFilter_h
#define itkFrameAverageVideoFilter_h

#include "itkVideoToVideoFilter.h"

namespace itk
{

/** \class FrameAverageVideoFilter
 * \brief Average frames over a designated range in a video
 *
 * This filter computes the average of X frames at once from an input video. It
 * processes one frame forward at a time.
 *
 * \ingroup ITKVideoFiltering
 */
template<typename TInputVideoStream, typename TOutputVideoStream>
class ITK_TEMPLATE_EXPORT FrameAverageVideoFilter :
  public VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>
{
public:

  /** Standard class typedefs */
  typedef TInputVideoStream                                InputVideoStreamType;
  typedef TOutputVideoStream                               OutputVideoStreamType;
  typedef FrameAverageVideoFilter< InputVideoStreamType,
                                   OutputVideoStreamType > Self;
  typedef VideoToVideoFilter< InputVideoStreamType,
                              OutputVideoStreamType >      Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;
  typedef WeakPointer< const Self >                        ConstWeakPointer;

  typedef typename TInputVideoStream::FrameType  InputFrameType;
  typedef typename InputFrameType::PixelType     InputPixelType;
  typedef typename InputFrameType::RegionType    InputFrameSpatialRegionType;
  typedef typename TOutputVideoStream::FrameType OutputFrameType;
  typedef typename OutputFrameType::PixelType    OutputPixelType;
  typedef typename OutputFrameType::RegionType   OutputFrameSpatialRegionType;

  itkNewMacro(Self);

  itkTypeMacro(FrameAverageVideoFilter, VideoToVideoFilter);

  /** Get/Set the number of frames to average over */
  void SetNumberOfFrames(SizeValueType numFrames);
  SizeValueType GetNumberOfFrames();

protected:

  /** Constructor and Destructor */
  FrameAverageVideoFilter();
  virtual ~FrameAverageVideoFilter() ITK_OVERRIDE {}

  /** PrintSelf */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** FrameAverageVideoFilter is implemented as a temporal streaming and
   * spatially multithreaded filter, so we override ThreadedGenerateData */
  virtual void ThreadedGenerateData(
                const OutputFrameSpatialRegionType& outputRegionForThread,
                int threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FrameAverageVideoFilter);


};  // end class FrameAverageVideoFilter

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFrameAverageVideoFilter.hxx"
#endif

#endif
