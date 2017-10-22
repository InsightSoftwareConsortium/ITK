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
#ifndef itkDecimateFramesVideoFilter_h
#define itkDecimateFramesVideoFilter_h

#include "itkVideoToVideoFilter.h"

namespace itk
{

/** \class DecimateFramesVideoFilter
 * \brief Reduce a video's frame-rate by keeping every Nth frame
 *
 * This filter simply takes an input video and passes every Nth frame through
 * to the output.
 *
 * \ingroup ITKVideoFiltering
 */
template<typename TVideoStream>
class ITK_TEMPLATE_EXPORT DecimateFramesVideoFilter :
  public VideoToVideoFilter<TVideoStream, TVideoStream>
{
public:

  /** Standard class typedefs */
  typedef TVideoStream                                 VideoStreamType;
  typedef TVideoStream                                 InputVideoStreamType;
  typedef TVideoStream                                 OutputVideoStreamType;
  typedef DecimateFramesVideoFilter< VideoStreamType > Self;
  typedef VideoToVideoFilter< VideoStreamType,
                              VideoStreamType >        Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef WeakPointer< const Self >                    ConstWeakPointer;

  typedef typename TVideoStream::FrameType FrameType;
  typedef typename FrameType::PixelType    PixelType;
  typedef typename FrameType::RegionType   FrameSpatialRegionType;

  itkNewMacro(Self);

  itkTypeMacro(DecimateFramesVideoFilter, VideoToVideoFilter);

  /** Get/Set the spacing of the preserved frames */
  void SetPreservedFrameSpacing(SizeValueType numFrames);
  SizeValueType GetPreservedFrameSpacing();

protected:

  /** Constructor and Destructor */
  DecimateFramesVideoFilter();
  virtual ~DecimateFramesVideoFilter() ITK_OVERRIDE {}

  /** PrintSelf */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** DecimateFramesVideoFilter is implemented as a temporal streaming and
   * spatially multithreaded filter, so we override ThreadedGenerateData */
  virtual void ThreadedGenerateData(
                const FrameSpatialRegionType& outputRegionForThread,
                int threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DecimateFramesVideoFilter);


};  // end class DecimateFramesVideoFilter

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDecimateFramesVideoFilter.hxx"
#endif

#endif
