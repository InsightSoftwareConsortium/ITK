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
#ifndef __itkVideoToVideoFilter_h
#define __itkVideoToVideoFilter_h

#include "itkVideoSource.h"
#include "itkVideoStream.h"

namespace itk
{

/** \class VideoToVideoFilter
 * \brief Base class for filters that use a VideoStream as input and output
 *
 * VideoToVideoFilter is the base class for all process objects that output
 * VideoStream data and requre VideoStream data as input. This class defines
 * the SetInput() method for setting the input to a filter.
 *
 * An implementation of GenerateInputRequestedRegion() is provided here that
 * uses the implementation from TemporalProcessObject to generate input
 * temporal regions and then provides its own mechanism for generating input
 * spatial regions. The default implementation simply takes the requested
 * spatial region from the first frame of output and uses that as the requested
 * region for each of the input frames.
 */
template< class TInputVideoStream, class TOutputVideoStream >
class ITK_EXPORT VideoToVideoFilter : public VideoSource< TOutputVideoStream >
{
public:

  /*-TYPEDEFS----------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef TInputVideoStream                           InputVideoStreamType;
  typedef TOutputVideoStream                          OutputVideoStreamType;
  typedef VideoToVideoFilter< InputVideoStreamType,
                              OutputVideoStreamType > Self;
  typedef VideoSource< OutputVideoStreamType >        Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef WeakPointer< const Self >                   ConstWeakPointer;

  /** Superclass typedefs */
  typedef typename Superclass::OutputFrameType              OutputFrameType;
  typedef typename Superclass::OutputFrameSpatialRegionType OutputFrameSpatialRegionType;

  /** Input typedefs */
  typedef typename InputVideoStreamType::FrameType         InputFrameType;
  typedef typename InputVideoStreamType::SpatialRegionType InputFrameSpatialRegionType;
  typedef typename InputVideoStreamType::IndexType         InputFrameIndexType;
  typedef typename InputVideoStreamType::PixelType         InputFramePixelType;
  typedef typename InputVideoStreamType::PointType         InputFramePointType;
  typedef typename InputVideoStreamType::SpacingType       InputFrameSpacingType;
  typedef typename InputVideoStreamType::SizeType          InputFrameSizeType;
  typedef typename InputVideoStreamType::DirectionType     InputFrameDirectionType;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoToVideoFilter, VideoSource);

  /*-PUBLIC METHODS----------------------------------------------------------*/

  /** Set the input VideoStream for this temporal process object */
  virtual void SetInput( const InputVideoStreamType* videoStream);
  virtual void SetInput( unsigned int idx, const InputVideoStreamType* videoStream);

  /** Get the input VideoSream for this temporal process object */
  const InputVideoStreamType* GetInput() const;
  const InputVideoStreamType* GetInput(unsigned int idx) const;

protected:

  /** Extend the default implementation of GenerateInputRequestedRegion from
   * TemporalProcessObject to propagate spatial regions as well as temporal
   * regions. This default implementation takes the requested spatial region
   * from the first requested output frame and applies it to all of the
   * requested input frames. */
  virtual void GenerateInputRequestedRegion();

  VideoToVideoFilter();
  virtual ~VideoToVideoFilter() {};
  virtual void PrintSelf(std::ostream & os, Indent indent) const
    { Superclass::Print(os, indent); };


private:

  VideoToVideoFilter(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class VideoToVideoFilter

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoToVideoFilter.txx"
#endif

#endif
