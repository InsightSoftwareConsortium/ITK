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
#ifndef __itkVideoStream_h
#define __itkVideoStream_h

#include "itkTemporalDataObject.h"
#include "itkImage.h"

namespace itk
{

/** \class VideoStream
 * \brief A DataObject that holds a buffered portion of a video
 *
 * The function of VideoStream is to provide an Image-specific subclass of
 * TemporalDataObject. It provides several convenient typedefs to get common
 * attributes of the frames.
 */
template<class TFrameType>
class ITK_EXPORT VideoStream : public TemporalDataObject
{
public:

  /** Standard class typedefs */
  typedef VideoStream                Self;
  typedef TemporalDataObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;

  typedef TFrameType                 FrameType;
  typedef RingBuffer<FrameType>      BufferType;

  typedef typename FrameType::RegionType    SpatialRegionType;
  typedef typename FrameType::IndexType     IndexType;
  typedef typename FrameType::PixelType     PixelType;
  typedef typename FrameType::PointType     PointType;
  typedef typename FrameType::SpacingType   SpacingType;
  typedef typename FrameType::SizeType      SizeType;
  typedef typename FrameType::DirectionType DirectionType;

  /** Type used to store map between frame numbers and spatial regions */
  typedef typename std::map<unsigned long, SpatialRegionType> SpatialRegionMapType;

  /** Access the spacial dimensionality of the frames */
  itkStaticConstMacro(FrameDimension, unsigned int, FrameType::ImageDimension);
  static unsigned int GetFrameDimension()
    { return FrameType::ImageDimension; }

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoStream, TemporalDataObject);

  /** Initialize any empty frames. This method makes sure that the frame buffer
   * is large enough to hold the number of frames needed for the buffered
   * temporal region. It goes through the necessary number of frames making
   * sure that each one has been initialized. When allocating space for frames,
   * this method should be called first, followed by setting the spatial
   * regions on each frame, before Allocate is called. */
  void InitializeEmptyFrames();

  /** Provide access to the internal frame buffer object */
  BufferType* GetFrameBuffer()
    { return reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer()); }
  const BufferType* GetFrameBuffer() const
    { return reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer()); }

  /** Set the internal pixel buffer */
  void SetFrameBuffer(BufferType* buffer);


  /** Provide access to the internal caches for the spatial regions */
  const SpatialRegionMapType & GetLargestPossibleSpatialRegionCache() const
    { return m_LargestPossibleSpatialRegionCache; }
  void SetLargestPossibleSpatialRegionCache(SpatialRegionMapType map)
    { m_LargestPossibleSpatialRegionCache = map; }

  const SpatialRegionMapType & GetRequestedSpatialRegionCache() const
    { return m_RequestedSpatialRegionCache; }
  void SetRequestedSpatialRegionCache(SpatialRegionMapType map)
    { m_RequestedSpatialRegionCache = map; }

  const SpatialRegionMapType & GetBufferedSpatialRegionCache() const
    { return m_BufferedSpatialRegionCache; }
  void SetBufferedSpatialRegionCache(SpatialRegionMapType map)
    { m_BufferedSpatialRegionCache = map; }

  /** Set the contents of the frame at a given frame number */
  void SetFrame(unsigned long frameNumber, FrameType* frame);

  /** Get the frame for the given frame number. Internally, we always leave the
   * Head of the ring buffer in place and just use the frame number as an
   * offset. This allows all references to frames to be processed by an
   * explicit frame number rather than a potentially confusing offset. */
  FrameType* GetFrame(unsigned long frameNumber);
  const FrameType* GetFrame(unsigned long frameNumber) const;

  /** Get/Set the LargestPossibleRegion of a frame */
  void SetFrameLargestPossibleSpatialRegion(unsigned long frameNumber,
                                            SpatialRegionType region);
  const SpatialRegionType &
  GetFrameLargestPossibleSpatialRegion(unsigned long frameNumber) const;

  /** Get/Set the RequestedRegion of a frame */
  void SetFrameRequestedSpatialRegion(unsigned long frameNumber,
                                      SpatialRegionType region);
  const SpatialRegionType &
  GetFrameRequestedSpatialRegion(unsigned long frameNumber) const;

  /** Get/Set the BufferedRegion of a frame */
  void SetFrameBufferedSpatialRegion(unsigned long frameNumber,
                                     SpatialRegionType region);
  const SpatialRegionType &
  GetFrameBufferedSpatialRegion(unsigned long frameNumber) const;

  /** Set the LargestPossibleRegion on all frames. This assumes that all frames
   * in the buffered temporal region have been initialized (should be called
   * after InitializeEmptyFrames). */
  void SetAllLargestPossibleSpatialRegions(SpatialRegionType region);

  /** Set the RequestedRegion on all frames. This assumes that all frames in
   * the buffered temporal region have been initialized (should be called
   * after InitializeEmptyFrames). */
  void SetAllRequestedSpatialRegions(SpatialRegionType region);

  /** Set the BufferedRegion on all frames. This assumes that all frames in the
   * buffered temporal region have been initialized (should be called after
   * InitializeEmptyFrames). */
  void SetAllBufferedSpatialRegions(SpatialRegionType region);

  /** Allocate memory for the buffered spatial region of each frame in the
   * buffered temporal region. This assumes that all frames in the buffered
   * temporal region have been initialized and that the buffered spatial region
   * has been set for each of these frames. A typical setup would look like:
   *
   * \code
   *    // Set the temporal regions
   *    TemporalRegionType temporalRegion;
   *    temporalRegion.SetFrameStart( 0 );
   *    temporalRegion.SetFrameDuration( 3 );
   *    video->SetLargestPossibleTemporalRegion( temporalRegion );
   *    video->SetRequestedTemporalRegion( temporalRegion );
   *    video->SetBufferedTemporalRegion( temporalRegion );
   *
   *    // Initialize all frames in the buffered temporal region
   *    video->InitializeEmptyFrames();
   *
   *    // Set the buffered spatial region for each frame
   *    SpatialRegionType bufferedSpatialRegion;
   *    SpatialRegionType::SizeType size;
   *    SpatialRegionType::IndexType start;
   *    size[0] = 50;
   *    size[1] = 40;
   *    start.Fill( 0 );
   *    bufferedSpatialRegion.SetSize( size );
   *    bufferedSpatialRegion.SetIndex( start );
   *    video->SetAllBufferedSpatialRegions( bufferedSpatialRegion );
   *
   *    // Allocate memory for the frames
   *    video->Allocate();
   * \endcode
   */
  void Allocate();


  /** Graft the data and information from one VideoStream to this one. This
   * just copies the meta information using TemporalProcessObject's Graft then
   * sets the internal RingBuffer pointer to point to the same buffer used by
   * the other VideoStream. */
  virtual void Graft(const DataObject* data);

protected:

  VideoStream() {};
  virtual ~VideoStream() {};
  virtual void PrintSelf(std::ostream & os, Indent indent) const
    { Superclass::Print(os, indent); }


  /** These maps are used to cache a mapping between frame number and spatial
   * region. This is done because frames will often not be in actual existance
   * at the time when the region gets set. */
  SpatialRegionMapType m_LargestPossibleSpatialRegionCache;
  SpatialRegionMapType m_RequestedSpatialRegionCache;
  SpatialRegionMapType m_BufferedSpatialRegionCache;

private:

  VideoStream(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class VideoStream

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoStream.txx"
#endif

#endif
