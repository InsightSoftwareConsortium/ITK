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
#ifndef itkVideoSource_h
#define itkVideoSource_h

#include "itkTemporalProcessObject.h"
#include "itkVideoStream.h"

namespace itk
{

/** \class VideoSource
 * \brief A TemporalProcessObject that produces a VideoStream
 *
 * VideoSource acts as the base class for all TemporalProcess objects that
 * produce a VideoStream as their output. This class defines GetOutput() which
 * returns a pointer to the VideoStream object on output port 0.
 *
 * The other roll that VideoSource plays is to implement the framework for
 * spatial streaming to complement the temporal streaming implemented in
 * TemporalProcessObject. This implementation mirrors the implementation in
 * ImageSource except that each thread will be able to operate on a spatial
 * region of each frame in the current temporal region that is being processed.
 *
 * \ingroup ITKVideoCore
 */
template< typename TOutputVideoStream >
class ITK_TEMPLATE_EXPORT VideoSource : public TemporalProcessObject
{
public:

  /*-TYPEDEFS----------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef VideoSource                Self;
  typedef TemporalProcessObject      Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;
  typedef TOutputVideoStream         OutputVideoStreamType;

  typedef typename OutputVideoStreamType::FrameType         OutputFrameType;
  typedef typename OutputVideoStreamType::SpatialRegionType OutputFrameSpatialRegionType;
  typedef typename OutputVideoStreamType::IndexType         OutputFrameIndexType;
  typedef typename OutputVideoStreamType::PixelType         OutputFramePixelType;
  typedef typename OutputVideoStreamType::PointType         OutputFramePointType;
  typedef typename OutputVideoStreamType::SpacingType       OutputFrameSpacingType;
  typedef typename OutputVideoStreamType::SizeType          OutputFrameSizeType;
  typedef typename OutputVideoStreamType::DirectionType     OutputFrameDirectionType;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoSource, TemporalProcessObject);

  /*-PUBLIC METHODS----------------------------------------------------------*/

  /** Access the spacial dimensionality of the frames */
  itkStaticConstMacro(OutputFrameDimension, unsigned int, OutputFrameType::ImageDimension);
  static unsigned int GetOutputFrameDimension()
  {
    return OutputFrameType::ImageDimension;
  }

  /** GetOutput() returns a pointer to the VideoStream on output port 0. This
   * is analogous to the GetOutput on ImageSource and will be used to pass
   * VideoStream data between TemporalProcessObjects (typically video filters)
   */
  OutputVideoStreamType* GetOutput();

  OutputVideoStreamType* GetOutput(unsigned int idx);

  /** Graft the provided VideoStream onto the TemporalProcessObject's idx'th
   * output using VideoStream's Graft implementation */
  virtual void GraftNthOutput(unsigned int idx, OutputVideoStreamType* output);

  /** Graft the provided VideoStream onto the 0th output. This is useful for
   * ImageSources that use a mini-pipeline internally. See comment in
   * ImageSource for more information. */
  virtual void GraftOutput(OutputVideoStreamType* output);

  /** Make a DataObject of the correct type for the specified output port. The
   * default always creates an OutputVideoStreamType object, so subclasses with
   * multiple types of output must override this to return the proper type. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

protected:

  /** Override GenerateOutputRequestedTemporalRegion to make sure that if the
   * requested temporal region is not set, it gets set to the largest possible
   * temporal region (using TemporalProcessObject's implementation) and the
   * output has enough frame buffer slots to hold the entire request. We don't
   * want the number of buffers to be changed for mid-pipeline filters since
   * they will usually operate on fewer frames than requested at any given
   * time. The solution is to only change the number of buffers if the
   * requested temporal region was unset at the beginning of the call since
   * mid-pipeline filters will always have their outputs' requested spatial
   * regions set by the GenerateInputRequestedRegion call from the filter one
   * further down the pipeline. */
  virtual void GenerateOutputRequestedTemporalRegion(TemporalDataObject* output) ITK_OVERRIDE;

  /** We override the default implementation of TemporalStreamingGenerateData
   * from TemporalProcessObject to provide functionality for spatial streaming.
   * This implementation works exactly the same way as the implementation of
   * GenerateData in ImageSource. The filter-specific implementation of
   * ThreadedGenerateData will be responsible of accessing the correct frames. */
  virtual void TemporalStreamingGenerateData() ITK_OVERRIDE;

  /** ThreadedGenerateData here serves the same symnatic purpose as
   * ThreadedGenerateData in ProcessObjects that handle Images. This is to say
   * that ThreadedGenerateData is in charge of performing a single algorithmic
   * operation on a portion of the requested region. The difference will come
   * in the implementation of each filter. Subclass implementations must access
   * the passed spatial region across the desired number of frames in the
   * buffered TemporalRegion of the input.
   *
   * WARNING: subclasses may need to handle the case where different frames
   * have different spatial dimensions and thus some frames may not be
   * accessible at the requested spatial region. */
  virtual void ThreadedGenerateData(
    const OutputFrameSpatialRegionType& outputRegionForThread,
    int threadId);

  /** The GenerateData method normally allocates the buffers for all of the
   * outputs of a filter. Some filters may want to override this default
   * behavior. For example, a filter may have multiple outputs with
   * varying resolution. Or a filter may want to process data in place by
   * grafting its input to its output. */
  virtual void AllocateOutputs();

  /** Method that gets called before threads are dispatched from
   * TemporalStreamingGeneratData */
  virtual void BeforeThreadedGenerateData() {
  }

  /** Method that gets called after all threads finish in
   * TemporalStreamingGenerateData */
  virtual void AfterThreadedGenerateData() {
  }

  /** This method functions like SplitRequestedRegion in ImageSource and is
   * used by the implementation of TemporalStreamingGenerateData to dispatch
   * threads that call ThreadedGenerateData. */
  virtual int SplitRequestedSpatialRegion(int i, int num,
                                          OutputFrameSpatialRegionType& splitRegion);

  /** Static thread callback function for the MultiThreader. This gives control
   * to ThreadedGenerateData(). */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback(void* arg);

  /** Internal structure used for passing image data into the threading library
    */
  struct ThreadStruct {
    Pointer Filter;
    };

  VideoSource();
  virtual ~VideoSource() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(VideoSource);

};  // end class VideoSource

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoSource.hxx"
#endif

#endif
