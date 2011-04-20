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
template<class TImageType>
class ITK_EXPORT VideoStream : public TemporalDataObject
{
public:

  /** Standard class typedefs */
  typedef VideoStream                Self;
  typedef TemporalDataObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;

  typedef TImageType                 ImageType;
  typedef RingBuffer<ImageType>      BufferType;

  typedef typename ImageType::RegionType    SpatialRegionType;
  typedef typename ImageType::IndexType     IndexType;
  typedef typename ImageType::PixelType     PixelType;
  typedef typename ImageType::PointType     PointType;
  typedef typename ImageType::SpacingType   SpacingType;
  typedef typename ImageType::SizeType      SizeType;
  typedef typename ImageType::DirectionType DirectionType;

  /** Access the spacial dimensionality of the frames */
  itkStaticConstMacro(FrameDimension, unsigned int, ImageType::ImageDimension);
  static unsigned int GetFrameDimension()
    { return ImageType::ImageDimension; }

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoStream, TemporalDataObject);

protected:

  VideoStream() {};
  virtual ~VideoStream() {};
  virtual void PrintSelf(std::ostream & os, Indent indent) const
    { Superclass::Print(os, indent); };

private:

  VideoStream(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class VideoStream

} // end namespace itk

#endif
