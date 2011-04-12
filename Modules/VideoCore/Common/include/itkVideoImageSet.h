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
#ifndef __itkVideoImageSet_h
#define __itkVideoImageSet_h

#include "itkRingBufferImageSet.h"
 
namespace itk
{
/** \class VideoImageSet
 *  \brief Templated ring buffer for streaming video
 *
 * This class inherits the bulk of its functionality from RingBufferImageSet
 * and adds some video specific methods
 */
template< class TPixel,
          unsigned int VImageDimension = 2,
          unsigned int VNumberOfBuffers = 3 >
class ITK_EXPORT VideoImageSet:
  public RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
{

public:

  /**-TYPEDEFS---------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef VideoImageSet                 Self;
  typedef RingBufferImageSet< TPixel,
    VImageDimension, VNumberOfBuffers > Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;
  typedef WeakPointer< const Self >     ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoImageSet, RingBufferImageSet);



  /**-PUBLIC METHODS---------------------------------------------------------*/

  /** Get the current position as frame, ratio, or MSec */
  unsigned long GetCurrentPositionFrame();
  double GetCurrentPositionRatio();
  double GetCurrentPositionMSec();

  /** Get number of frames */
  unsigned long GetNumberOfFrames();

  /** Get framerate */
  double GetFpS();


protected:

  /**-PROTECTED METHODS------------------------------------------------------*/
  VideoImageSet();
  virtual ~VideoImageSet();
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  VideoImageSet(const Self &); // purposely not implemented
  void operator=(const Self &);     // purposely not implemented


};  // end VideoImageSet class

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoImageSet.txx"
#endif

#endif
