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
#ifndef __itkTemporalDataObject_h
#define __itkTemporalDataObject_h

#include "itkDataObject.h"
#include "itkRingBuffer.h"
#include "itkTemporalRegion.h"

namespace itk
{

/** \class TemporalDataObject
 * \brief DataObject subclass with knowledge of temporal region
 *
 * This class represents a data object that relies on temporal regions. It uses
 * an itk::RingBuffer to store DataObject pointers in sequential order. The
 * pointers in the ring buffer should correspond to the BufferedTemporalRegion.
 * The LargestPossibleTemporalRegion should indicate the maximum extent that
 * data object is logically capable of holding, and the RequestedTemporalRegion
 * is used in the pipeline to request that a certain temporal region be
 * buffered
 *
 * \ingroup Video-Core-Common
 */
class ITK_EXPORT TemporalDataObject : public DataObject
{
public:

  /** Standard class typedefs */
  typedef TemporalDataObject                  Self;
  typedef DataObject                          Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef WeakPointer< const Self >           ConstWeakPointer;

  typedef RingBuffer<DataObject>              BufferType;
  typedef TemporalRegion                      TemporalRegionType;

  /** Enum for defining the way in which to compare temporal regions */
  typedef enum {Frame, RealTime, FrameAndRealTime} TemporalUnitType;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TemporalDataObject, DataObject);

  /** Get the type of temporal units we care about (Defaults to Frame)*/
  virtual TemporalUnitType GetTemporalUnit() const
  { return m_TemporalUnit; }

  /** Explicity set temporal units (Defaults to Frame)*/
  virtual void SetTemporalUnitToFrame()
  { m_TemporalUnit = Frame; }
  virtual void SetTemporalUnitToRealTime()
  { m_TemporalUnit = RealTime; }
  virtual void SetTemporalUnitToFrameAndRealTime()
  { m_TemporalUnit = FrameAndRealTime; }

  /** Get/Set the number of frames that the internal buffer can hold */
  unsigned long GetNumberOfBuffers()
    { return m_DataObjectBuffer->GetNumberOfBuffers(); }
  void SetNumberOfBuffers(unsigned long num)
    { m_DataObjectBuffer->SetNumberOfBuffers(num); }

  virtual void SetLargestPossibleTemporalRegion(
    const TemporalRegionType & region)
  {
    m_LargestPossibleTemporalRegion = region;
    this->Modified();
  }
  virtual const TemporalRegionType & GetLargestPossibleTemporalRegion() const
  { return m_LargestPossibleTemporalRegion; }

  virtual void SetBufferedTemporalRegion(const TemporalRegionType & region)
  {
    m_BufferedTemporalRegion = region;
    this->Modified();
  }
  virtual const TemporalRegionType & GetBufferedTemporalRegion() const
  { return m_BufferedTemporalRegion; }

  virtual void SetRequestedTemporalRegion(const TemporalRegionType & region)
  {
    m_RequestedTemporalRegion = region;
    this->Modified();
  }
  virtual const TemporalRegionType & GetRequestedTemporalRegion() const
  { return m_RequestedTemporalRegion; }

  /** Get the portion of the requested region that is not covered by the
   * buffered region */
  virtual const TemporalRegionType GetUnbufferedRequestedTemporalRegion();

  virtual void SetRequestedRegionToLargestPossibleRegion();

  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();

  virtual bool VerifyRequestedRegion();

  virtual void CopyInformation(const DataObject *);

  virtual void SetRequestedRegion(DataObject *);

  virtual void Graft(const DataObject *);

protected:

  TemporalDataObject();
  virtual ~TemporalDataObject();
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  /** Buffer for holding component data objects */
  BufferType::Pointer m_DataObjectBuffer;

  /** We want to keep track of our regions in time. **/
  TemporalRegionType m_LargestPossibleTemporalRegion;
  TemporalRegionType m_RequestedTemporalRegion;
  TemporalRegionType m_BufferedTemporalRegion;

  TemporalUnitType m_TemporalUnit;

private:

  TemporalDataObject(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class TemporalDataObject

} // end namespace itk

#endif
