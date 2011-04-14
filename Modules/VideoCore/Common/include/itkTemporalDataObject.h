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

namespace itk
{

class RingBuffer;
class TemporalRegion;

/** \class TemporalDataObject
 * \brief DataObject subclass with knowledge of temporal region
 *
 */
class ITK_EXPORT TemporalDataObject:public DataObject
{

  /** Standard class typedefs */
  typedef TemporalDataObject                  Self;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef WeakPointer< const Self >           ConstWeakPointer;

  typedef RingBuffer<DataObject>              BufferType;
  typedef TemporalRegion                      TemporalRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TemporalDataObject, DataObject);

  /** Function for getting the size of the underlying ring buffer. */
  virtual BufferType::SizeValueType GetBufferSize() const;

  virtual void SetLargestPossibleTemporalRegion(
    const TemporalRegionType & region);
  virtual const TemporalRegionType & GetLargestPossibleTemporalRegion() const
  { return m_LargestPossibleTemporalRegion; }

  virtual void SetBufferedTemporalRegion(const TemporalRegionType & region);
  virtual const TemporalRegionType & GetBufferedTemporalRegion() const
  { return m_BufferedTemporalRegion; }

  virtual void SetRequestedTemporalRegion(const TemporalRegionType & region);
  virtual const TemporalRegionType & GetRequestedTemporalRegion() const
  { return m_RequestedTemporalRegion; }

  virtual void SetRequestedRegionToLargestPossibleRegion();

  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();

  virtual bool VerifyRequestedRegion();

  virtual void CopyInformation(const DataObject *);

  virtual void SetRequestedRegion(DataObject *);

  virtual void Graft(const DataObject *);

protected:

  TemporalDataObject();
  ~TemporalDataObject();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Buffer for holding component data objects */
  BufferType::Pointer m_DataObjectBuffer;

  /** We want to keep track of our regions in time. **/
  TemporalRegionType::Pointer m_LargestPossibleTemporalRegion;
  TemporalRegionType::Pointer m_RequestedTemporalRegion;
  TemporalRegionType::Pointer m_BufferedTemporalRegion;

private:

  TemporalDataObject(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class TemporalDataObject

} // end namespace itk

#endif
