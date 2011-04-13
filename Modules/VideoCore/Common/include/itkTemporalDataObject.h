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

  /** Set the RequestedRegion to the LargestPossibleRegion.  This
   * forces a filter to produce all of the output in one execution
   * (i.e. not streaming) on the next call to Update(). */
  virtual void SetRequestedRegionToLargestPossibleRegion();

  /** Determine whether the RequestedRegion is outside of the
   * BufferedRegion. This method returns true if the RequestedRegion
   * is outside the BufferedRegion (true if at least one pixel is
   * outside). This is used by the pipeline mechanism to determine
   * whether a filter needs to re-execute in order to satisfy the
   * current request.  If the current RequestedRegion is already
   * inside the BufferedRegion from the previous execution (and the
   * current filter is up to date), then a given filter does not need
   * to re-execute */
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion();

  /** Verify that the RequestedRegion is within the LargestPossibleRegion.
   *
   * If the RequestedRegion is not within the LargestPossibleRegion,
   * then the filter cannot possibly satisfy the request. This method
   * returns true if the request can be satisfied (even if it will be
   * necessary to process the entire LargestPossibleRegion) and
   * returns false otherwise.  This method is used by
   * PropagateRequestedRegion().  PropagateRequestedRegion() throws a
   * InvalidRequestedRegionError exception if the requested region is
   * not within the LargestPossibleRegion. Default implementation
   * simply returns true in order to support DataObjects that do not
   * need regions (for instance itk::EquivalencyTable). */
  virtual bool VerifyRequestedRegion();

  /** Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * The default implementation of this method is empty. If a subclass
   * overrides this method, it should always call its superclass'
   * version. */
  virtual void CopyInformation(const DataObject *);

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  For
   * DataObject's that do not support Regions, this method does
   * nothing. Subclasses of DataObject that do support Regions,
   * provide an alternative implementation. */
  virtual void SetRequestedRegion(DataObject *);

  /** Method for grafting the content of one data object into another one.
   * This method is intended to be overloaded by derived classes. Each one of
   * them should use dynamic_casting in order to verify that the grafted
   * object is actually of the same type as the class on which the Graft()
   * method was invoked. */
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
