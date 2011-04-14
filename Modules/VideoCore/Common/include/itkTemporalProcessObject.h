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
#ifndef __itkTemporalProcessObject_h
#define __itkTemporalProcessObject_h

#include "itkProcessObject.h"

namespace itk
{

/** Forward declarations */
class TemporalRegion;
class TemporalDataObject;


/** \class TemporalProcessObject
 * \brief TemporalProcessObject implements a ProcessObject for the itk pipeline
 *        with the notion of a temporal region
 *
 * TemporalProcessObject acts as a pass-through in the inheritance tree in
 * order to require that subclasses properly implement handeling of temporal
 * regions. The three methods that a subclass must implement are:
 *
 * EnlargeOutputRequestedTemporalRegion(TemporalDataObject*)
 * GenerateOutputRequestedTemporalRegion(TemporalDataObject*)
 * GenerateInputRequestedTemporalRegion(TemporalDataObject*)
 *
 * These three methods mirror the similarly named methods in ProcessObject but
 * require that the inputs be TemporalDataObjects. These methods are called by
 * the corresponding RequestedRegion methods after ensuring that the DataObject
 * outputs can be cast to TemporalDataObject.
 */
class TemporalProcessObject:public ProcessObject
{
public:

  /** Standard class typedefs */
  typedef TemporalProcessObject       Self;
  typedef ProcessObject               Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TemporalProcessObject, ProcessObject);

  /** Override EnlargeOutputRequestedRegion, GenerateOutputRequestedRegion, and
   * GenerateInputRequestedRegion to handle temporal regions */
  virtual void EnlargeOutputRequestedRegion(DataObject* output);
  virtual void GenerateOutputRequestedRegion(DataObject* output);
  virtual void GenerateInputRequestedRegion(DataObject* output);

protected:

  /** Provide explicit protected methods for handling temporal region in
   * EnlargeOutputRequestedRegion, GenerateOutputRequestedRegion, and
   * GenerateInputRequestedRegion. These methods are pure virtual here so that
   * subclasses MUST handle temporal regions but they can still do so however
   * they want */
  virtual void EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output) = 0;
  virtual void GenerateOutputRequestedTemporalRegion(TemporalDataObject* output) = 0;
  virtual void GenerateInputRequestedTemporalRegion(TemporalDataObject* output) = 0;

  TemporalProcessObject(){};
  virtual ~TemporalProcessObject(){};
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  TemporalProcessObject(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented


};  // end class TemporalProcessObject

} // end namespace itk

#endif
