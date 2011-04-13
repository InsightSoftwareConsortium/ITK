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

/** \class TemporalDataObject
 * \brief DataObject subclass with knowledge of temporal region
 *
 *
 */
class ITK_EXPORT TemporalDataObject:public DataObject
{

  /**-TYPEDEFS---------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef TemporalDataObject                  Self;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef WeakPointer< const Self >           ConstWeakPointer;


  /**-PUBLIC METHODS---------------------------------------------------------*/



protected:

  /**-PROTECTED METHODS------------------------------------------------------*/
  TemporalDataObject();
  ~TemporalDataObject();
  void PrintSelf(std::ostream & os, Indent indent) const;


  /**-PROTECTED MEMBERS------------------------------------------------------*/

private:

  /**-PRIVATE METHODS--------------------------------------------------------*/
  TemporalDataObject(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented



  /**-PRIVATE MEMBERS--------------------------------------------------------*/





};  // end class TemporalDataObject

} // end namespace itk

#endif
