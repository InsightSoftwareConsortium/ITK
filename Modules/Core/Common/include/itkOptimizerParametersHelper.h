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
#ifndef itkOptimizerParametersHelper_h
#define itkOptimizerParametersHelper_h

#include "itkObject.h"
#include "itkArray.h"

namespace itk
{
/** \class OptimizerParametersHelper
 *  \brief Basic helper class to manage parameter data as an Array type,
 *  the default type.
 *
 * \ingroup ITKCommon
 */

template< typename TValue >
class ITK_TEMPLATE_EXPORT OptimizerParametersHelper
{
public:

  /** The element type stored at each location in the Array. */
  typedef TValue                          ValueType;
  typedef OptimizerParametersHelper       Self;

  /** Type of common data object used by OptimizerParameters. */
  typedef Array< TValue >                 CommonContainerType;

  /** Default constructor. Nothing to do. */
  OptimizerParametersHelper(){}

  /** Set a new data pointer for the parameter data, pointing it to a different
   * memory block. The size of the new memory block must equal the current
   * size, in elements of TValue.
   * This call is passed to the assigned OptimizerParametersHelper.
   * \warning Memory must be managed by caller after this call.
   * \c container is the OptimizerParameters object to which this helper
   * is assigned.
   * Generally this will be called from the OptimizerParameters object to
   * which this helper is assigned.
   */
  virtual void MoveDataPointer(CommonContainerType* container,
                               TValue * pointer )
    {
    container->SetData(
      pointer, container->GetSize(), false /*LetArrayManageMemory*/);
    }

  /** Set an object that holds the parameters. Used by
   * derived classes that use an object other than itkArray to hold parameter
   * data. The derived class must check that the object is the correct type.
   * Generally this will be called from the OptimizerParameters object to
   * which this helper is assigned.
   * \c container is the OptimizerParameters object to which this helper
   * is assigned.
   */
  virtual void SetParametersObject(CommonContainerType *,
                                   LightObject *)
    {
    itkGenericExceptionMacro("OptimizerParametersHelper::SetParametersObject: "
      "Not implemented for base class.");
    }

  virtual ~OptimizerParametersHelper(){}

};

}//namespace itk

#endif
