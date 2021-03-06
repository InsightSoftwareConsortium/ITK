/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOptimizerParameters_h
#define itkOptimizerParameters_h

#include "itkArray.h"
#include "itkOptimizerParametersHelper.h"

#include <memory> // For unique_ptr.

namespace itk
{
/** \class OptimizerParameters
 *  \brief Class to hold and manage different parameter types used during
 *  optimization.
 *
 * \ingroup ITKCommon
 */

template <typename TParametersValueType>
class ITK_TEMPLATE_EXPORT OptimizerParameters : public Array<TParametersValueType>
{
public:
  /** The element type stored at each location in the Array. */
  using ValueType = TParametersValueType;
  using Self = OptimizerParameters;
  using Superclass = Array<TParametersValueType>;
  using ArrayType = Superclass;
  using VnlVectorType = typename Superclass::VnlVectorType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Helper class for managing different types of parameter
   * data. */
  using OptimizerParametersHelperType = OptimizerParametersHelper<TParametersValueType>;

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  OptimizerParameters() = default;

  /** Copy constructor.  Uses VNL copy constructor with correct
   *  setting for memory management.
   *  The vnl vector copy constructor creates new memory
   *  no matter the setting of let array manage memory of rhs.
   */
  OptimizerParameters(const OptimizerParameters & rhs)
    : Array<TParametersValueType>(rhs)
  {
    // Note: don't copy the OptimizerParametersHelper.
    // The Array copy constructor will allocate new memory
    // and copy the data to it. So we end up here with a generic
    // OptimizerParameters data object even if 'rhs' points to
    // something different.
  }

  /** Constructor with size. Size can only be changed by assignment.
   * \note This constructor may not initialize its elements.
   */
  explicit OptimizerParameters(SizeValueType dimension)
    : Array<TParametersValueType>(dimension)
  {}

  /** Constructor with Array assignment */
  OptimizerParameters(const ArrayType & array)
    : Array<TParametersValueType>(array)
  {}

  /** Constructor with size and initial value for each element. */
  explicit OptimizerParameters(const SizeValueType dimension, const ValueType & value)
    : Array<TParametersValueType>(dimension, value)
  {}


  /** Constructor with input data and size (number of elements). */
  explicit OptimizerParameters(const ValueType * const inputData, const SizeValueType dimension)
    : Array<TParametersValueType>(inputData, dimension)
  {}


  /** Initialize. Initialization called by constructors. */
  void
  Initialize()
  {
    // Set the default OptimizerParametersHelper
    this->m_Helper.reset(new OptimizerParametersHelperType);
  }


  /** Set a new data pointer for the parameter data, pointing it to a different
   * memory block. The size of the new memory block must equal the current
   * size, in elements of TParametersValueType.
   * This call is passed to the assigned OptimizerParametersHelper.
   * \warning Memory must be managed by caller after this call. */
  virtual void
  MoveDataPointer(TParametersValueType * pointer)
  {
    if (m_Helper == nullptr)
    {
      itkGenericExceptionMacro("OptimizerParameters::MoveDataPointer: "
                               "m_Helper must be set.");
    }
    this->m_Helper->MoveDataPointer(this, pointer);
  }

  /** Set an object that holds the parameters. Used by the helper of
   * derived classes that use an object other than itkArray to hold parameter
   * data. The helper class must check that the object is the correct type.
   * The call is passed to the assigned OptimizerParametersHelper. */
  virtual void
  SetParametersObject(LightObject * object)
  {
    if (m_Helper == nullptr)
    {
      itkGenericExceptionMacro("OptimizerParameters::SetParameterObject: "
                               "m_Helper must be set.");
    }
    this->m_Helper->SetParametersObject(this, object);
  }

  /** Assign a helper. OptimizerParameters manages the helper once
   *  its been assigned. The generic helper, OptimizerParametersHelper,
   *  is set in constructor.
   *  Classes that need a specialized helper should allocate
   *  one themselves and assign it with this method. */
  virtual void
  SetHelper(OptimizerParametersHelperType * helper)
  {
    this->m_Helper.reset(helper);
  }

  /** Get the helper in use. */
  OptimizerParametersHelperType *
  GetHelper()
  {
    return m_Helper.get();
  }

  /** Copy operators
   *
   * TODO Determine behavior when copying from obj pointing to image parameters.
   *  By default should copy image param data into Array portion of new object,
   *  i.e. into data_block. Is that what we want? */
  const Self &
  operator=(const Self & rhs)
  {
    // Note: there's no need to copy the OptimizerParametersHelper.
    // Call the superclass implementation.
    this->ArrayType::operator=(rhs);
    return *this;
  }

  const Self &
  operator=(const ArrayType & rhs)
  {
    // Call the superclass implementation
    this->ArrayType::operator=(rhs);
    return *this;
  }

  const Self &
  operator=(const VnlVectorType & rhs)
  {
    // Call the superclass implementation
    this->ArrayType::operator=(rhs);
    return *this;
  }

  ~OptimizerParameters() override = default;

private:
  std::unique_ptr<OptimizerParametersHelperType> m_Helper =
    std::unique_ptr<OptimizerParametersHelperType>{ new OptimizerParametersHelperType };
};

} // namespace itk

#endif
