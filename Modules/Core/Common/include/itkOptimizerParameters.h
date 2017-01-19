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
#ifndef itkOptimizerParameters_h
#define itkOptimizerParameters_h

#include "itkArray.h"
#include "itkOptimizerParametersHelper.h"

namespace itk
{
/** \class OptimizerParameters
 *  \brief Class to hold and manage different parameter types used during
 *  optimization.
 *
 * \ingroup ITKCommon
 */

template<typename TParametersValueType>
class ITK_TEMPLATE_EXPORT OptimizerParameters : public Array<TParametersValueType>
{
public:

  /** The element type stored at each location in the Array. */
  typedef TParametersValueType                 ValueType;
  typedef OptimizerParameters                  Self;
  typedef Array<TParametersValueType>          Superclass;
  typedef Superclass                           ArrayType;
  typedef typename Superclass::VnlVectorType   VnlVectorType;
  typedef typename Superclass::SizeValueType   SizeValueType;

  /** Helper class for managing different types of parameter
   * data. */
  typedef OptimizerParametersHelper<TParametersValueType> OptimizerParametersHelperType;

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  OptimizerParameters();

  /** Copy constructor.  Uses VNL copy construtor with correct
   *  setting for memory management.
   *  The vnl vector copy constructor creates new memory
   *  no matter the setting of let array manage memory of rhs.
   */
  OptimizerParameters(const OptimizerParameters& rhs);

  /** Constructor with size. Size can only be changed by assignment */
  explicit OptimizerParameters(SizeValueType  dimension);

  /** Constructor with Array assignment */
  OptimizerParameters( const ArrayType& array );

  /** Initialize. Initialization called by constructors. */
  void Initialize();

  /** Set a new data pointer for the parameter data, pointing it to a different
   * memory block. The size of the new memory block must equal the current
   * size, in elements of TParametersValueType.
   * This call is passed to the assigned OptimizerParametersHelper.
   * \warning Memory must be managed by caller after this call. */
  virtual void MoveDataPointer( TParametersValueType * pointer );

  /** Set an object that holds the parameters. Used by the helper of
   * derived classes that use an object other than itkArray to hold parameter
   * data. The helper class must check that the object is the correct type.
   * The call is passed to the assigned OptimizerParametersHelper. */
  virtual void SetParametersObject( LightObject * object );

  /** Assign a helper. OptimizerParameters manages the helper once
   *  its been assigned. The generic helper, OptimizerParametersHelper,
   *  is set in constructor.
   *  Classes that need a specialized helper should allocate
   *  one themselves and assign it with this method. */
  virtual void SetHelper( OptimizerParametersHelperType* helper );

  /** Get the helper in use. */
  OptimizerParametersHelperType* GetHelper()
    { return m_Helper; }

  /** Copy opertors
   *
   * TODO Determine behavior when copying from obj pointing to image parameters.
   *  By default should copy image param data into Array portion of new object,
   *  i.e. into data_block. Is that what we want? */
  const Self & operator=(const Self & rhs);

  const Self & operator=(const ArrayType & rhs);

  const Self & operator=(const VnlVectorType & rhs);

  virtual ~OptimizerParameters();

private:
   OptimizerParametersHelperType*           m_Helper;
};

}//namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOptimizerParameters.hxx"
#endif

#endif
