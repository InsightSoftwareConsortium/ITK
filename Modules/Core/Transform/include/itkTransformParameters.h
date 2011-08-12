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
#ifndef __itkTransformParameters_h
#define __itkTransformParameters_h

#include "itkArray.h"
#include "itkTransformParametersHelper.h"

namespace itk
{
/** \class TransformParameters
 *  \brief Class to hold and manage different parameter types used by Transforms.
 *
 * \ingroup ITKTransform
 */

template< typename TValueType >
class TransformParameters : public Array< TValueType >
{
public:

  /** The element type stored at each location in the Array. */
  typedef TValueType                          ValueType;
  typedef TransformParameters                 Self;
  typedef Array< TValueType >                 Superclass;
  typedef Superclass                          ArrayType;
  typedef typename Superclass::VnlVectorType  VnlVectorType;

  /** Helper class for managing different types of parameter
   * data. */
  typedef TransformParametersHelper< TValueType > TransformParametersHelperType;

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  TransformParameters();

  /** Copy constructor.  Uses VNL copy construtor with correct
   *  setting for memory management.
   *  The vnl vector copy constructor creates new memory
   *  no matter the setting of let array manage memory of rhs.
   */
  TransformParameters(const TransformParameters& rhs);

  /** Constructor with size. Size can only be changed by assignment */
  explicit TransformParameters(unsigned int dimension);

  /** Constructor with Array assignment */
  TransformParameters( const ArrayType& array );

  /** Initialize. Initialization called by constructors. */
  void Initialize();

  /** Set a new data pointer for the parameter data, pointing it to a different
   * memory block. The size of the new memory block must equal the current
   * size, in elements of TValueType.
   * This call is passed to the assigned TransformParametersHelper.
   * \warning Memory must be managed by caller after this call. */
  virtual void MoveDataPointer( TValueType * pointer )
    {
    if( m_Helper == NULL )
      {
      itkGenericExceptionMacro("TransformParameters::MoveDataPointer: "
        "m_Helper must be set.");
      }
    this->m_Helper->MoveDataPointer( this, pointer );
    }

  /** Set an object that holds the parameters. Used by the helper of
   * derived classes that use an object other than itkArray to hold parameter
   * data. The helper class must check that the object is the correct type.
   * The call is passed to the assigned TransformParametersHelper. */
  virtual void SetParametersObject( LightObject * object )
    {
    if( m_Helper == NULL )
      {
      itkGenericExceptionMacro("TransformParameters::SetParameterObject: "
        "m_Helper must be set.");
      }
      this->m_Helper->SetParametersObject( this, object );
    }

  /** Assign a helper. TransformParameters manages the helper once
   *  its been assigned. The generic helper, TransformParametersHelper,
   *  is set in constructor.
   *  Transform classes that need a specialized helper should allocate
   *  one themselves and assign it with this method. */
  virtual void SetHelper( TransformParametersHelperType* helper );

  /** Get the helper in use. */
  TransformParametersHelperType* GetHelper()
    { return m_Helper; }

  /** Copy opertors
   *
   * TODO Determine behavior when copying from obj pointing to image parameters.
   *  By default should copy image param data into Array portion of new object,
   *  i.e. into data_block. Is that what we want? */
  const Self & operator=(const Self & rhs);

  const Self & operator=(const ArrayType & rhs);

  const Self & operator=(const VnlVectorType & rhs);

  ~TransformParameters();
private:
   TransformParametersHelperType*           m_Helper;
};

}//namespace itk

#if ITK_TEMPLATE_TXX
#include "itkTransformParameters.hxx"
#endif

#endif
