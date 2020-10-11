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
#ifndef itkTransformParametersAdaptor_h
#define itkTransformParametersAdaptor_h

#include "itkTransformParametersAdaptorBase.h"
#include "itkTransform.h"

namespace itk
{
/** \class TransformParametersAdaptor
 * \brief Base helper class intended for multi-resolution image registration.
 *
 * During multi-resolution image registration, it is often useful to expand
 * the number of parameters describing the transform when going from one level
 * to the next.  For example, in B-spline registration, one often wants to
 * increase the mesh size (or, equivalently, the control point grid size) for
 * increased flexibility in optimizing the transform.  This requires the
 * propagation of the current transform solution to the next level where the
 * solution is identical but with an increase in the number of parameters.
 * This base class and those derived classes are meant to handle these types of
 * situations.
 *
 * Basic usage will involve the user specifying the required fixed parameters, i.e.
 *
     \code
     transformAdaptor->SetTransform( transform );
     transformAdaptor->SetRequiredFixedParameters( fixedParameters );
     transformAdaptor->AdaptTransformParameters();
     \endcode
 *
 * which will adjust the transform based on the new fixed parameters.
 *
 * \author Nick Tustison
 * \author Marius Staring
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class TransformParametersAdaptor
  : public TransformParametersAdaptorBase<
      Transform<typename TTransform::ScalarType, TTransform::InputSpaceDimension, TTransform::OutputSpaceDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = TransformParametersAdaptor;
  using Superclass = TransformParametersAdaptorBase<
    Transform<typename TTransform::ScalarType, TTransform::InputSpaceDimension, TTransform::OutputSpaceDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformParametersAdaptor, TransformParametersAdaptorBase);

  /** Typedefs associated with the transform */

  using TransformBaseType = typename Superclass::TransformBaseType;
  using TransformType = TTransform;
  using TransformPointer = typename TransformType::Pointer;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;

  /** Set the transform to be adapted */
  itkSetObjectMacro(Transform, TransformType);

  void
  SetTransform(TransformBaseType * _arg, void *) override
  {
    auto * tx = dynamic_cast<TransformType *>(_arg);
    itkAssertOrThrowMacro(tx != nullptr, "Unable to convert Transform to require concrete transform!");
    this->SetTransform(tx);
  }

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Set the fixed parameters */
  void
  SetRequiredFixedParameters(const FixedParametersType fixedParameters) override
  {
    itkDebugMacro("setting RequiredFixedParameters to " << fixedParameters);
    if (this->m_RequiredFixedParameters != fixedParameters)
    {
      this->m_RequiredFixedParameters = fixedParameters;
      this->Modified();
    }
  }

  /** Get the fixed parameters */
  const FixedParametersType &
  GetRequiredFixedParameters() const override
  {
    return this->m_RequiredFixedParameters;
  }

  /** Initialize the transform using the specified fixed parameters */
  void
  AdaptTransformParameters() override{};

protected:
  TransformParametersAdaptor() = default;
  ~TransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    itkPrintSelfObjectMacro(Transform);
  }

  TransformPointer m_Transform;
}; // class TransformParametersAdaptor
} // namespace itk

#endif /* itkTransformParametersAdaptor_h */
