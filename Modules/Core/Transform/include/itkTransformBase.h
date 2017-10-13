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
#ifndef itkTransformBase_h
#define itkTransformBase_h

#include "ITKTransformExport.h"

#include "itkObject.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkArray2D.h"
#include "itkOptimizerParameters.h"

#include "itkObjectFactory.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class itkTransformBaseTemplate
 *
 * This class is an abstract class to represent a spatial transform.
 *
 * This class is templated over the scalar type used to store the transform's
 * parameters.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType>
class TransformBaseTemplate:public Object
{
public:
  /** Standard class typedefs. */
  typedef TransformBaseTemplate    Self;
  typedef Object                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Type of the input parameters. */

  typedef  TParametersValueType                          ParametersValueType;
  typedef  OptimizerParameters<ParametersValueType>      ParametersType;
  typedef  double                                        FixedParametersValueType;
  typedef  OptimizerParameters<FixedParametersValueType> FixedParametersType;

// The ITK_FIXED_PARAMETERS_ARE_DOUBLE is intended
// to facilitate transition of ITK for those very
// rare cases where multiple versions of ITK
// may need to be supported simultaneously.
#if defined( ITK_LEGACY_REMOVE )
  #undef  ITK_FIXED_PARAMETERS_ARE_DOUBLE
#else
  #define ITK_FIXED_PARAMETERS_ARE_DOUBLE
  // #if !defined(ITK_FIXED_PARAMETERS_ARE_DOUBLE)
  // typedef ParametersValueType FixedParametersValueType;
  // #endif
#endif

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformBaseTemplate, Object);

  /** The number of parameters can potentially be very large,
   *  therefore we use here a large capacity integer. */
  typedef IdentifierType      NumberOfParametersType;

  /** Return the number of parameters that completely define the Transfom  */
  virtual NumberOfParametersType GetNumberOfParameters(void) const = 0;

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters(void) const = 0;

  /** Get the size of the input space */
  virtual unsigned int GetInputSpaceDimension(void) const = 0;

  /** Get the size of the output space */
  virtual unsigned int GetOutputSpaceDimension(void) const = 0;

  /** Set the transformation parameters and update internal transformation. */
  virtual void SetParameters(const ParametersType &) = 0;

  /** Set the transformation by copying parameters and update internal transformation.
   * This method forces the transform to copy the parameters.  The
   * default implementation is to call SetParameters.  This call must
   * be overridden if the transform normally implements SetParameters
   * by keeping a reference to the parameters.
   * \sa SetParameters
   */
  virtual void SetParametersByValue(const ParametersType & p) = 0;

  /** Set the fixed parameters. */
  virtual void SetFixedParameters(const FixedParametersType &) = 0;

  /** This function allow copying a range of values into the Parameters
    * The range of values must conform to std::copy(begin, end, m_Parameters)
    * requirements.
    */
  virtual void CopyInParameters(const ParametersValueType * const begin,
                                const ParametersValueType * const end) = 0;

  /** This function allow copying a range of values into the FixedParameters
    * The range of values must conform to std::copy(begin, end, m_FixedParameters)
    * requirements.
    */
  virtual void CopyInFixedParameters(const FixedParametersValueType * const begin,
                                     const FixedParametersValueType * const end) = 0;

  /** Get the fixed parameters. */
  virtual const FixedParametersType & GetFixedParameters() const = 0;

  /** Generate a platform independent name */
  virtual std::string GetTransformTypeAsString() const = 0;

  typedef enum {
    UnknownTransformCategory=0,
    Linear=1,
    BSpline=2,
    Spline=3,
    DisplacementField=4,
    VelocityField=5
  } TransformCategoryType;

  /** Get transform category */
  virtual TransformCategoryType GetTransformCategory() const = 0;

protected:
  TransformBaseTemplate(){}
  virtual ~TransformBaseTemplate() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TransformBaseTemplate);
};

/** This helps to meet backward compatibility */
typedef TransformBaseTemplate< double > TransformBase;

} // end namespace itk

#endif

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformBase
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#  if defined( ITKTransform_EXPORTS )
//   We are building this library
#    define ITKTransform_EXPORT_EXPLICIT ITK_TEMPLATE_EXPORT
#  else
//   We are using this library
#    define ITKTransform_EXPORT_EXPLICIT ITKTransform_EXPORT
#  endif
namespace itk
{
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")
extern template class ITKTransform_EXPORT_EXPLICIT TransformBaseTemplate< double >;
extern template class ITKTransform_EXPORT_EXPLICIT TransformBaseTemplate< float >;
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif
} // end namespace itk
#  undef ITKTransform_EXPORT_EXPLICIT
#endif
