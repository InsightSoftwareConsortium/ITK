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
#ifndef __itkTransformBase_h
#define __itkTransformBase_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkArray2D.h"
#include "itkTransformParameters.h"

#include "itkObjectFactory.h"

namespace itk
{
/** \class TransformBase
 *
 * This class is an abstract class to represent the transform
 * \ingroup Transforms
 *
 * \ingroup ITKTransform
 */
class ITK_EXPORT TransformBase:public Object
{
public:
  /** Standard class typedefs. */
  typedef TransformBase              Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Type of the input parameters. */
  typedef  double                                     ParametersValueType;
  typedef  TransformParameters< ParametersValueType > ParametersType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformBase, Object);

  /** Return the number of parameters that completely define the Transfom  */
  virtual unsigned int GetNumberOfParameters(void) const = 0;

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
  virtual void SetFixedParameters(const ParametersType &) = 0;

  /** Get the fixed parameters. */
  virtual const ParametersType & GetFixedParameters() const = 0;

  /** Generate a platform independant name */
  virtual std::string GetTransformTypeAsString() const = 0;

protected:
  TransformBase() {}
  virtual ~TransformBase() {}
private:
  TransformBase(const Self &);  //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#endif
