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
#ifndef itkSpatialFunction_h
#define itkSpatialFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"

namespace itk
{
/** \class SpatialFunction
 * \brief N-dimensional spatial function class
 *
 * itk::SpatialFunction provides the ability to define functions that can
 * be evaluated at an arbitrary point in space (physical or otherwise). The return
 * type is specified by the derived class, and the input to the function
 * is an n-dimensional itk::Point.
 *
 * Although itk::ImageFunction and itk::SpatialFunction are quite similar,
 * itk::SpatialFunction derived classes exist without reference to an Image
 * type.
 *
 * SpatialFunction is templated over output type (the data type
 * returned by an evaluate() call) and dimensionality.
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */
template< typename TOutput,
          unsigned int VImageDimension = 3,
          typename TInput = Point< double, VImageDimension > >
class ITK_TEMPLATE_EXPORT SpatialFunction:public FunctionBase< TInput, TOutput >
{
public:
  /** Standard class typedefs. */
  typedef SpatialFunction                 Self;
  typedef FunctionBase< TInput, TOutput > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialFunction, FunctionBase);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Spatial dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Evaluate the function at a given position. Remember, position is
  * represented by an n-d itk::Point object with data type double. */
  virtual OutputType Evaluate(const InputType & input) const ITK_OVERRIDE = 0;

protected:
  SpatialFunction();
  virtual ~SpatialFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunction.hxx"
#endif

#endif
