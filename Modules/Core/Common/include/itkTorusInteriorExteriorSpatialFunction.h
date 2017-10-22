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
#ifndef itkTorusInteriorExteriorSpatialFunction_h
#define itkTorusInteriorExteriorSpatialFunction_h

#include "vnl/vnl_vector.h"
#include "itkInteriorExteriorSpatialFunction.h"
#include "itkCovariantVector.h"

namespace itk
{
/**
 * \class TorusInteriorExteriorSpatialFunction
 * \brief Spatial function implementation of torus symmetric about the z-axis in 3D
 *
 * Handle with care! May behave in strange ways when used with dimensions other than 3
 *
 * \ingroup SpatialFunctions
 *
 *
 * \ingroup ITKCommon
 */

template< unsigned int VDimension = 3,
          typename TInput = Point< double, VDimension > >
class ITK_TEMPLATE_EXPORT TorusInteriorExteriorSpatialFunction:
  public InteriorExteriorSpatialFunction< VDimension, TInput >
{
public:

  /** Standard class typedefs. */
  typedef TorusInteriorExteriorSpatialFunction                  Self;
  typedef InteriorExteriorSpatialFunction< VDimension, TInput > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Run time information. */
  itkTypeMacro(TorusInteriorExteriorSpatialFunction, InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType & position) const ITK_OVERRIDE;

  /** Set/Get the origin of the torus (the point from which the major
   * radius is measured). */
  itkGetConstMacro(Origin, InputType);
  itkSetMacro(Origin, InputType);

  /** Set/Get the major radius of the torus. */
  itkGetConstMacro(MajorRadius, double);
  itkSetMacro(MajorRadius, double);

  /** Set/Get the minor radius of the torus (radius of tube). */
  itkGetConstMacro(MinorRadius, double);
  itkSetMacro(MinorRadius, double);

protected:
  TorusInteriorExteriorSpatialFunction();
  virtual ~TorusInteriorExteriorSpatialFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TorusInteriorExteriorSpatialFunction);

  InputType m_Origin;
  double    m_MajorRadius;
  double    m_MinorRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTorusInteriorExteriorSpatialFunction.hxx"
#endif

#endif
