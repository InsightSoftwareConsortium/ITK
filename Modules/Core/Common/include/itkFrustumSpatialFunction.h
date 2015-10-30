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
#ifndef itkFrustumSpatialFunction_h
#define itkFrustumSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{
/**
 * \class FrustumSpatialFunction
 * \brief Spatial function implementation of a truncated pyramid.
 *
 * Implements a function that returns 0 for points inside or on the surface
 * of a truncated pyrami, 1 for points outside the truncated pyramid
 *
 * \ingroup SpatialFunctions
 *
 *
 * \ingroup ITKCommon
 */

template< unsigned int VImageDimension = 3, typename TInput = Point< double, 3 > >
class FrustumSpatialFunction:
  public InteriorExteriorSpatialFunction< VImageDimension, TInput >
{
public:

  /** Standard class typedefs. */
  typedef FrustumSpatialFunction< VImageDimension, TInput >          Self;
  typedef InteriorExteriorSpatialFunction< VImageDimension, TInput > Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrustumSpatialFunction, InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;

  /** Rotate the frustum in the XZ or the YZ plane */
  typedef enum {
    RotateInXZPlane = 1,
    RotateInYZPlane
    } FrustumRotationPlaneType;

  /** Evaluates the function at a given position */
  OutputType Evaluate(const InputType & position) const ITK_OVERRIDE;

  /** Get and set the center of the sphere */
  itkGetConstMacro(Apex, InputType);
  itkSetMacro(Apex, InputType);

  /** Get and set the angle of the pyramid axis
   * with respect to the Z axis */
  itkGetConstMacro(AngleZ, double);
  itkSetMacro(AngleZ, double);

  /** Get and set the aperture angle in X */
  itkGetConstMacro(ApertureAngleX, double);
  itkSetMacro(ApertureAngleX, double);

  /** Get and set the aperture angle in Y */
  itkGetConstMacro(ApertureAngleY, double);
  itkSetMacro(ApertureAngleY, double);

  /** Get and set the top plane distance to the Apex */
  itkGetConstMacro(TopPlane, double);
  itkSetMacro(TopPlane, double);

  /** Get and set the bottom plane distance to the Apex */
  itkGetConstMacro(BottomPlane, double);
  itkSetMacro(BottomPlane, double);

  /** Set macro to set the plane in which the frustum should rotate */
  itkSetMacro(RotationPlane, FrustumRotationPlaneType);

protected:
  FrustumSpatialFunction();
  virtual ~FrustumSpatialFunction();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  FrustumSpatialFunction(const Self &) ITK_DELETE_FUNCTION;
  void operator=(const Self &) ITK_DELETE_FUNCTION;

  /** The apex of the pyramid (of the same type as Input) */
  InputType m_Apex;

  /** Angle between the pyramid axis and the Z axis */
  double m_AngleZ;

  /** Aperture Angle in X direction */
  double m_ApertureAngleX;

  /** Aperture Angle in Y direction */
  double m_ApertureAngleY;

  /** Distance from Apex to top plane */
  double m_TopPlane;

  /** Distance from Apex to bottom plane */
  double m_BottomPlane;

  /** Plane in which to the frustum is being rotated */
  FrustumRotationPlaneType m_RotationPlane;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFrustumSpatialFunction.hxx"
#endif

#endif
