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
 * of a truncated pyramid, 1 for points outside the truncated pyramid.
 *
 * \ingroup SpatialFunctions
 *
 *
 * \ingroup ITKCommon
 */

template< unsigned int VDimension = 3,
          typename TInput = Point< double, VDimension > >
class ITK_TEMPLATE_EXPORT FrustumSpatialFunction:
  public InteriorExteriorSpatialFunction< VDimension, TInput >
{
public:

  /** Standard class typedefs. */
  typedef FrustumSpatialFunction< VDimension, TInput >          Self;
  typedef InteriorExteriorSpatialFunction< VDimension, TInput > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrustumSpatialFunction, InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Rotate the frustum in the XZ or the YZ plane. */
  typedef enum {
    RotateInXZPlane = 1,
    RotateInYZPlane
    } FrustumRotationPlaneType;

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType & position) const ITK_OVERRIDE;

  /** Set/Get the apex of the pyramid. */
  itkGetConstMacro(Apex, InputType);
  itkSetMacro(Apex, InputType);

  /** Set/Get the angle of the pyramid axis with respect to the Z axis. */
  itkGetConstMacro(AngleZ, double);
  itkSetMacro(AngleZ, double);

  /** Set/Get the aperture angle in the X axis. */
  itkGetConstMacro(ApertureAngleX, double);
  itkSetMacro(ApertureAngleX, double);

  /** Set/Get the aperture angle in the Y axis. */
  itkGetConstMacro(ApertureAngleY, double);
  itkSetMacro(ApertureAngleY, double);

  /** Set/Get the top plane distance to the apex. */
  itkGetConstMacro(TopPlane, double);
  itkSetMacro(TopPlane, double);

  /** Set/Get the bottom plane distance to the apex. */
  itkGetConstMacro(BottomPlane, double);
  itkSetMacro(BottomPlane, double);

  /** Set/Get the plane in which the frustum should rotate. */
  itkGetConstMacro(RotationPlane, FrustumRotationPlaneType);
  itkSetMacro(RotationPlane, FrustumRotationPlaneType);

protected:
  FrustumSpatialFunction();
  virtual ~FrustumSpatialFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FrustumSpatialFunction);

  InputType                 m_Apex;
  double                    m_AngleZ;
  double                    m_ApertureAngleX;
  double                    m_ApertureAngleY;
  double                    m_TopPlane;
  double                    m_BottomPlane;
  FrustumRotationPlaneType  m_RotationPlane;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFrustumSpatialFunction.hxx"
#endif

#endif
