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
#ifndef itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h
#define itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{
/** \class SymmetricEllipsoidInteriorExteriorSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Similar to EllipsoidInteriorExteriorSpatialFunction in that it
 * implements a function that returns 1 for points inside or on the surface
 * of a ellipsoid and 0 for points outside the ellipsoid. However, this
 * ellipsoid is defined by a single orientation vector and deals
 * only with symmetric ellipsoids. An n-dimensional symmetric ellipsoid
 * is one which has m axes of equal length and (n - m) unique axes lengths.
 * Specifically, this class deals with the case where (n - m) = 1 and
 * the ellipsoid's major axis is oriented along a singles orientation vector.
 * \ingroup ITKCommon
 */
template< unsigned int VDimension = 3,
          typename TInput = Point< double, VDimension > >
class ITK_TEMPLATE_EXPORT SymmetricEllipsoidInteriorExteriorSpatialFunction:
  public InteriorExteriorSpatialFunction< VDimension, TInput >
{
public:
  /** Standard class typedefs. */
  typedef SymmetricEllipsoidInteriorExteriorSpatialFunction Self;
  typedef InteriorExteriorSpatialFunction< VDimension >     Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;
  typedef Vector< double, VDimension >                      VectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time information. */
  itkTypeMacro(SymmetricEllipsoidInteriorExteriorSpatialFunction, InteriorExteriorSpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType & position) const ITK_OVERRIDE;

  /** Get and set the center of the ellipsoid. */
  itkGetConstMacro(Center, InputType);
  itkSetMacro(Center, InputType);

  /** Set the orientation vector of the ellipsoid's unique axis and axes lengths.
   * Must be normalized!!!!! */
  void SetOrientation(VectorType orientation, double uniqueAxis, double symmetricAxes);

protected:
  SymmetricEllipsoidInteriorExteriorSpatialFunction();
  virtual ~SymmetricEllipsoidInteriorExteriorSpatialFunction() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  SymmetricEllipsoidInteriorExteriorSpatialFunction(const Self &); //purposely
                                                                   // not
                                                                   // implemented
  void operator=(const Self &);                                    //purposely
                                                                   // not
                                                                   // implemented

  /** The center of the ellipsoid. */
  InputType m_Center;

  /** The unique axis length of the ellipsoid. */
  double m_UniqueAxis;

  /** The symmetric axes lengths of the ellipsoid. */
  double m_SymmetricAxes;

  /** The orientation vector of the ellipsoid's unique axis. */
  Vector< double, VDimension > m_Orientation;

  /** The vector ratio. */
  double m_VectorRatio;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.hxx"
#endif

#endif
