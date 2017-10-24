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
#ifndef itkEllipsoidInteriorExteriorSpatialFunction_h
#define itkEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/**
 * \class EllipsoidInteriorExteriorSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Implements a function that returns 1 for points inside or on the
 * surface of a ellipsoid and 0 for points outside the ellipsoid. The
 * orientation of the  n-dimensional ellipsoid axes are defined by n
 * orthogonal vectors. See
 * Examples/EllipsoidInteriorExteriorSpatialFunction/README for an
 * example of creating an Ellipsoid in an image.
 * \ingroup ITKCommon
 */
template< unsigned int VDimension = 3,
          typename TInput = Point< double, VDimension > >
class ITK_TEMPLATE_EXPORT EllipsoidInteriorExteriorSpatialFunction:
  public InteriorExteriorSpatialFunction< VDimension, TInput >
{
public:
  /** Standard class typedefs. */
  typedef EllipsoidInteriorExteriorSpatialFunction              Self;
  typedef InteriorExteriorSpatialFunction< VDimension, TInput > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EllipsoidInteriorExteriorSpatialFunction, InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;

  /** Typedef for the orientation matrix */
  typedef vnl_matrix_fixed< double, VDimension, VDimension > OrientationType;

  /** Set/Get and set the center of the ellipsoid. */
  itkGetConstMacro(Center, InputType);
  itkSetMacro(Center, InputType);

  /** Get and set the axes lengths of the ellipsoid. */
  itkGetConstMacro(Axes, InputType);
  itkSetMacro(Axes, InputType);

  /** Set the orientation vectors (must be orthogonal) of the ellipsoid axes.
   * Must be normalized!!!!! */
  void SetOrientations(const OrientationType &);

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType & position) const ITK_OVERRIDE;

protected:
  EllipsoidInteriorExteriorSpatialFunction();
  virtual ~EllipsoidInteriorExteriorSpatialFunction() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EllipsoidInteriorExteriorSpatialFunction);

  /** The center of the ellipsoid. */
  InputType m_Center;

  /** The axes lenths of the ellipsoid. */
  InputType m_Axes;

  /** The orientation vectors (must be orthogonal) of the ellipsoid axes. */
  double **m_Orientations;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEllipsoidInteriorExteriorSpatialFunction.hxx"
#endif

#endif
