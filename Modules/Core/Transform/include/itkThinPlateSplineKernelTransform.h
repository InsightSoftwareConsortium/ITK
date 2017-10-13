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
#ifndef itkThinPlateSplineKernelTransform_h
#define itkThinPlateSplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{
/** \class ThinPlateSplineKernelTransform
 * This class defines the thin plate spline (TPS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType,
          unsigned int NDimensions = 3>
// Number of dimensions
class ITK_TEMPLATE_EXPORT ThinPlateSplineKernelTransform:
  public KernelTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef ThinPlateSplineKernelTransform                     Self;
  typedef KernelTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThinPlateSplineKernelTransform, KernelTransform);

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType      ParametersType;
  typedef typename Superclass::FixedParametersType FixedParametersType;

  /** Jacobian Type */
  typedef typename Superclass::JacobianType JacobianType;

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, Superclass::SpaceDimension);

  /** These (rather redundant) typedefs are needed because typedefs are not inherited */
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::PointsIterator            PointsIterator;

protected:
  ThinPlateSplineKernelTransform() {}
  virtual ~ThinPlateSplineKernelTransform() ITK_OVERRIDE {}

  /** These (rather redundant) typedefs are needed because typedefs are not inherited. */
  typedef typename Superclass::GMatrixType GMatrixType;

  /** Compute G(x)
   * For the thin plate spline, this is:
   * G(x) = r(x)*I
   * \f$ G(x) = r(x)*I \f$
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * \f[ r(x) = \sqrt{ x_1^2 + x_2^2 + x_3^2 }  \f]
   * I = identity matrix. */
  virtual void ComputeG(const InputVectorType & landmarkVector, GMatrixType & gmatrix) const ITK_OVERRIDE;

  /** Compute the contribution of the landmarks weighted by the kernel funcion
      to the global deformation of the space  */
  virtual void ComputeDeformationContribution(const InputPointType & inputPoint,
                                              OutputPointType & result) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThinPlateSplineKernelTransform);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThinPlateSplineKernelTransform.hxx"
#endif

#endif // itkThinPlateSplineKernelTransform_h
