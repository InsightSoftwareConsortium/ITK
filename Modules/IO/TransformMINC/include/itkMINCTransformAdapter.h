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
#ifndef itkMINCTransformAdapter_h
#define itkMINCTransformAdapter_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_det.h"
#include "vnl/vnl_vector_fixed_ref.h"
#include "vnl/vnl_vector.h"
#include "itkTransform.h"
#include "itkObjectFactory.h"

// minc header
#include "itk_minc2.h"

namespace itk
{

/** \class MINCTransformAdapter
 * \ingroup  ITKIOTransformMINC
 * \brief ITK wrapper around MINC general transform functions, supports all the transformations that MINC XFM supports
 *
 * \author Vladimir S. FONOV
 *         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
 * \ingroup ITKIOTransformMINC
 */
template <typename TParametersValueType = double, unsigned int NInputDimensions = 3, unsigned int NOutputDimensions = 3>
class MINCTransformAdapter : public Transform<TParametersValueType, NInputDimensions, NOutputDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MINCTransformAdapter);

  /** Standard class type aliases. */
  using Self = MINCTransformAdapter;

  using Superclass = Transform<TParametersValueType, NInputDimensions, NOutputDimensions>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MINCTransformAdapter, Transform);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NInputDimensions;
  static constexpr unsigned int OutputSpaceDimension = NOutputDimensions;

  /** Type of the input parameters. */
  using ScalarType = double;

  /** Type of the input parameters. */
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;

  /** Type of the Jacobian matrix. */
  using JacobianType = typename Superclass::JacobianType;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, Self::InputSpaceDimension>;
  using OutputVectorType = Vector<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard variable length vector type for this class
   *  this provides an interface for the VectorImage class */
  using InputVectorPixelType = VariableLengthVector<TParametersValueType>;
  using OutputVectorPixelType = VariableLengthVector<TParametersValueType>;

  /** Standard covariant vector type for this class */
  using InputCovariantVectorType = CovariantVector<TParametersValueType, Self::InputSpaceDimension>;

  using OutputCovariantVectorType = CovariantVector<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard coordinate point type for this class */
  using InputPointType = Point<TParametersValueType, NInputDimensions>;
  using OutputPointType = Point<TParametersValueType, NInputDimensions>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = vnl_vector_fixed<TParametersValueType, NInputDimensions>;
  using OutputVnlVectorType = vnl_vector_fixed<TParametersValueType, NOutputDimensions>;

  /**  Method to transform a point. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override
  {
    if (!m_Initialized)
    {
      return point;
    }

    if (m_Invert && !m_Initialized_invert)
    {
      return point;
    }

    OutputPointType pnt;
    // works only for 3D->3D transforms
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), point[0], point[1], point[2], &pnt[0], &pnt[1], &pnt[2]);

    return pnt;
  }

  //! use finate element difference to estimate local jacobian
  void
  estimate_local_jacobian(const InputPointType & orig, vnl_matrix_fixed<double, 3, 3> & m)
  {
    double       u1, v1, w1;
    double       u2, v2, w2;
    const double delta = 1e-4;

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0] - delta, orig[1], orig[2], &u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0] + delta, orig[1], orig[2], &u2, &v2, &w2);
    m(0, 0) = (u2 - u1) / (2 * delta);
    m(0, 1) = (v2 - v1) / (2 * delta);
    m(0, 2) = (w2 - w1) / (2 * delta);

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1] - delta, orig[2], &u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1] + delta, orig[2], &u2, &v2, &w2);
    m(1, 0) = (u2 - u1) / (2 * delta);
    m(1, 1) = (v2 - v1) / (2 * delta);
    m(1, 2) = (w2 - w1) / (2 * delta);

    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1], orig[2] - delta, &u1, &v1, &w1);
    general_transform_point((m_Invert ? &m_Xfm_inv : &m_Xfm), orig[0], orig[1], orig[2] + delta, &u2, &v2, &w2);
    m(2, 0) = (u2 - u1) / (2 * delta);
    m(2, 1) = (v2 - v1) / (2 * delta);
    m(2, 2) = (w2 - w1) / (2 * delta);
  }

  /**  Method to transform a vector. */
  OutputVectorType
  TransformVector(const InputVectorType & vector, const InputPointType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return vector;
  }

  /**  Method to transform a vector. */
  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vector, const InputPointType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return vector;
  }

  /**  Method to transform a vector. */
  OutputVectorType
  TransformVector(const InputVectorType & vector) const override
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vector) const override
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVectorPixelType
  TransformVector(const InputVectorPixelType & vector) const override
  {
    return Superclass::TransformVector(vector);
  }

  /**  Method to transform a vector. */
  OutputVectorPixelType
  TransformVector(const InputVectorPixelType & vector, const InputPointType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return vector;
  }

  /**  Method to transform a CovariantVector. */
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vector, const InputPointType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return vector;
  }

  /**  Method to transform a CovariantVector. */
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vector) const override
  {
    return Superclass::TransformCovariantVector(vector);
  }

  /**  Method to transform a CovariantVector. */
  OutputVectorPixelType
  TransformCovariantVector(const InputVectorPixelType & vector) const override
  {
    return Superclass::TransformCovariantVector(vector);
  }

  /**  Method to transform a CovariantVector. */
  OutputVectorPixelType
  TransformCovariantVector(const InputVectorPixelType & vector, const InputPointType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return vector;
  }

  /** Set the transformation to an Identity
   */
  virtual void
  SetIdentity()
  {
    cleanup();
  }

  void
  SetFixedParameters(const FixedParametersType &) override
  {
    itkExceptionMacro(<< "Not Implemented");
  }

  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType &) const override
  {
    itkExceptionMacro(<< "Not Implemented");
  }

  NumberOfParametersType
  GetNumberOfParameters() const override
  {
    // this transform is defined by XFM file
    itkExceptionMacro(<< "Not Defined");
    return 0;
  }

  /** Set the Transformation Parameters
   * and update the internal transformation. */
  void
  SetParameters(const ParametersType &) override
  {
    itkExceptionMacro(<< "Not Implemented");
  }

  const ParametersType &
  GetParameters() const override
  {
    itkExceptionMacro(<< "Not Implemented");
    return m_Parameters;
  }

  void
  OpenXfm(const char * xfm)
  {
    cleanup();
    if (input_transform_file(xfm, &m_Xfm) != VIO_OK)
      itkExceptionMacro(<< "Error reading XFM:" << xfm);
    m_Initialized = true;
  }

  void
  Invert()
  {
    if (!m_Initialized)
      itkExceptionMacro(<< "XFM not initialized");
    if (!m_Initialized_invert)
    {
      create_inverse_general_transform(&m_Xfm, &m_Xfm_inv);
      m_Initialized_invert = true;
    }
    m_Invert = !m_Invert;
  }

protected:
  MINCTransformAdapter()
    : Transform<TParametersValueType, NInputDimensions, NOutputDimensions>(0)
  {
    if (NInputDimensions != 3 || NOutputDimensions != 3)
      itkExceptionMacro(<< "Sorry, only 3D to 3d minc xfm transform is currently implemented");
  }

  ~MINCTransformAdapter() override { cleanup(); }

  void
  cleanup()
  {
    if (m_Initialized)
    {
      delete_general_transform(&m_Xfm);
    }
    if (m_Initialized_invert)
    {
      delete_general_transform(&m_Xfm_inv);
    }
    m_Initialized = false;
    m_Initialized_invert = false;
  }

  ParametersType m_Parameters;

  mutable VIO_General_transform m_Xfm;
  mutable VIO_General_transform m_Xfm_inv;

  bool m_Invert{ false };
  bool m_Initialized{ false };
  bool m_Initialized_invert{ false };
};

} // namespace itk
#endif // itkMINCTransformAdapter_h
