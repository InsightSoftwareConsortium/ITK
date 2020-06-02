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
#ifndef itkDTITubeSpatialObjectPoint_h
#define itkDTITubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "itkDiffusionTensor3D.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/**\class DTITubeSpatialObjectPointEnums
 * \brief Contains all enum classes used by DTITubeSpatialObjectPoint class.
 * \ingroup ITKSpatialObjects
 */
class DTITubeSpatialObjectPointEnums
{
public:
  /**
*\class DTITubeSpatialObjectPointField
* \ingroup ITKSpatialObjects
* If you add a type here you need to modify the TranslateEnumToChar
to translate the enum to a string */
  enum class DTITubeSpatialObjectPointField : uint8_t
  {
    FA = 0,
    ADC = 1,
    GA = 2
  };
};
// Define how to print enumeration
extern ITKSpatialObjects_EXPORT std::ostream &
                                operator<<(std::ostream & out, const DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField value);
/**
 *\class DTITubeSpatialObjectPoint
 * \brief Point used for a tube definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build tubes.
 *
 * \sa DTITubeSpatialObject
 * \ingroup ITKSpatialObjects
 */
template <unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT DTITubeSpatialObjectPoint : public TubeSpatialObjectPoint<TPointDimension>
{
public:
  using Self = DTITubeSpatialObjectPoint;
  using Superclass = TubeSpatialObjectPoint<TPointDimension>;
  using PointType = Point<double, TPointDimension>;
  using VectorType = Vector<double, TPointDimension>;
  using CovariantVectorType = CovariantVector<double, TPointDimension>;
  using FieldType = std::pair<std::string, float>;
  using FieldListType = std::vector<FieldType>;

  using DTITubeSpatialObjectPointFieldEnum = DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField;
#if !defined(ITK_LEGACY_REMOVE)
  /** Enables backwards compatibility for enum values */
  using FieldEnumType = DTITubeSpatialObjectPointFieldEnum;
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr DTITubeSpatialObjectPointFieldEnum FA = DTITubeSpatialObjectPointFieldEnum::FA;
  static constexpr DTITubeSpatialObjectPointFieldEnum ADC = DTITubeSpatialObjectPointFieldEnum::ADC;
  static constexpr DTITubeSpatialObjectPointFieldEnum GA = DTITubeSpatialObjectPointFieldEnum::GA;
#endif

  /** Constructor. This one defines the number of dimensions in the
   * DTITubeSpatialObjectPoint */
  DTITubeSpatialObjectPoint();

  /** Copy Constructor */
  DTITubeSpatialObjectPoint(const DTITubeSpatialObjectPoint & other);

  /** Default destructor. */
  ~DTITubeSpatialObjectPoint() override = default;

  /** Set/Get the tensor matrix */
  void
  SetTensorMatrix(const DiffusionTensor3D<double> & matrix)
  {
    std::copy(matrix.Begin(), matrix.End(), m_TensorMatrix);
  }

  void
  SetTensorMatrix(const DiffusionTensor3D<float> & matrix)
  {
    std::copy(matrix.Begin(), matrix.End(), m_TensorMatrix);
  }

  void
  SetTensorMatrix(const float * matrix)
  {
    for (unsigned int i = 0; i < 6; i++)
    {
      m_TensorMatrix[i] = matrix[i];
    }
  }

  const float *
  GetTensorMatrix() const
  {
    return m_TensorMatrix;
  }

  /** Copy one DTITubeSpatialObjectPoint to another */
  Self &
  operator=(const DTITubeSpatialObjectPoint & rhs);

  /** Add a field to the point list */
  void
  AddField(const char * name, float value);

  /** Add a field to the point list */
  void
  AddField(DTITubeSpatialObjectPointFieldEnum name, float value);

  /** Set a field value */
  void
  SetField(DTITubeSpatialObjectPointFieldEnum name, float value);

  void
  SetField(const char * name, float value);

  /** Return the list of extra fields */
  const FieldListType &
  GetFields() const
  {
    return m_Fields;
  }

  /** Return the value of the specific fiedls */
  float
  GetField(const char * name) const;

  float
  GetField(DTITubeSpatialObjectPointFieldEnum name) const;

protected:
  float         m_TensorMatrix[6];
  FieldListType m_Fields;

  /** Print the object */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Translate the enum to char */
  std::string
  TranslateEnumToChar(DTITubeSpatialObjectPointFieldEnum name) const;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDTITubeSpatialObjectPoint.hxx"
#endif

#endif // itkDTITubeSpatialObjectPoint_h
