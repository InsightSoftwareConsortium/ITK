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
#ifndef itkDTITubeSpatialObjectPoint_h
#define itkDTITubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "itkDiffusionTensor3D.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/** \class DTITubeSpatialObjectPoint
 * \brief Point used for a tube definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build tubes.
 *
 * \sa DTITubeSpatialObject
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT DTITubeSpatialObjectPoint:
  public TubeSpatialObjectPoint< TPointDimension >
{
public:

  typedef DTITubeSpatialObjectPoint                  Self;
  typedef SpatialObjectPoint< TPointDimension >      Superclass;
  typedef Point< double, TPointDimension >           PointType;
  typedef Vector< double, TPointDimension >          VectorType;
  typedef CovariantVector< double, TPointDimension > CovariantVectorType;
  typedef std::pair< std::string, float >            FieldType;
  typedef std::vector< FieldType >                   FieldListType;

  // If you add a type here you need to modify the TranslateEnumToChar
  // to translate the enum to a string
  typedef enum { FA, ADC, GA } FieldEnumType;

  /** Constructor. This one defines the number of dimensions in the
   * DTITubeSpatialObjectPoint */
  DTITubeSpatialObjectPoint();

  /** Default destructor. */
  virtual ~DTITubeSpatialObjectPoint() ITK_OVERRIDE;

  /** Set/Get the tensor matrix */
  void SetTensorMatrix(const DiffusionTensor3D< double > & matrix)
  {
    std::copy(matrix.Begin(), matrix.End(), m_TensorMatrix);
  }

  void SetTensorMatrix(const DiffusionTensor3D< float > & matrix)
  {
    std::copy(matrix.Begin(), matrix.End(), m_TensorMatrix);
  }

  void SetTensorMatrix(const float *matrix)
  {
    for ( unsigned int i = 0; i < 6; i++ )
      {
      m_TensorMatrix[i] = matrix[i];
      }
  }

  const float * GetTensorMatrix() const { return m_TensorMatrix; }

  /** Copy one DTITubeSpatialObjectPoint to another */
  Self & operator=(const DTITubeSpatialObjectPoint & rhs);

  /** Add a field to the point list */
  void AddField(const char *name, float value);

  /** Add a field to the point list */
  void AddField(FieldEnumType name, float value);

  /** Set a field value */
  void SetField(FieldEnumType name, float value);

  void SetField(const char *name, float value);

  /** Return the list of extra fields */
  const FieldListType & GetFields() const { return m_Fields; }

  /** Return the value of the specific fiedls */
  float GetField(const char *name) const;

  float GetField(FieldEnumType name) const;

protected:

  float         m_TensorMatrix[6];
  FieldListType m_Fields;

  /** Print the object */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Translate the enum to char */
  std::string TranslateEnumToChar(FieldEnumType name) const;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDTITubeSpatialObjectPoint.hxx"
#endif

#endif // itkDTITubeSpatialObjectPoint_h
