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
#ifndef itkDistanceMetric_h
#define itkDistanceMetric_h

#include "itkFunctionBase.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class DistanceMetric
 * \brief this class declares common interfaces
 * for distance functions.
 *
 * As a function derived from FunctionBase, users use Evaluate method to get
 * result.
 *
 * To use this function users should first set the origin by calling
 * SetOrigin() function, then call Evaluate() method with a point to get the
 * distance between the origin point and the evaluation point.
 *
 * \sa EuclideanSquareDistanceMetric
 * \sa EuclideanDistanceMetric
 * \sa ManhattanDistanceMetric
 *
 * \ingroup ITKStatistics
 */

template< typename TVector >
class ITK_TEMPLATE_EXPORT DistanceMetric:public FunctionBase< TVector, double  >
{
public:
  /** Standard typedefs */
  typedef DistanceMetric                  Self;
  typedef FunctionBase< TVector, double > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** declare the MeasurementVector type */
  typedef TVector MeasurementVectorType;

  /** declare Measurement vector component type */
  /** Type used to represent the number of components oft he
    MeasurementVectorType */
  typedef unsigned int MeasurementVectorSizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DistanceMetric, FunctionBase);

  typedef Array< double > OriginType;

  /** Sets the origin point that will be used for the single point version
   * Evaluate() function. This function is necessary part of implementing the
   * Evaluate() interface. The argument of SetOrigin() must be of type
   * DistanceMetric::OriginType, which in most cases will be different from the
   * TVector type. This is necessary because often times the origin would be a
   * mean, or a vector representative of a collection of vectors. */
  void SetOrigin(const OriginType & x);

  itkGetConstReferenceMacro(Origin, OriginType);

  /** Gets the distance between the origin point and x. This function
   * work with SetOrigin() function. */
  virtual double Evaluate(const MeasurementVectorType & x) const ITK_OVERRIDE = 0;

  /** Gets the distance between x1 and x2. This method is used by
    * KdTreeKMeans estimators. When the estimator is refactored,
    * this method should be removed. Distance between two measurement
    * vectors can be computed by setting one of them as an origin of
    * the distane and using the Evaluate method with a single argument */
  virtual double Evaluate(const MeasurementVectorType & x1,
                          const MeasurementVectorType & x2) const = 0;

  /** Set method for the length of the measurement vector */
  virtual void SetMeasurementVectorSize(MeasurementVectorSizeType s)
  {
    // Test whether the vector type is resizable or not
    MeasurementVectorType m;

    if ( MeasurementVectorTraits::IsResizable(m) )
      {
      // then this is a resizable vector type
      //
      // if the new size is the same as the previou size, just return
      if ( s == this->m_MeasurementVectorSize )
        {
        return;
        }
      else
        {
        this->m_MeasurementVectorSize = s;
        this->Modified();
        }
      }
    else
      {
      // If this is a non-resizable vector type
      MeasurementVectorType     m3;
      MeasurementVectorSizeType defaultLength =
        NumericTraits<MeasurementVectorType>::GetLength(m3);
      // and the new length is different from the default one, then throw an
      // exception
      if ( defaultLength != s )
        {
        itkExceptionMacro(
          "Attempting to change the measurement \
                           vector size of a non-resizable vector type"                                                  );
        }
      }
  }

  /** Get method for the length of the measurement vector */
  itkGetConstMacro(MeasurementVectorSize, MeasurementVectorSizeType);

protected:
  DistanceMetric();
  virtual ~DistanceMetric() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  OriginType m_Origin;

  /** Number of components in the MeasurementVectorType */
  MeasurementVectorSizeType m_MeasurementVectorSize;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceMetric.hxx"
#endif

#endif
