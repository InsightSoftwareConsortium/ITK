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
#ifndef itkMembershipFunctionBase_h
#define itkMembershipFunctionBase_h

#include "itkFunctionBase.h"
#include "itkMeasurementVectorTraits.h"
#include "itkNumericTraitsCovariantVectorPixel.h"

namespace itk
{
namespace Statistics
{
/** \class MembershipFunctionBase
 * \brief MembershipFunctionBase defines common interfaces
 * for membership functions.
 *
 * MembershipFunctionBase is a subclass of FunctionBase which
 * restricts the function type to be a membership function. Membership
 * functions provide a mapping from an arbitrary domain to a set of
 * real numbers. Membership functions are typically used to model or
 * approximate likelihood functions, \f$p( x | i )\f$, i.e. the
 * probability of the measurement \f$x\f$ belonging to a class
 * \f$i\f$.
 *
 * The Statistics framework models random variables \f$x\f$ as
 * vectors. Typical uses of MembershipFunctions include templating
 * over a FixedArray, Array, Vector, or VariableLengthVector.
 *
 * The Evaluate() method returns the membership rank or likelihood
 * that the measurement belongs to the class represented by this
 * membership function.
 *
 * Evaluations of a single measurement across of set MembershipFunctions
 * can then be passed to a DecisionRule in order to establish
 * class (or group) assignment.
 *
 * \ingroup ITKStatistics
 */

template< typename TVector >
class ITK_TEMPLATE_EXPORT MembershipFunctionBase:
  public FunctionBase< TVector, double >
{
public:
  /** Standard class typedefs */
  typedef MembershipFunctionBase          Self;
  typedef FunctionBase< TVector, double > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Standard macros */
  itkTypeMacro(MembershipFunctionBase, FunctionBase);

  /** MeasurementVector typedef support */
  typedef TVector MeasurementVectorType;

  /** Typedef for the length of each measurement vector */
  typedef unsigned int MeasurementVectorSizeType;

  /** Method to get membership score (discriminant score) of an entity
   * or measurement. Evaluate() maps from a vector measurement type
   * to a real number. */
  virtual double Evaluate(const MeasurementVectorType & x) const ITK_OVERRIDE = 0;

  /** Set the length of the measurement vector. If this membership
   * function is templated over a vector type that can be resized,
   * the new size is set. If the vector type has a fixed size and an
   * attempt is made to change its size, an exception is
   * thrown. Subclasses may have to override this method if a change
   * in vector size requires invalidating other instance variables,
   * e.g. covariance matrices, mean vectors, etc. */
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
          "Attempting to change the measurement vector size of a non-resizable vector type" );
        }
      }
  }

  /** Get the length of the measurement vector */
  itkGetConstMacro(MeasurementVectorSize, MeasurementVectorSizeType);

protected:
  MembershipFunctionBase()
  {
    m_MeasurementVectorSize = NumericTraits<MeasurementVectorType>::GetLength(
      MeasurementVectorType() );
  }

  virtual ~MembershipFunctionBase(void) ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Length of measurement vectors: "
       << m_MeasurementVectorSize << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MembershipFunctionBase);

  MeasurementVectorSizeType m_MeasurementVectorSize;

};  // end of class
} // end of namespace Statistics
} // end namespace itk

#endif
