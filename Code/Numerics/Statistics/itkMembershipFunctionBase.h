/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMembershipFunctionBase_h
#define __itkMembershipFunctionBase_h

#include "itkFunctionBase.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class MembershipFunctionBase
 * \brief MembershipFunctionBase class declares common interfaces
 * for membership functions.
 *
 * As a function derived from FunctionBase, users use Evaluate method
 * get result. However, the return value type of the method is fixed
 * as double. Any function derived from this class returns quantitative
 * measure for how well the vector x belong to the class ( or group)
 * represented by the function.
 */

template< class TVector >
class ITK_EXPORT MembershipFunctionBase:
  public FunctionBase< TVector, double >
{
public:
  /** Standard class typedefs */
  typedef MembershipFunctionBase          Self;
  typedef FunctionBase< TVector, double > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Strandard macros */
  itkTypeMacro(MembershipFunctionBase, FunctionBase);

  /** MeasurementVector typedef support */
  typedef TVector MeasurementVectorType;

  /** Typedef for the length of each measurement vector */
  typedef unsigned int MeasurementVectorSizeType;

  /** Method to get membership score (discriminant score) of an entity. */
  virtual double Evaluate(const MeasurementVectorType & x) const = 0;

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
      MeasurementVectorSizeType defaultLength = MeasurementVectorTraits::GetLength(m3);
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
  MembershipFunctionBase()
  {
    m_MeasurementVectorSize = MeasurementVectorTraits::GetLength(
      MeasurementVectorType() );
  }

  virtual ~MembershipFunctionBase(void) {}

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Length of measurement vectors: "
       << m_MeasurementVectorSize << std::endl;
  }

  MeasurementVectorSizeType m_MeasurementVectorSize;
};  // end of class
} // end of namespace Statistics
} // end namespace itk

#endif
