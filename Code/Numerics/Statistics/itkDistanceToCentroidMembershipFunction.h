/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceToCentroidMembershipFunction_h
#define __itkDistanceToCentroidMembershipFunction_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_transpose.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_determinant.h>


#include "itkMembershipFunctionBase.h"

#define PI 3.141592

namespace itk{ 
  namespace Statistics{

/** \class DistanceToCentroidMembershipFunction
 * \brief DistanceToCentroidMembershipFunction class represents DistanceToCentroid Density Function.
 *
 * This class keeps parameter to define DistanceToCentroid Density Function  and has
 * method to return the probability density 
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement. 
 */

template< class TVector >
class ITK_EXPORT DistanceToCentroidMembershipFunction :
      public MembershipFunctionBase< TVector >
{
public:
  /** Standard class typedefs */
  typedef DistanceToCentroidMembershipFunction Self;
  typedef MembershipFunctionBase< TVector > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(DistanceToCentroidMembershipFunction, MembershipFunctionBase);
  itkNewMacro(Self);
  
  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType ;

  /** Method to set mean */
  void SetCentroid(const vnl_vector<double> &centroid) ;
  
  /** Method to get mean */
  const vnl_vector<double> & GetCentroid() const;

  /** Method to set the number of samples */
  itkSetMacro( NumberOfSamples, int );

  /** Method to get the number of samples */
  itkGetMacro( NumberOfSamples, int );

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType &measurement) const;
  
protected:
  DistanceToCentroidMembershipFunction(void) ;
  virtual ~DistanceToCentroidMembershipFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  vnl_vector< double >  m_Centroid;              // mean

  // Number of samples defining this density
  int m_NumberOfSamples;

  itkStaticConstMacro(VectorDimension, unsigned int, TVector::Dimension);
  typedef vnl_matrix_fixed<double,1,itkGetStaticConstMacro(VectorDimension)> ColumnVectorType;

  mutable ColumnVectorType      m_TempVec;
  mutable ColumnVectorType      m_TempMat;
};

  } // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceToCentroidMembershipFunction.txx"
#endif

#endif
