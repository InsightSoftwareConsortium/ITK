/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMixtureModelComponentBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMixtureModelComponentBase_h
#define __itkMixtureModelComponentBase_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

#include "itkObject.h"
#include "itkMembershipFunctionBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class MixtureModelComponentBase
 * \brief calculates sample mean
 *
 * You plug in the target sample data using SetSample method. Then call
 * the GenerateData method to run the alogithm.
 *
 * The return value that the GetOutput method 
 * \f$ = \frac{1}{n}\sum^{n}_{i=1} \f$ where \f$n\f$ is the
 * number of measurement vectors in the target 
 *
 */

template< class TSample >
class MixtureModelComponentBase :
    public Object
{
public:
  /**Standard class typedefs. */
  typedef MixtureModelComponentBase Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /**Standard Macros */
  itkTypeMacro(MixtureModelComponentBase, Object);
  itkNewMacro(Self) ;
  
  /** Sample typedefs alias */
  typedef TSample SampleType ;
  typedef typename TSample::Pointer SamplePointer ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  typedef MembershipFunctionBase< MeasurementVectorType >
  MembershipFunctionType ;
  
  typedef typename MembershipFunctionType::Pointer MembershipFunctionPointer ;

  typedef Array< double > WeightArrayType ;

  /** Stores the sample pointer */
  virtual void SetSample(SamplePointer sample) ;
  
  /** Returns the sample pointer */
  SamplePointer GetSample() ;

  MembershipFunctionPointer GetMembershipFunction() ;

  void AreParametersModified(bool flag) ;

  bool AreParametersModified() ;

  void SetWeight(int index, double value) ;
  
  double GetWeight(int index) ;

  double Evaluate(MeasurementVectorType& measurements) ;

  WeightArrayType* GetWeights() ;

  virtual void Update() ;

protected:
  MixtureModelComponentBase() ;
  virtual ~MixtureModelComponentBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void CreateWeightArray() ;
  void DeleteWeightArray() ;
  void SetMembershipFunction(MembershipFunctionPointer function) ;
  virtual void GenerateData() ;

private:
  /** Target sample data pointer */
  SamplePointer m_Sample ;
  MembershipFunctionPointer m_MembershipFunction ;
  WeightArrayType* m_Weights ;
  bool m_ParametersModified ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMixtureModelComponentBase.txx"
#endif

#endif

