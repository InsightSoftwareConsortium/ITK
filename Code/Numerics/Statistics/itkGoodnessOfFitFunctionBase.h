/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGoodnessOfFitFunctionBase_h
#define __itkGoodnessOfFitFunctionBase_h

#include "itkSampleAlgorithmBase.h"
#include "itkFunctionBase.h"
#include "itkGoodnessOfFitComponentBase.h"

namespace itk{ 
namespace Statistics{

/** \class GoodnessOfFitFunctionBase
 *  \brief base class for classes calculates different types of goodness-of-fit 
 *  statistics
 *
 * Goodness-of-fit function quantifies discrepancy between the given model 
 * with the observation by summarizing the difference between the expected frequency
 * and the observed frequency.
 *
 * \sa GoodnessOfFitMixtureModelCostFunction, GoodnessOfFitComponentBase
 */

template< class TInputHistogram >
class GoodnessOfFitFunctionBase 
  : public SampleAlgorithmBase< TInputHistogram > 
{
public:
  /** Standard class typedefs */
  typedef GoodnessOfFitFunctionBase Self;
  typedef SampleAlgorithmBase< TInputHistogram > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GoodnessOfFitFunctionBase, 
               SampleAlgorithimBase) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedefs from Superclass */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TInputHistogram::MeasurementVectorSize) ;
  typedef typename TInputHistogram::MeasurementType MeasurementType ;
  typedef typename TInputHistogram::MeasurementVectorType MeasurementVectorType ;
  typedef TInputHistogram InputHistogramType ;

  /** Sets the observed histogram input */
  void SetObservedHistogram(InputHistogramType* histogram) ;

  /** Gets the observed histogram */
  InputHistogramType* GetObservedHistogram() 
  { return m_ObservedHistogram ; }

  /** Sets the expected histogram input */
  void SetExpectedHistogram(InputHistogramType* histogram) ;

  /** Gets the expected histogram input */
  InputHistogramType* GetExpectedHistogram() 
  { return m_ExpectedHistogram ; }

  /** Sets the flag that tells if a subclass needs
   * the expected histogram for goodness-of-fit statistics
   * calculation. This flag should be set only by a subclass */
  itkGetMacro(UseExpectedHistogram, bool) ;

  void SetTotalObservedScale(double* scale)
  { 
    if ( m_TotalObservedScale != scale )
      {
        m_TotalObservedScale = scale ;
        this->Modified() ;
      }
  }

  typedef double OutputType ;

  OutputType& GetOutput()
  { return m_Output ; }

protected:
  GoodnessOfFitFunctionBase() ;
  virtual ~GoodnessOfFitFunctionBase(){} ;
  void PrintSelf(std::ostream& os, Indent indent) const;  

  itkSetMacro(UseExpectedHistogram, bool) ;
  
  virtual void GenerateData() {}

  float m_Epsilon ;
  float m_LogEpsilon ;

private:
  bool m_UseExpectedHistogram ;

  InputHistogramType* m_ObservedHistogram ;
  InputHistogramType* m_ExpectedHistogram ;
  
  double* m_TotalObservedScale ;
  OutputType m_Output ;

} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGoodnessOfFitFunctionBase.txx"
#endif

#endif

