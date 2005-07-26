/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleAlgorithmBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSampleAlgorithmBase_h
#define __itkSampleAlgorithmBase_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkMeasurementVectorTraits.h"

namespace itk{ 
  namespace Statistics{
  
/** \class SampleAlgorithmBase
 * \brief This class is a base class for algorithms that operate on Sample
 * data. The class is templated over the SampleType, which it takes as
 * input using the SetInputSample() method. Derived classes that operate
 * or calculate statistics on this input sample data and can access it
 * using the GetInputSample() method.
 */

template< class TInputSample >
class ITK_EXPORT SampleAlgorithmBase : public Object
{
public:
  /**Standard class typedefs. */
  typedef SampleAlgorithmBase Self;
  typedef Object Superclass ;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**Standard Macros */
  itkTypeMacro(SampleAlgorithmBase, Object);
  itkNewMacro(Self) ;
  
  /** Length of a measurement vector */
  typedef unsigned int MeasurementVectorSizeType;

  /** Sample typedefs alias */
  typedef TInputSample                                    InputSampleType;
  typedef typename InputSampleType::MeasurementVectorType MeasurementVectorType;

  /** Stores the sample pointer */
  void SetInputSample( const TInputSample * sample ) 
  {
    if ( m_InputSample != sample )
      {
        m_InputSample = sample ;
        m_MeasurementVectorSize = m_InputSample->GetMeasurementVectorSize();
        this->Modified() ;
      }
  }


  /** Get Macro to get the length of a measurement vector. This is equal to 
   * the length of each measurement vector contained in the samples that are
   * plugged in as input to this class. GetMeasurementVectorSize() will return 
   * zero until the SetInputSample() method has been called */
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType );
  itkSetMacro( MeasurementVectorSize, MeasurementVectorSizeType )

  const TInputSample * GetInputSample() const
  { return m_InputSample.GetPointer() ; }

  /** dummy function that calls the GenerateData() function to generate
   * output. It exists for future compatibility with ProcessObject 
   * without streaming */
  void Update()
  { this->GenerateData() ; }
    
protected:
  SampleAlgorithmBase() ;
  virtual ~SampleAlgorithmBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the mean and save it */
  virtual void GenerateData() ;

private:
  /** Length of each measurement vector */
  MeasurementVectorSizeType m_MeasurementVectorSize;
  
  /** Target sample data pointer */
  typename TInputSample::ConstPointer m_InputSample ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleAlgorithmBase.txx"
#endif

#endif

