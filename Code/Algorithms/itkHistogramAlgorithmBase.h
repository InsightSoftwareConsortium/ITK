/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramAlgorithmBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHistogramAlgorithmBase_h
#define __itkHistogramAlgorithmBase_h

#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkObject.h"

namespace itk
{ 
  
/** \class HistogramAlgorithmBase
 * \brief base class for algorithms operating on histograms
 *
 * You plug in the target sample data using SetInputHistogram method. Then call
 * the GenerateData method to run the alogithm.
 *
 */

template< class TInputHistogram >
class HistogramAlgorithmBase : public Object
{
public:
  /**Standard class typedefs. */
  typedef HistogramAlgorithmBase Self;
  typedef Object Superclass ;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**Standard Macros */
  itkTypeMacro(HistogramAlgorithmBase, Object);
  
  /** Histogram typedefs alias */
  typedef TInputHistogram InputHistogramType ;

  /** Stores the histogram pointer */
  void SetInputHistogram( const TInputHistogram * histogram ) 
  {
    if ( m_InputHistogram != histogram )
      {
        m_InputHistogram = histogram ;
        this->Modified() ;
      }
  }

  /** Returns the histogram const pointer */
  const TInputHistogram * GetInputHistogram() const
  { return m_InputHistogram.GetPointer() ; }

  /** dummy function that calls the GenerateData() function to generate
   * output. It exists for future compatibility with ProcessObject 
   * without streaming */
  void Update()
  { this->GenerateData() ; }
    
protected:
  HistogramAlgorithmBase() ;
  virtual ~HistogramAlgorithmBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData() = 0;

private:
  /** Target histogram data pointer */
  typename TInputHistogram::ConstPointer m_InputHistogram ;
} ; // end of class
    
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramAlgorithmBase.txx"
#endif

#endif

