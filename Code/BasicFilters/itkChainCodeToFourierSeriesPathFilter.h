/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodeToFourierSeriesPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkChainCodeToFourierSeriesPathFilter_h
#define _itkChainCodeToFourierSeriesPathFilter_h

#include "itkPathToPathFilter.h"
#include "itkOffset.h"
//Templates require interfaces conforming to itkPath.h and itkChainCodePath.h

namespace itk
{
  
/** \class ChainCodeToFourierSeriesPathFilter
 * \brief Filter that produces a Fourier series version of a chain code path
 *
 * ChainCodeToFourierSeriesPathFilter produces a Fourier series representation
 * of a chain code path. By default, the first 8 harmonics (frequency
 * coefficients, which include the "DC" term) are computed. 
 * SetNumberOfHarmonics() can be used to override this value.
 * Because the requested number of harmonics may not be able to be computed,
 * it is advisable to check the number of harmonics in the actual output.
 * 
 * \ingroup PathFilters
 */
template <class TInputChainCodePath, class TOutputFourierSeriesPath>
class ITK_EXPORT ChainCodeToFourierSeriesPathFilter : public
  PathToPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
{
public:
  /** Standard class typedefs. */
  typedef ChainCodeToFourierSeriesPathFilter  Self;
  typedef PathToPathFilter< TInputChainCodePath,
                            TOutputFourierSeriesPath >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodeToFourierSeriesPathFilter, PathToPathFilter);

  /** Some convenient typedefs. */
  typedef TInputChainCodePath                 InputPathType;
  typedef typename InputPathType::Pointer     InputPathPointer;
  typedef typename InputPathType::InputType   InputPathInputType;
  typedef TOutputFourierSeriesPath            OutputPathType;
  typedef typename OutputPathType::Pointer    OutputPathPointer;
  typedef typename OutputPathType::InputType  OutputPathInputType;
  typedef typename InputPathType::IndexType   IndexType;
  typedef typename InputPathType::OffsetType  OffsetType;
  typedef typename OutputPathType::VectorType VectorType;

  /** Set the number of harmonics to try to compute.  By default, the first 8
   * harmonics (frequency coefficients, which include the "DC" term) are
   * computed.  SetNumberOfHarmonics() can be used to override this value.  If
   * the chain code has too few steps to calculate the desired number of
   * harmonics (due to the Nyquist criterion), then as many harmonics as are
   * possible (input->NumberOfSteps()/2) will be calculated, but at least 2
   * harmonics will always be calculated.*/
  itkSetMacro( NumberOfHarmonics, unsigned int )
  
protected:
  ChainCodeToFourierSeriesPathFilter();
  virtual ~ChainCodeToFourierSeriesPathFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  ChainCodeToFourierSeriesPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_NumberOfHarmonics;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChainCodeToFourierSeriesPathFilter.txx"
#endif

#endif
