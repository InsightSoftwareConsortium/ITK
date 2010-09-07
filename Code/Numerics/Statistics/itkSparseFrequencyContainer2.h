/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer2.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseFrequencyContainer2_h
#define __itkSparseFrequencyContainer2_h

#include <map>
#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkNumericTraits.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class SparseFrequencyContainer2
 *  \brief his class is a container for an histogram.
 *
 *  This class uses an map to store histogram. If your histogram is dense
 *  use DenseHistogram.  You should access each bin by
 * (InstanceIdentifier)index or measurement vector.
 */

class ITK_EXPORT SparseFrequencyContainer2:public Object
{
public:
  /** Standard class typedefs. */
  typedef SparseFrequencyContainer2  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard macros */
  itkTypeMacro(SparseFrequencyContainer2, Object);
  itkNewMacro(Self);

  /** instance idenfitifer alias */
  typedef MeasurementVectorTraits::InstanceIdentifier InstanceIdentifier;

  /** Absolute frequency type alias */
  typedef MeasurementVectorTraits::AbsoluteFrequencyType AbsoluteFrequencyType;

  /** Absolute Total frequency type */
  typedef MeasurementVectorTraits::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;

  /** Relative frequency type alias */
  typedef MeasurementVectorTraits::RelativeFrequencyType RelativeFrequencyType;

  /** Relative Relative frequency type */
  typedef MeasurementVectorTraits::TotalRelativeFrequencyType TotalRelativeFrequencyType;

  /** Histogram typedef support */
  typedef std::map< InstanceIdentifier, AbsoluteFrequencyType > FrequencyContainerType;
  typedef FrequencyContainerType::const_iterator
  FrequencyContainerConstIterator;

  /** prepares the frequency container */
  void Initialize(unsigned long length);

  /** Calls the SetToZero method of superclass to initialize all the bins to Zero.
   *  This should be done before starting to call the IncreaseFrequency method. */
  void SetToZero();

  /** Method to set the frequency of histogram using instance identifier. It
   * returns false when the Id is out of bounds */
  bool SetFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value);

  /** Method to increase the frequency by one.  This function is convinent
   * to create a histogram. It returns false when the id is out of bounds. */
  bool IncreaseFrequency(const InstanceIdentifier id,
                         const AbsoluteFrequencyType value);

  /** Method to get the frequency of a bin from the histogram. It will return
   * zero when the Id is out of bounds.  */
  AbsoluteFrequencyType GetFrequency(const InstanceIdentifier id) const;

  TotalAbsoluteFrequencyType GetTotalFrequency()
  {
    return m_TotalFrequency;
  }

protected:
  SparseFrequencyContainer2();
  virtual ~SparseFrequencyContainer2() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  SparseFrequencyContainer2(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented

  // Container of histogram
  FrequencyContainerType     m_FrequencyContainer;
  TotalAbsoluteFrequencyType m_TotalFrequency;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
