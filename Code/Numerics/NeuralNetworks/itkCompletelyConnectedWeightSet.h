/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompletelyConnectedWeightSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCompletelyConnectedWeightSet_h
#define __itkCompletelyConnectedWeightSet_h

#include "itkWeightSetBase.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
class CompletelyConnectedWeightSet : public WeightSetBase<TVector, TOutput>
{
public:
 
  #define MAX_SIZE 1000

  typedef CompletelyConnectedWeightSet Self;
  typedef WeightSetBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  itkTypeMacro(CompletelyConnectedWeightSet, WeightSetBase);      
  itkNewMacro(Self);

  void SetCompleteConnectivity();

  void SetRandomConnectivity(int[][MAX_SIZE]);

protected:

  CompletelyConnectedWeightSet();
  ~CompletelyConnectedWeightSet(){};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompletelyConnectedWeightSet.txx"
#endif

#endif
