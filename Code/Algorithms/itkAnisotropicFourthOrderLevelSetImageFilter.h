/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicFourthOrderLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

     =========================================================================*/
#ifndef _itkAnisotropicFourthOrderLevelSetImageFilter_h_
#define _itkAnisotropicFourthOrderLevelSetImageFilter_h_

#include "itkLevelSetFunctionWithRefitTerm.h"
#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TOutputImage>
class ITK_EXPORT AnisotropicFourthOrderLevelSetImageFilter
  : public SparseFieldFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs */
  typedef AnisotropicFourthOrderLevelSetImageFilter Self;
  typedef SparseFieldFourthOrderLevelSetImageFilter <TInputImage,
                                                     TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(AnisotropicFourthOrderLevelSetImageFilter,
               SparseFieldFourthOrderLevelSetImageFilter);

  /** Standard new macro */
  itkNewMacro( Self );

  /** The sparse image type used in LevelSetFunctionWithRefitTerm */
  typedef typename Superclass::SparseImageType SparseImageType;

  /** The level set function class with a refit term that forces the curvature
      of the moving front to match a prescribed curvature image. */
  typedef LevelSetFunctionWithRefitTerm <TOutputImage,SparseImageType> FunctionType;

  /** The radius type for the neighborhoods. */
  typedef typename FunctionType::RadiusType RadiusType;

  itkGetMacro(MaxFilterIteration,int);
  itkSetMacro(MaxFilterIteration,int);
  
protected:
  AnisotropicFourthOrderLevelSetImageFilter();
  ~AnisotropicFourthOrderLevelSetImageFilter() {};
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** The LevelSetFunctionWithRefitTerm object. */
  typename FunctionType::Pointer m_Function;

  /** The number of iterations for which this filter will run. */
  int m_MaxFilterIteration;

  /** This filter halts when the iteration count reaches the specified count. */
  virtual bool Halt()
  {
    if (this->GetElapsedIterations()==m_MaxFilterIteration) return true;
    else return false;
  }

private:
  AnisotropicFourthOrderLevelSetImageFilter(const Self&);
  //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnisotropicFourthOrderLevelSetImageFilter.txx"
#endif

} // end namespace itk

#endif
