/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetImageFilter_h_
#define __itkSegmentationLevelSetImageFilter_h_

#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkSegmentationLevelSetFunction.h"

namespace itk {

template <class TInputImage, class TOutputImage>
class SegmentationLevelSetImageFilter
  : public SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs */
  typedef SegmentationLevelSetImageFilter Self;
  typedef SparseFieldLevelSetImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

 /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::TimeStepType TimeStepType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImageType  InputImageType;

  /** The generic level set function type */
  typedef SegmentationLevelSetFunction<TOutputImage> SegmentationFunctionType;

  /** Feature image type */
  typedef typename SegmentationFunctionType::FeatureImageType FeatureImageType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationLevelSetImageFilter, SparseFieldLevelSetImageFilter);

  /** Set/Get the maximum RMS error allowed for the solution.  The solver will
   *  halt once this threshold has been reached. */
  itkSetMacro(MaximumRMSError, ValueType);
  itkGetMacro(MaximumRMSError, ValueType);

  /** Set/Get the maximum number of iterations allowed for the solver.  This
   *  prevents infinite loops if a solution "bounces". */
  itkSetMacro(MaximumIterations, unsigned int);
  itkGetMacro(MaximumIterations, unsigned int); 

  /** Set/Get the feature image to be used for speed function of the level set
   *  equation */
  virtual void SetFeatureImage(FeatureImageType *f)
  {
    m_SegmentationFunction->SetFeatureImage(f);
    this->Modified();
  }
  virtual FeatureImageType * GetFeatureImage() const
  { return m_SegmentationFunction->GetFeatureImage(); }

  virtual typename SegmentationFunctionType::ImageType *GetSpeedImage() const
  { return m_SegmentationFunction->GetSpeedImage(); }
  
protected:
  virtual ~SegmentationLevelSetImageFilter() {}
  SegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set the segmentation function.  This should only be called by a subclass
   *  of this object. */
  virtual void SetSegmentationFunction(SegmentationFunctionType *s);
  virtual SegmentationFunctionType *GetSegmentationFunction()
  { return m_SegmentationFunction; }
  
  /** Overridden from ProcessObject to set certain values before starting the
   * finite difference solver and then create an appropriate output */
  void GenerateData();

  /** Tells the solver when the solution has converged within the specified
   * parameters. */
  bool Halt();

private:
  unsigned int m_MaximumIterations;
  SegmentationFunctionType *m_SegmentationFunction;
  ValueType m_MaximumRMSError;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationLevelSetImageFilter.txx"
#endif

#endif

