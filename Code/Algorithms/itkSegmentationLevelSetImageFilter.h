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

/**
 *
 \class SegmentationLevelSetImageFilter
 
 \brief A base class which defines the API for implementing a special class of
 image segmentation filters using level set methods.

 \par OVERVIEW
 This object defines the framework for a class of segmentation filters which
 use level set methods.  These filters work by constructing a ``feature image''
 onto which the evolving level set locks as it moves.  In the feature image,
 values that are close to zero are associated with object boundaries.  An
 original (or preprocessed) image is given to the filter as the feature image
 and a seed for the level set is given as the input of the filter.  The seed is
 converted into a level set embedding which propagates according to the features
 calculated from the original image.

 \par INPUTS
 The input to any subclass of this filter is the seed image for the initial
 level set embedding.  As with other subclasses of the
 SparseLevelSetImageFilter, the type of the input image is is not important.
 The (RequestedRegion) size of the seed image must, however, match the
 (RequestedRegion) size of the feature image.

 \par
 Depending on the particular application and filter that you are using, the
 feature image should be preprocessed with some type of noise reduction
 filtering.  The feature image should be of a floating point type (floats or
 doubles).  You may need to cast your image to this type before attaching it to
 this filter.

 \par OUTPUTS
 The output of any subclass of this filter is a level set embedding as
 described in SparseFieldLevelSetImageFilter.  The zero crossings of the output
 image give the pixels closest to the level set boundary.  By ITK convention,
 positive values are pixels inside the segmented region and negative values are
 pixels outside the segmented region.

 \par PARAMETERS
 The MaximumRMSChange parameter is used to determine when the solution has
 converged.  A lower value will result in a tighter-fitting solution, but
 will require more computations.  Too low a value could put the solver into
 an infinite loop unless a reasonable MaximumIterations parameter is set.
 Values should always be greater than 0.0 and less than 1.0.
 
 \par
 The MaximumIterations parameter can be used to halt the solution after a
 specified number of iterations, overriding the MaximumRMSChange halting
 criteria.

 \par
 The UseNegativeFeatures parameter tells the function object to reverse the
 sign of the feature image, which also reverses the INSIDE OUTSIDE sign
 convention.

 \par
 The FeatureScaling parameter controls the magnitude of the features calculated
 for use in the level set propagation speed.  This is important in controlling
 the relative effect of the feature values versus the curvature values (and possibly other
 terms).  Default value is 1.0.

 \par
 The CurvatureScaling parameter controls the magnitude of the curvature values
 which are calculated on the evolving isophote.  This is important in
 controlling the relative effect of curvature in the calculation.  Default
 value is 1.0.
 
 \todo Use a second input image for the feature image instead of keeping the
 feature image as a parameter.  This may be tricky because the feature image
 may be of a different type as the first input (seed image) and so the default
 pipeline mechanism will have to be modified.  When this is done, however, it
 will allow the feature image to be properly updated in a unified pipeline with
 the seed image.
 */  
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SegmentationLevelSetImageFilter
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

  
  /** This method reverses the speed function direction, effectively changing
   *  inside feature values to outside feature values and vice versa */
  void SetUseNegativeFeaturesOn()
  {
    this->SetUseNegativeFeatures(true);
  }
  void SetUseNegativeFeaturesOff()
  {
    this->SetUseNegativeFeatures(false);
  }

  /** Set/Get the value of the UseNegativeFeatures flag.  This flag controls
   * whether (true) or not (false) the direction of the speed function is
   * reversed.  By default, level set segmentation filters take ``inside''
   * values as positive, and ``outside'' values as negative.*/
  itkSetMacro(UseNegativeFeatures, bool);
  itkGetMacro(UseNegativeFeatures, bool);

  /** Set/Get the scaling of the propagation speed. */
  itkSetMacro(FeatureScaling, ValueType);
  itkGetMacro(FeatureScaling, ValueType);

  /** Set/Get the scaling of the curvature. */
  itkSetMacro(CurvatureScaling, ValueType);
  itkGetMacro(CurvatureScaling, ValueType);

  /** Set the segmentation function.  In general, this should only be called by a subclass
   *  of this object. It is made public to allow itk::Command objects access. */
  virtual void SetSegmentationFunction(SegmentationFunctionType *s);
  virtual SegmentationFunctionType *GetSegmentationFunction()
  { return m_SegmentationFunction; }
  
protected:
  virtual ~SegmentationLevelSetImageFilter() {}
  SegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Overrides parent implementation */
  virtual void InitializeIteration()
  {
    Superclass::InitializeIteration();
    // Estimate the progress of the filter
    this->SetProgress( (float) ((float)this->GetElapsedIterations()
                                / (float)this->GetMaximumIterations()) );
  }
  
  
  /** Overridden from ProcessObject to set certain values before starting the
   * finite difference solver and then create an appropriate output */
  void GenerateData();

  /** Tells the solver when the solution has converged within the specified
   * parameters. */
  bool Halt();

  /** Flag which sets the inward/outward direction of positive propagation speed.*/
  bool m_UseNegativeFeatures;

  /** Scalar parameter for propagation speed.*/
  ValueType m_FeatureScaling;

  /** Scalar parameter for curvature.*/
  ValueType m_CurvatureScaling;
  
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

