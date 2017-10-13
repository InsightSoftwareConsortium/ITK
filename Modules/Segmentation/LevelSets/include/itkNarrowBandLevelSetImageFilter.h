/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNarrowBandLevelSetImageFilter_h
#define itkNarrowBandLevelSetImageFilter_h

#include "itkNarrowBandImageFilterBase.h"
#include "itkSegmentationLevelSetFunction.h"
#include "itkFastChamferDistanceImageFilter.h"
#include "itkIsoContourDistanceImageFilter.h"
#include "itkMath.h"

namespace itk
{
/**
 *
 * \class NarrowBandLevelSetImageFilter
 *
 * \brief A base class which defines the API for implementing a special class of
 * image segmentation filters using level set methods.
 *
 * \par OVERVIEW
 * This object defines the framework for a class of segmentation filters which
 * use level set methods.  These filters work by constructing a "feature image"
 * onto which the evolving level set locks as it moves.  In the feature image,
 * values that are close to zero are associated with object boundaries.  An
 * original (or preprocessed) image is given to the filter as the feature image
 * and a seed for the level set is given as the input of the filter.  The seed is
 * converted into a level set embedding which propagates according to the features
 * calculated from the original image.
 *
 * \par TEMPLATE PARAMETERS
 * There are two required and two optional template parameter for these
 * filters. Of the optional parameters, the last, TOutputImage, should not be
 * changed from its default.  It is only there to instantiate the parent class
 * correctly.
 *
 * TInputImage is the image type of the initial model you will input to the
 * filter using SetInput() or SetInitialImage().
 *
 * TFeatureImage is the image type of the image from which the filter will
 * calculate the speed term for segmentation (see INPUTS).
 *
 * TOutputPixelType is the data type used for the output image phi, the implicit
 * level set image.  This should really only ever be set as float (default) or
 * double.
 *
 * \par INPUTS
 * The input to any subclass of this filter is the seed image for the initial
 * level set embedding.  As with other subclasses of the
 * SparseLevelSetImageFilter, the type of the input image is is not important.
 * The (RequestedRegion) size of the seed image must, however, match the
 * (RequestedRegion) size of the feature image.
 *
 * You must identify the initial front (surface) in the input image.  You do
 * this by specifying its isovalue through the method SetIsoSurfaceValue(float
 * f).  The algorithm will then initialize its solution using the front represented by
 * value f.  Note that the front is always represented by isosurface zero in
 * the output and not the isosurface you specified for the input.  This is
 * because, for simplicity, the filter will shift your input image so that the
 * active front has zero values.
 *
 * \par
 * Depending on the particular application and filter that you are using, the
 * feature image should be preprocessed with some type of noise reduction
 * filtering.  The feature image input can be of any type, but it will be cast
 * to floating point before calculations are done.
 *
 * \par OUTPUTS
 * The output of any subclass of this filter is a level set embedding as
 * described in SparseFieldLevelSetImageFilter.  The zero crossings of the output
 * image give the pixels closest to the level set boundary.  By ITK convention,
 * NEGATIVE values are pixels INSIDE the segmented region and POSITIVE values are
 * pixels OUTSIDE the segmented region.
 *
 * \par PARAMETERS
 * The MaximumRMSChange parameter is used to determine when the solution has
 * converged.  A lower value will result in a tighter-fitting solution, but
 * will require more computations.  Too low a value could put the solver into
 * an infinite loop unless a reasonable NumberOfIterations parameter is set.
 * Values should always be greater than 0.0 and less than 1.0.
 *
 * \par
 * The NumberOfIterations parameter can be used to halt the solution after a
 * specified number of iterations, overriding the MaximumRMSChange halting
 * criteria.
 *
 * \par
 * The standard convention for ITK level-set segmentation filters is that
 * POSITIVE propagation (speed) and advection terms cause the surface to EXPAND
 * while negative terms cause the surface to CONTRACT.  When the
 * ReverseExpansionDirection parameter is set to TRUE (on), it tells the
 * function object to reverse the standard ITK convention so that NEGATIVE
 * terms cause EXPANSION and positive terms cause CONTRACTION.
 *
 * This parameter can be safely changed as appropriate for a particular
 * application or data set to achieve the desired behavior.
 *
 * \par
 * The FeatureScaling parameter controls the magnitude of the features calculated
 * for use in the level set propagation and advection speeds.  This value simply
 * sets both parameters to equal values at once.  Some filters may only use on of
 * these two terms and this method is a generic way to set either or both without
 * having to know which is in use.
 *
 * \par
 * The CurvatureScaling parameter controls the magnitude of the curvature values
 * which are calculated on the evolving isophote.  This is important in
 * controlling the relative effect of curvature in the calculation.  Default
 * value is 1.0.  Higher values relative to the other level set equation terms
 * (propagation and advection) will give a smoother result.
 *
 * \par
 * The PropagationScaling parameter controls the scaling of the scalar
 * propagation (speed) term relative to other terms in the level set
 * equation. Setting this value will  override any value already set by
 * FeatureScaling.
 *
 * \par
 * The AdvectionScaling parameter controls the scaling of the vector
 * advection field term relative to other terms in the level set
 * equation. Setting this value will  override any value already set by
 * FeatureScaling.
 *
 * \par
 *  See LevelSetFunction for more information.
 * \ingroup ITKLevelSets
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputPixelType = float,
          typename TOutputImage = Image< TOutputPixelType,
                                      TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT NarrowBandLevelSetImageFilter:
  public NarrowBandImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef NarrowBandLevelSetImageFilter                          Self;
  typedef NarrowBandImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType      ValueType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::TimeStepType   TimeStepType;
  typedef typename Superclass::InputImageType InputImageType;

  /** Local image typedefs */
  typedef TOutputImage  OutputImageType;
  typedef TFeatureImage FeatureImageType;

  /** The generic level set function type */
  typedef SegmentationLevelSetFunction< OutputImageType, FeatureImageType >
  SegmentationFunctionType;

  /** The type used for the advection field */
  typedef typename SegmentationFunctionType::VectorImageType VectorImageType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NarrowBandLevelSetImageFilter, NarrowBandImageFilterBase);

  /** Set/Get the feature image to be used for speed function of the level set
   *  equation.  Equivalent to calling Set/GetInput(1, ..) */
  virtual void SetFeatureImage(const FeatureImageType *f)
  {
    this->ProcessObject::SetNthInput( 1, const_cast< FeatureImageType * >( f ) );
    m_SegmentationFunction->SetFeatureImage(f);
  }

  virtual FeatureImageType * GetFeatureImage()
  {
    return ( static_cast< FeatureImageType * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Set/Get the initial level set model.  Equivalent to calling SetInput(..)
      */
  virtual void SetInitialImage(InputImageType *f)
  {
    this->SetInput(f);
  }

  virtual const typename SegmentationFunctionType::ImageType * GetSpeedImage() const
  { return m_SegmentationFunction->GetSpeedImage(); }

  virtual const typename SegmentationFunctionType::VectorImageType * GetAdvectionImage() const
  { return m_SegmentationFunction->GetAdvectionImage(); }

  /** THIS METHOD IS DEPRECATED AND SHOULD NOT BE USED.  This method reverses
   * the speed function direction, effectively changing inside feature values to
   * outside feature values and vice versa. */
  void SetUseNegativeFeaturesOn()
  {
    itkWarningMacro(
      << "SetUseNegativeFeaturesOn has been deprecated.  Please use ReverseExpansionDirectionOn() instead");
    this->ReverseExpansionDirectionOn();
  }

  void SetUseNegativeFeaturesOff()
  {
    itkWarningMacro(
      << "SetUseNegativeFeaturesOff has been deprecated.  Please use ReverseExpansionDirectionOff() instead");
    this->ReverseExpansionDirectionOff();
  }

  /** Set/Get the value of the UseNegativeFeatures flag.  This method is
   * deprecated.  Use Set/Get ReverseExpansionDirection instead. */
  void SetUseNegativeFeatures(bool u)
  {
    itkWarningMacro(<< "SetUseNegativeFeatures has been deprecated.  Please use SetReverseExpansionDirection instead");
    if ( u == true )
      {
      this->SetReverseExpansionDirection(false);
      }
    else
      {
      this->SetReverseExpansionDirection(true);
      }
  }

  bool GetUseNegativeFeatures() const
  {
    itkWarningMacro(<< "GetUseNegativeFeatures has been deprecated.  Please use GetReverseExpansionDirection() instead");
    if ( this->GetReverseExpansionDirection() == false )
      {
      return true;
      }
    else
      {
      return false;
      }
  }

  /** Turn On/Off the flag which determines whether Positive or Negative speed
   * terms will cause surface expansion.  If set to TRUE then negative speed
   * terms will cause the surface to expand and positive speed terms will cause
   * the surface to contract.  If set to FALSE (default) then positive speed terms will
   * cause the surface to expand and negative speed terms will cause the
   * surface to contract.  This method can be safely used to reverse the
   * expansion/contraction as appropriate to a particular application or data
   * set. */
  itkSetMacro(ReverseExpansionDirection, bool);
  itkGetConstMacro(ReverseExpansionDirection, bool);
  itkBooleanMacro(ReverseExpansionDirection);

  /** Combined scaling of the propagation and advection speed
      terms. You should use either this -or- Get/SetPropagationScaling and
      Get/SetAdvectionScaling (if appropriate).  See subclasses for details
      on when and whether to set these parameters. */
  void SetFeatureScaling(ValueType v)
  {
    if ( v != m_SegmentationFunction->GetPropagationWeight() )
      {
      this->SetPropagationScaling(v);
      }
    if ( v != m_SegmentationFunction->GetAdvectionWeight() )
      {
      this->SetAdvectionScaling(v);
      }
  }

  /** Set/Get the scaling of the propagation speed.  Setting the FeatureScaling
      parameter overrides any previous values set for PropagationScaling. */
  void SetPropagationScaling(ValueType v)
  {
    if ( Math::NotExactlyEquals(v, m_SegmentationFunction->GetPropagationWeight()) )
      {
      m_SegmentationFunction->SetPropagationWeight(v);
      }
  }

  ValueType GetPropagationScaling() const
  {
    return m_SegmentationFunction->GetPropagationWeight();
  }

  /** Set/Get the scaling of the advection field.  Setting the FeatureScaling
      parameter will override any existing value for AdvectionScaling. */
  void SetAdvectionScaling(ValueType v)
  {
    if ( Math::NotExactlyEquals(v, m_SegmentationFunction->GetAdvectionWeight()) )
      {
      m_SegmentationFunction->SetAdvectionWeight(v);
      }
  }

  ValueType GetAdvectionScaling() const
  {
    return m_SegmentationFunction->GetAdvectionWeight();
  }

  /** Set/Get the scaling of the curvature. Use this parameter to increase the
    *  influence of curvature on the movement of the surface.  Higher
    *  values relative to Advection and Propagation values will give
    *  smoother surfaces. */
  void SetCurvatureScaling(ValueType v)
  {
    if ( Math::NotExactlyEquals(v, m_SegmentationFunction->GetCurvatureWeight()) )
      {
      m_SegmentationFunction->SetCurvatureWeight(v);
      }
  }

  ValueType GetCurvatureScaling() const
  {
    return m_SegmentationFunction->GetCurvatureWeight();
  }

  /** Set the segmentation function.  In general, this should only be called by a subclass
   *  of this object. It is made public to allow itk::Command objects access. */
  virtual void SetSegmentationFunction(SegmentationFunctionType *s);

  virtual SegmentationFunctionType * GetSegmentationFunction()
  { return m_SegmentationFunction; }

  /** Set/Get the maximum number of iterations allowed for the solver.  This
   *  prevents infinite loops if a solution "bounces". */
  void SetMaximumIterations(unsigned int i)
  {
    itkWarningMacro("SetMaximumIterations is deprecated.  Please use SetNumberOfIterations instead.");
    this->SetNumberOfIterations(i);
  }

  unsigned int GetMaximumIterations()
  {
    itkWarningMacro("GetMaximumIterations is deprecated. Please use GetNumberOfIterations instead.");
    return this->GetNumberOfIterations();
  }

  virtual void SetMaximumRMSError(const double) ITK_OVERRIDE
  {
    itkWarningMacro(
      "The current implmentation of this solver does not compute maximum RMS change. The maximum RMS error value will not be set or used.");
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  virtual ~NarrowBandLevelSetImageFilter() ITK_OVERRIDE {}
  NarrowBandLevelSetImageFilter();

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;


  /** Overrides parent implementation */
  virtual void InitializeIteration() ITK_OVERRIDE
  {
    Superclass::InitializeIteration();
    // Estimate the progress of the filter
    this->UpdateProgress( (float)( (float)this->GetElapsedIterations()
                                / (float)this->GetNumberOfIterations() ) );
  }

  /** Tells the solver how to reinitialize the narrowband when the reinitialization
    * criterion meets */

  virtual void CreateNarrowBand() ITK_OVERRIDE;

  /** Overridden from ProcessObject to set certain values before starting the
   * finite difference solver and then create an appropriate output */
  void GenerateData() ITK_OVERRIDE;

  /** Flag which sets the inward/outward direction of propagation speed. See
      SetReverseExpansionDirection for more information. */
  bool m_ReverseExpansionDirection;

  /** Reinitialization filters **/
  /** Internal filter types used for reinitialization */
  typedef IsoContourDistanceImageFilter< OutputImageType, OutputImageType >
  IsoFilterType;
  typedef FastChamferDistanceImageFilter< OutputImageType, OutputImageType >
  ChamferFilterType;

  typename IsoFilterType::Pointer m_IsoFilter;

  typename ChamferFilterType::Pointer m_ChamferFilter;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NarrowBandLevelSetImageFilter);

  SegmentationFunctionType *m_SegmentationFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNarrowBandLevelSetImageFilter.hxx"
#endif

#endif
