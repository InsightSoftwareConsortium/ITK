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
#ifndef itkAntiAliasBinaryImageFilter_h
#define itkAntiAliasBinaryImageFilter_h
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkCurvatureFlowFunction.h"

namespace itk
{
/**
 * \class AntiAliasBinaryImageFilter
 * \brief A method for estimation of a surface from a binary volume.
 *
 * \par
 * This filter implements a surface-fitting method for estimation of a
 * surface from a binary volume.  This process can be used to reduce aliasing
 * artifacts which result in visualization of binary partitioned surfaces.
 *
 * \par
 * The binary volume (filter input) is used as a set of constraints in an
 * iterative relaxation process of an estimated ND surface.  The surface is
 * described implicitly as the zero level set of a volume \f$ \phi \f$ and
 * allowed to deform under curvature flow.  A set of constraints is imposed
 * on this movement as follows:
 *
 * \par
 * \f[ u_{i,j,k}^{n+1} = \left\{ \begin{array}{ll}
 * \mbox{max} (u_{i,j,k}^{n} + \Delta t H_{i,j,k}^{n}, 0) & \mbox{$B_{i,j,k} = 1$} \\
 * \mbox{min} (u_{i,j,k}^{n} + \Delta t H_{i,j,k}^{n}, 0) & \mbox{$B_{i,j,k} = -1$}
 * \end{array}\right.  \f]
 *
 * \par
 * where \f$ u_{i,j,k}^{n} \f$ is the value of \f$ \phi \f$ at discrete index
 * \f$ (i,j,k) \f$ and iteration \f$ n \f$, \f$ H \f$ is the gradient magnitude
 * times mean curvature of \f$ \phi \f$, and \f$ B \f$ is the binary input
 * volume, with 1 denoting an inside pixel and -1 denoting an outside pixel.
 *
 * \par NOTES
 * This implementation uses a sparse field level set solver instead of the
 * narrow band implementation described in the reference below, which may
 * introduce some differences in how fast and how accurately (in terms of RMS
 * error) the solution converges.
 *
 * \par REFERENCES
 * Whitaker, Ross.  "Reducing Aliasing Artifacts In Iso-Surfaces of Binary
 * Volumes"  IEEE Volume Visualization and Graphics Symposium, October 2000,
 * pp.23-32.
 *
 * \par PARAMETERS
 *  The MaximumRMSChange parameter is used to determine when the solution has
 *  converged.  A lower value will result in a tighter-fitting solution, but
 *  will require more computations.  Too low a value could put the solver into
 *  an infinite loop.  Values should always be less than 1.0.  A value of 0.07
 *  is a good starting estimate.
 *
 *  \par
 *  The MaximumIterations parameter can be used to halt the solution after a
 *  specified number of iterations.
 *
 * \par INPUT
 *  The input is an N-dimensional image of any type. It is assumed to be a
 *  binary image. The filter will use an isosurface value that is halfway
 *  between the min and max values in the image.  A signed data type is *not*
 *  necessary for the input.
 *
 * \par OUTPUT
 *  The filter will output a level set image of real, signed values.  The zero
 *  crossings of this (N-dimensional) image represent the position of the
 *  isosurface value of interest. Values outside the zero level set are
 *  negative and values inside the zero level set are positive values.
 *
 * \par IMPORTANT!
 *  The output image type you use to instantiate this filter should be a real
 *  valued scalar type.  In other words: doubles or floats.
 *
 * \par USING THIS FILTER
 *  The filter is relatively straightforward to use.  Tests and examples exist
 *  to illustrate.  The important thing is to understand the input and output
 *  types so you can properly interperet your results.
 *
 * \par
 *  In the common case, the only parameter that will need to be set is the
 *  MaximumRMSChange parameter, which determines when the solver halts.
 *
 * \ingroup ITKAntiAlias
 *
 * \wiki
 * \wikiexample{Smoothing/AntiAliasBinaryImageFilter,Anti alias a binary image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT AntiAliasBinaryImageFilter:
  public SparseFieldLevelSetImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef AntiAliasBinaryImageFilter                                  Self;
  typedef SparseFieldLevelSetImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType       ValueType;
  typedef typename Superclass::IndexType       IndexType;
  typedef typename Superclass::TimeStepType    TimeStepType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImageType  InputImageType;

  /** The function type which will calculate the curvature flow */
  typedef CurvatureFlowFunction< OutputImageType > CurvatureFunctionType;

  /** ValueType of the input binary image */
  typedef typename TInputImage::ValueType BinaryValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AntiAliasBinaryImageFilter, SparseFieldLevelSetImageFilter);

  /** Get the upper and lower binary values in the input image. */
  itkGetConstMacro(UpperBinaryValue, BinaryValueType);
  itkGetConstMacro(LowerBinaryValue, BinaryValueType);

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

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< typename TInputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  AntiAliasBinaryImageFilter();
  ~AntiAliasBinaryImageFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Overridden from the parent class to indroduce a constraint on
   *  surface flow under certain conditions. */
  virtual ValueType CalculateUpdateValue(const IndexType & idx,
                                         const TimeStepType & dt,
                                         const ValueType & value,
                                         const ValueType & change) ITK_OVERRIDE;

  /** Overridden from ProcessObject to set certain values before starting the
    * finite difference solver and then create an appropriate output */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AntiAliasBinaryImageFilter);

  BinaryValueType m_UpperBinaryValue;
  BinaryValueType m_LowerBinaryValue;

  typename CurvatureFunctionType::Pointer m_CurvatureFunction;

  const TInputImage *m_InputImage;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAntiAliasBinaryImageFilter.hxx"
#endif

#endif
