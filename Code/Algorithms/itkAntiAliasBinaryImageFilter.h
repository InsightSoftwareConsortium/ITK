/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAntiAliasBinaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAntiAliasBinaryImageFilter_h_
#define __itkAntiAliasBinaryImageFilter_h_
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkCurvatureFlowFunction.h"

namespace itk {

/**
 * \class AntiAliasBinaryImageFilter
 *
 *\par
 * This filter implements a surface-fitting method for estimation of a
 * surface from a binary volume.  This process can be used to reduce aliasing
 * artifacts which result in visualization of binary partitioned surfaces.
 *
 * \par
 * The binary volume (filter input) is used as a set of constraints in an
 * iterative relaxation process of an estimated ND surface.  The surface is
 * described implicitly as the zero level set of a volume \f$ \phi \f$ and
 * allowed to deform under curvature flow.  A set of contraints is imposed
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
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AntiAliasBinaryImageFilter
  : public SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs */
  typedef AntiAliasBinaryImageFilter Self;
  typedef SparseFieldLevelSetImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::TimeStepType TimeStepType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImageType  InputImageType;

  
  /** The function type which will calculate the curvature flow */
  typedef CurvatureFlowFunction<OutputImageType> CurvatureFunctionType;
  
  /** ValueType of the input binary image */
  typedef typename TInputImage::ValueType  BinaryValueType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AntiAliasBinaryImageFilter, SparseFieldLevelSetImageFilter);

  /** Set/Get the maximum RMS error allowed for the solution.  The solver will
   *  halt once this threshold has been reached. */
  itkSetMacro(MaximumRMSError, ValueType);
  itkGetMacro(MaximumRMSError, ValueType);

  /** Set/Get the maximum number of iterations allowed for the solver.  This
   *  prevents infinite loops when a solution "bounces".  The filter halt and
   *  issue a warning when this number is reached because the MaximumRMSError
   *  criteria will not have been met.*/
  itkSetMacro(MaximumIterations, unsigned int);
  itkGetMacro(MaximumIterations, unsigned int);
  
  /** Get the upper and lower binary values in the input image.*/
  itkGetMacro(UpperBinaryValue, BinaryValueType);
  itkGetMacro(LowerBinaryValue, BinaryValueType);
  
protected:
  AntiAliasBinaryImageFilter();
  ~AntiAliasBinaryImageFilter() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Overridden from the parent class to indroduce a constraint on
   *  surface flow under certain conditions. */
  inline virtual ValueType CalculateUpdateValue(const IndexType &idx,
                                                const TimeStepType &dt,
                                                const ValueType &value,
                                                const ValueType &change);
  
  /** Overrides the default Halt() method to stop only when the RMS error is
   * less than a user-specified threshold. */
  bool Halt()
  {
    if (this->GetElapsedIterations() >= m_MaximumIterations)
      {
      itkWarningMacro("This filter has passed the maximum number of iterations allowed and will be halted.  This means that the solution did not converge to the MaximumRMSError you specified.  Try setting m_MaximumRMSError to a higher value, or set m_MaxmimumIterations to a higher value.");
      return true;
      }
    else if ( this->GetElapsedIterations() == 0)
      { return false; }
    else if ( this->GetRMSChange() <= m_MaximumRMSError )
      { return true; }
    else
      { return false; }
  }

  /** Overridden from ProcessObject to set certain values before starting the
    * finite difference solver and then create an appropriate output */
  void GenerateData();
  
private:
  ValueType m_MaximumRMSError;
  BinaryValueType m_UpperBinaryValue;
  BinaryValueType m_LowerBinaryValue;
  typename CurvatureFunctionType::Pointer m_CurvatureFunction;
  
  unsigned int m_MaximumIterations;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAntiAliasBinaryImageFilter.txx"
#endif

#endif
