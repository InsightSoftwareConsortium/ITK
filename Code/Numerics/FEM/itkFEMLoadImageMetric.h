/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFEMLoadImageMetric.h Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkFEMLoadImageMetric_h_
#define _itkFEMLoadImageMetric_h_


#include <itkMutualInformationImageToImageMetric.h>
#include <itkMeanSquaresImageToImageMetric.h>
#include <itkNormalizedCorrelationImageToImageMetric.h>
#include <itkPatternIntensityImageToImageMetric.h>

#include <FEM/itkFEMLoadImagePairBase.h>



namespace itk 
{
namespace fem
{

/**
 * \class LoadImageMetric
 * \brief General image pair load that uses the itkImageToImageMetrics.
 *
 * LoadImageMetric computes FEM gravity loads by using derivatives provided 
 * by itkImageToImageMetrics (e.g. mean squares intensity difference.)
 * The function responsible for this is called Fg, as required by the FEMLoad
 * standards.  It takes a vnl_vector as input.
 * We assume the vector input is of size 2*ImageDimension.
 * The 0 to ImageDimension-1 elements contain the position, p,
 * in the reference (moving) image.  The next ImageDimension to 2*ImageDimension-1
 * elements contain the value of the vector field at that point, v(p).
 *
 * Then, we evaluate the derivative at the point p+v(p) with respect to
 * some region of the target (fixed) image by calling the metric with 
 * the translation parameters as provided by the vector field at p.
 * The metrics return both a scalar similarity value and vector-valued derivative.  
 * The derivative is what gives us the force to drive the FEM registration.
 * These values are computed with respect to some region in the Target image.
 * This region size may be set by the user by calling SetTargetRadius.
 * As the metric derivative computation evolves, performance should improve
 * and more functionality will be available (such as scale selection).
 */ 
template<class TReference,class TTarget> 
class LoadImageMetric : public LoadImagePairBase<TReference,TTarget>
{
typedef LoadImagePairBase<TReference,TTarget> TemplatedParentClass;
FEM_CLASS_SP(LoadImageMetric,TemplatedParentClass)
public:
//  typedef LoadImageMetric             Self;                   
//  typedef LoadImagePairBase     Superclass;         
//  typedef SmartPointer<Self>       Pointer;   
//  typedef SmartPointer<const Self> ConstPointer;  
//  itkTypeMacro(LoadImageMetric,LoadImagePairBase);
  //itkNewMacro(Self);  // need light object

//------------------------------------------------------------
// Set up the metrics
//------------------------------------------------------------
  typedef double                   CoordinateRepresentationType;
  typedef Transform< CoordinateRepresentationType,ImageDimension, ImageDimension > TransformBaseType;
  typedef TranslationTransform<CoordinateRepresentationType,  ImageDimension >  DefaultTransformType;

 /**  Type of supported metrics. */
  typedef   ImageToImageMetric<TargetType,ReferenceType > MetricBaseType;
  typedef typename MetricBaseType::Pointer             MetricBaseTypePointer;

  typedef   MutualInformationImageToImageMetric<  TargetType, ReferenceType  > MutualInformationMetricType;

  typedef   MeanSquaresImageToImageMetric< TargetType, ReferenceType  > MeanSquaresMetricType;

  typedef   NormalizedCorrelationImageToImageMetric< TargetType,  ReferenceType  > NormalizedCorrelationMetricType;

  typedef   PatternIntensityImageToImageMetric<  TargetType, ReferenceType  > PatternIntensityMetricType;

//  typedef typename MutualInformationMetricType             DefaultMetricType;
//  typedef typename NormalizedCorrelationMetricType             DefaultMetricType;
//  typedef typename PatternIntensityMetricType             DefaultMetricType;
  typedef typename MeanSquaresMetricType             DefaultMetricType;
  typedef typename DefaultTransformType::ParametersType         ParametersType;
  typedef typename DefaultTransformType::JacobianType           JacobianType;


//------------------------------------------------------------
// Set up an Interpolator
//------------------------------------------------------------
  typedef LinearInterpolateImageFunction< ReferenceType, double > InterpolatorType;


// FUNCTIONS
  
  /** Set/Get the Metric. FIXME */
//  void SetMetric(MetricBaseTypePointer MP) { m_metric=MP; };   
 
  void InitializeMetric();
  /** Implements the LoadGrav Fg function using the selected image metric. */
  VectorType Fg(VectorType);

  void SetSolution(Solution::Pointer ptr) {  m_Solution2=ptr; }
  void SetSolution(Solution::ConstPointer ptr) {  m_Solution=ptr; }
  Solution::ConstPointer GetSolution() {  return m_Solution; }

  /**
   *  This method returns the total metric evaluated over the image with respect to the current solution.
   */
  Float EvaluateMetricGivenSolution( Element::ArrayType* el, Float step);
  Float GetMetric (VectorType  InVec);
  
  // FIXME - WE ASSUME THE 2ND VECTOR (INDEX 1) HAS THE INFORMATION WE WANT
  Float GetSolution(unsigned int i,unsigned int which){  return m_Solution2->GetSolutionValue(i,which); }
  
  LoadImageMetric(); // cannot be private until we always use smart pointers
  
  virtual Baseclass::Pointer Clone() const 
      { return new Self(*this); }  //FIXME?
 
protected:

private:
  Solution::Pointer   m_Solution2;
  Solution::ConstPointer   m_Solution;
  typename MetricBaseTypePointer                         m_Metric;
  typename TransformBaseType::Pointer                 m_Transform;
  typename InterpolatorType::Pointer               m_Interpolator;

public:
//  LoadImageMetric(const Self&);  FIXME NEED COPY CONSTRUCTOR   //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};



}} // end namespace fem/itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMLoadImageMetric.txx"
#endif

#endif
