/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBayesianClassifierImageFilter_h
#define __itkBayesianClassifierImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkMaximumRatioDecisionRule.h"
#include "itkGaussianDensityFunction.h"

namespace itk
{
  
/** \class BayesianClassifierImageFilter
 *
 * \brief This filter will perform Bayesian Classification on an image.
 *
 * [add detailed documentation]
 * 
 * This filter is templated over the input image type
 * and the output image type.
 * 
 * The filter expect both images to have the same number of dimensions.
 *
 * \author John Melonakos, Georgia Tech
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT BayesianClassifierImageFilter :
    public
ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef BayesianClassifierImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BayesianClassifierImageFilter, ImageToImageFilter);

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;
  
  /** Input and Output image types */
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;

  /** Type of the Measurement */
  typedef InputPixelType   MeasurementVectorType;

  /** Type of the Gaussian density functions */
  typedef itk::Statistics::GaussianDensityFunction< MeasurementVectorType > 
                                                              MembershipFunctionType;
  typedef typename MembershipFunctionType::ConstPointer       MembershipFunctionConstPointer;

  /** Membership function container */
  typedef std::vector< MembershipFunctionConstPointer >       MembershipFunctionContainer;

  /** Decision rule to use for defining the label */
  typedef itk::MaximumRatioDecisionRule                       DecisionRuleType;

  
  /** Add a membership function to the filter. This is expected to be a Gaussian */
  void AddMembershipFunction( const MembershipFunctionType * newFunction );


protected:
  BayesianClassifierImageFilter();
  virtual ~BayesianClassifierImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Here is where the classification is computed.*/
  virtual void GenerateData();

private:
  BayesianClassifierImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  MembershipFunctionContainer     m_MembershipFunctions;  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBayesianClassifierImageFilter.txx"
#endif

#endif
