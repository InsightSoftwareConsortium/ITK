/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHessian3DToVesselnessMeasureImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHessian3DToVesselnessMeasureImageFilter_h
#define __itkHessian3DToVesselnessMeasureImageFilter_h

#include "itkSymmetricSecondRankTensor.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"

namespace itk
{
/** \class Hessian3DToVesselnessMeasureImageFilter
 * \brief 
 * Line filter to provide a vesselness measure for tubular objects from the
 * hessian matrix. The filter takes as input an image of hessian pixels
 * (SymmetricSecondRankTensor pixels) and preserves pixels that have
 * eigen values \f[\lambda_3\f] close to 0 and \f[\lambda_2\f] and \f[\lambda_1\f] as
 * large negative values. (for bright tubular structures).
 * 
 * \f[ \lambda_1 < \lambda_2 < \lambda_3 \f]
 *  
 *
 * \par References: 
 * "3D Multi-scale line filter for segmentation and visualization of 
 * curvilinear structures in medical images", 
 * Yoshinobu Sato, Shin Nakajima, Hideki Atsumi, Thomas Koller,
 * Guido Gerig, Shigeyuki Yoshida, Ron Kikinis.
 *
 * http://splweb.bwh.harvard.edu:8000/pages/papers/yoshi/cr.html
 *
 * 
 * \sa HessianRecursiveGaussianImageFilter 
 * \sa SymmetricEigenAnalysisImageFilter
 * \sa SymmetricSecondRankTensor
 * 
 * \ingroup IntensityImageFilters TensorObjects
 *
 */
  
template < typename  TPixel >
class ITK_EXPORT Hessian3DToVesselnessMeasureImageFilter : public
ImageToImageFilter< Image< SymmetricSecondRankTensor< double, 3 >, 3 >, 
                                                  Image< TPixel, 3 > >
{
public:
  /** Standard class typedefs. */
  typedef Hessian3DToVesselnessMeasureImageFilter Self;
  typedef ImageToImageFilter< 
          Image< SymmetricSecondRankTensor< double, 3 >, 3 >, 
          Image< TPixel, 3 > >                 Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  typedef typename Superclass::InputImageType            InputImageType;
  typedef typename Superclass::OutputImageType           OutputImageType;
  typedef typename InputImageType::PixelType             InputPixelType;
  typedef TPixel                                         OutputPixelType;
  
  /** Image dimension = 3. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      ::itk::GetImageDimension<InputImageType>::ImageDimension);
  itkStaticConstMacro(InputPixelDimension, unsigned int,
                      InputPixelType::ImageDimension);

  typedef  FixedArray< double, itkGetStaticConstMacro(InputPixelDimension) >
                                                          EigenValueArrayType;
  typedef  Image< EigenValueArrayType, itkGetStaticConstMacro(ImageDimension) >
                                                          EigenValueImageType;
  typedef   SymmetricEigenAnalysisImageFilter< 
              InputImageType, EigenValueImageType >     EigenAnalysisFilterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Set/Get macros for alpha_1 */
  itkSetMacro(Alpha1, double);
  itkGetMacro(Alpha1, double);
  
  /** Set/Get macros for alpha_1 */
  itkSetMacro(Alpha2, double);
  itkGetMacro(Alpha2, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(DoubleConvertibleToOutputCheck,
                  (Concept::Convertible<OutputPixelType>));
  /** End concept checking */
#endif

protected:
  Hessian3DToVesselnessMeasureImageFilter();
  ~Hessian3DToVesselnessMeasureImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate Data */
  void GenerateData( void );

private:
  Hessian3DToVesselnessMeasureImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename EigenAnalysisFilterType::Pointer         m_SymmetricEigenValueFilter;
  double m_Alpha1;
  double m_Alpha2;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHessian3DToVesselnessMeasureImageFilter.txx"
#endif
  
#endif
