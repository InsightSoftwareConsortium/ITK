/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHessianToObjectnessMeasureImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHessianToObjectnessMeasureImageFilter_h
#define __itkHessianToObjectnessMeasureImageFilter_h

#include "itkSymmetricSecondRankTensor.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"

namespace itk
{
/** \class HessianObjectnessMeasureImageFilter
 * \brief A filter to enhance M-dimensional objects in N-dimensional images 
 * 
 * The objectness measure is a generalization of Frangi's vesselness measure,
 * which is based on the analysis of the the Hessian eigen system. The filter
 * can enhance blob-like structures (M=0), vessel-like structures (M=1), 2D 
 * plate-like structures (M=2), hyper-plate-like structures (M=3) in N-dimensional 
 * images, with M<N.  
 * The filter takes an image of a Hessian pixels ( SymmetricSecondRankTensor pixels
 * pixels ) and produces an enhanced image. The Hessian input image can be produced 
 * using itkHessianSmoothedRecursiveGaussianImageFilter. 
 *  
 *
 * \par References
 * Frangi, AF, Niessen, WJ, Vincken, KL, & Viergever, MA (1998). Multiscale Vessel 
 * Enhancement Filtering. In Wells, WM, Colchester, A, & Delp, S, Editors, MICCAI '98 
 * Medical Image Computing and Computer-Assisted Intervention, Lecture Notes in Computer 
 * Science, pages 130-137, Springer Verlag, 1998.
 * 
 * \author Luca Antiga Ph.D.  Medical Imaging Unit,
 *                            Bioengineering Deparment, Mario Negri Institute, Italy.
 * 
 * \sa MultiScaleHessianBasedMeasureImageFilter 
 * \sa Hessian3DToVesselnessMeasureImageFilter
 * \sa HessianSmoothedRecursiveGaussianImageFilter 
 * \sa SymmetricEigenAnalysisImageFilter
 * \sa SymmetricSecondRankTensor
 * 
 * \ingroup IntensityImageFilters TensorObjects
 *
 */
  
template < typename TInputImage, typename TOutputImage > 
class ITK_EXPORT HessianToObjectnessMeasureImageFilter : public
ImageToImageFilter< TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef HessianToObjectnessMeasureImageFilter Self;

  typedef ImageToImageFilter< TInputImage, TOutputImage >  Superclass;

  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;
  
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename InputImageType::PixelType    InputPixelType;
  typedef typename OutputImageType::PixelType   OutputPixelType;
  
  /** Image dimension */
  itkStaticConstMacro(ImageDimension, unsigned int, ::itk::GetImageDimension<InputImageType>::ImageDimension);

  typedef double                                                    EigenValueType;
  typedef itk::FixedArray< EigenValueType, itkGetStaticConstMacro( ImageDimension ) > EigenValueArrayType;
  typedef itk::Image< EigenValueArrayType, itkGetStaticConstMacro( ImageDimension ) > EigenValueImageType;

  typedef SymmetricEigenAnalysisImageFilter< 
    InputImageType, EigenValueImageType >                           EigenAnalysisFilterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get Alpha, the weight corresponding to R_A 
   * (the ratio of the smallest eigenvalue that has to be large to the larger ones). 
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Alpha,double);
  itkGetMacro(Alpha,double);

  /** Set/Get Beta, the weight corresponding to R_B 
   * (the ratio of the largest eigenvalue that has to be small to the larger ones). 
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Beta,double);
  itkGetMacro(Beta,double);

  /** Set/Get Gamma, the weight corresponding to S 
   * (the Frobenius norm of the Hessian matrix, or second-order structureness) */
  itkSetMacro(Gamma,double);
  itkGetMacro(Gamma,double);

  /** Toggle scaling the objectness measure with the magnitude of the largest absolute eigenvalue */ 
  itkSetMacro(ScaleObjectnessMeasure,bool);
  itkGetMacro(ScaleObjectnessMeasure,bool);
  itkBooleanMacro(ScaleObjectnessMeasure);

  /** Set/Get the dimensionality of the object (0: points (blobs), 
   * 1: lines (vessels), 2: planes (plate-like structures), 3: hyper-planes.
   * ObjectDimension must be smaller than ImageDimension. */
  itkSetMacro(ObjectDimension,int);
  itkGetMacro(ObjectDimension,int);

  /** Enhance bright structures on a dark background if true, the opposite if false. */
  itkSetMacro(BrightObject,bool);
  itkGetMacro(BrightObject,bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(DoubleConvertibleToOutputCheck,(Concept::Convertible<double, OutputPixelType>));
  /** End concept checking */
#endif
  
protected:
  HessianToObjectnessMeasureImageFilter();
  ~HessianToObjectnessMeasureImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate Data */
  void GenerateData(void);

private:
  HessianToObjectnessMeasureImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename EigenAnalysisFilterType::Pointer m_SymmetricEigenValueFilter;

  double                 m_Alpha;
  double                 m_Beta;
  double                 m_Gamma;
  int                    m_ObjectDimension;
  bool                   m_BrightObject;
  bool                   m_ScaleObjectnessMeasure;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHessianToObjectnessMeasureImageFilter.txx"
#endif
  
#endif
