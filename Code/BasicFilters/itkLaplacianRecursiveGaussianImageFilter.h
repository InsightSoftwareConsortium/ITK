/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianRecursiveGaussianImageFilter_h
#define __itkLaplacianRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkCommand.h"


namespace itk
{

/** \class LaplacianRecursiveGaussianImageFilter
 * \brief Computes the Laplacian of an image by convolution with the second derivative of a Gaussian.
 * 
 * This filter is implemented using the recursive gaussian filters
 *
 * 
 * \ingroup GradientFilters   
 * \ingroup Singlethreaded
 */
// NOTE that the ITK_TYPENAME macro has to be used here in lieu 
// of "typename" because VC++ doesn't like the typename keyword 
// on the defaults of template parameters
template <typename TInputImage, 
          typename TOutputImage= TInputImage >
class ITK_EXPORT LaplacianRecursiveGaussianImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LaplacianRecursiveGaussianImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  
  /** Pixel Type of the input image */
  typedef TInputImage                                    InputImageType;
  typedef typename InputImageType::PixelType             PixelType;


  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef typename NumericTraits<PixelType>::RealType      RealType;

  /** Define the image type for internal computations 
      RealType is usually 'double' in NumericTraits. 
      Here we prefer float in order to save memory.  */

  typedef float                                            InternalRealType;
  typedef Image<InternalRealType, 
                itkGetStaticConstMacro(ImageDimension) >   RealImageType;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    GaussianFilterType;

  /**  Derivative filter type, it will be the first in the pipeline  */
  typedef RecursiveGaussianImageFilter<
    InputImageType,
    RealImageType
    >    DerivativeFilterType;

  /**  Pointer to a gaussian filter.  */
  typedef typename GaussianFilterType::Pointer     GaussianFilterPointer;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterType::Pointer   DerivativeFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer           OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage      OutputImageType;
  typedef typename          OutputImageType::PixelType      OutputPixelType;

  /**  Auxiliary image for holding the values of the squared gradient components */
  typedef Image< InternalRealType, 
                 itkGetStaticConstMacro(ImageDimension) >      CumulativeImageType;
  typedef typename CumulativeImageType::Pointer                CumulativeImagePointer;

  /**  Command for observing progress of internal pipeline filters */
  typedef          MemberCommand< Self >     CommandType;  
  typedef typename CommandType::Pointer      CommandPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing. */
  void SetSigma( RealType sigma );

  /** Define which normalization factor will be used for the Gaussian */
  void SetNormalizeAcrossScale( bool normalizeInScaleSpace );
  itkGetMacro( NormalizeAcrossScale, bool );

  /** LaplacianRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, LaplacianRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  LaplacianRecursiveGaussianImageFilter();
  virtual ~LaplacianRecursiveGaussianImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate Data */
  void GenerateData( void );

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output);

  /** Compute progress by weighting the contributions of the internal filters */
  void ReportProgress(const Object * object, const EventObject & event );

private:
  LaplacianRecursiveGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  GaussianFilterPointer         m_SmoothingFilters[ImageDimension-1];
  DerivativeFilterPointer       m_DerivativeFilter;

  CumulativeImagePointer        m_CumulativeImage;

  CommandPointer                m_ProgressCommand;
  float                         m_Progress;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale; 


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianRecursiveGaussianImageFilter.txx"
#endif

#endif




