/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleConnectedClosingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConnectedClosingImageFilter_h
#define __itkConnectedClosingImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class GrayscaleConnectedClosingImageFilter
 * \brief Enhance pixels associated with a dark object (identified by
 * a seed pixel) where the dark object is surrounded by a brigher object.
 *
 * GrayscaleConnectedClosingImagefilter is useful for enhancing dark
 * objects that are surrounded by bright borders. This filter makes it
 * easier to threshold the image and extract just the object of
 * interest. 
 *
 * Geodesic morphology and the connected closing algorithm are
 * described in Chapter 6 of Pierre Soille's book "Morphological Image
 * Analysis: Principles and Applications", Second Edition, Springer,
 * 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT GrayscaleConnectedClosingImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleConnectedClosingImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageRegionType::IndexType InputImageIndexType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleConnectedClosingImageFilter, 
               ImageToImageFilter);

  /** Set/Get the seed pixel for the segmentation */
  itkSetMacro(Seed, InputImageIndexType);
  itkGetMacro(Seed, InputImageIndexType);
  
  /** \deprecated
   * Get the number of iterations used to produce the current
   * output. This method is scheduled for removal since the
   * implementation now uses a noniterative solution. */
  unsigned long GetNumberOfIterationsUsed()
    { itkLegacyBody(itk::GrayscaleConnectedClosingImageFilter::GetNumberOfIterationsUsed, 2.2);
      return m_NumberOfIterationsUsed; };

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  /** End concept checking */
#endif

protected:
  GrayscaleConnectedClosingImageFilter();
  ~GrayscaleConnectedClosingImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GrayscaleConnectedClosingImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** GrayscaleConnectedClosingImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicDilateImageFilter. */
  void GenerateData();
  

private:
  GrayscaleConnectedClosingImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned long m_NumberOfIterationsUsed;
  InputImageIndexType m_Seed;
  
  bool                m_FullyConnected;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleConnectedClosingImageFilter.txx"
#endif

#endif


