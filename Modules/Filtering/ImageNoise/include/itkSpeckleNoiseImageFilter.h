/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSpeckleNoiseImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2009-02-24 19:03:15 $
  Version:   $Revision: 1.4 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSpeckleNoiseImageFilter_h
#define __itkSpeckleNoiseImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
  
/** \class SpeckleNoiseImageFilter
 *
 * \brief Alter an image with speckle (multiplicative) noise.
 *
 * The speckle noise follows a Gamma distribution of mean 1 and standard deviation
 * provided by the user. The noise is proportional to the pixel intensity.
 *
 * \author Gaetan Lehmann
 *
 * \ingroup IntensityImageFilters  Multithreaded
 * \sa InPlaceImageFilter
 */
template <class TInputImage, class TOutputImage=TInputImage>
class ITK_EXPORT SpeckleNoiseImageFilter :
      public
InPlaceImageFilter<TInputImage,TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef SpeckleNoiseImageFilter                               Self;
  typedef InPlaceImageFilter<
    TInputImage,TOutputImage   >                             Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SpeckleNoiseImageFilter, InPlaceImageFilter);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageType       OutputImageType;
  typedef typename Superclass::OutputImagePointer    OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                             InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  
  itkGetConstMacro(StandardDeviation, double);
  itkSetMacro(StandardDeviation, double);
  

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                   (Concept::Convertible<typename TInputImage::PixelType,
                    typename TOutputImage::PixelType>));
  // The following concept check doesn't seem to work with vector immages
  //itkConceptMacro(Input1Input2OutputDivisionOperatorsCheck,
  //                (Concept::DivisionOperators<typename TInputImage::PixelType,
  //                 double,
  //                 typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  SpeckleNoiseImageFilter();
  virtual ~SpeckleNoiseImageFilter() {};
   
  void PrintSelf(std::ostream &os, Indent indent) const;
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  SpeckleNoiseImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  double m_StandardDeviation;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpeckleNoiseImageFilter.txx"
#endif

#endif
