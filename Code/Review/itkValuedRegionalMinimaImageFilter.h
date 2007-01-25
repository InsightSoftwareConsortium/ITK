/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkValuedRegionalMinimaImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkValuedRegionalMinimaImageFilter_h
#define __itkValuedRegionalMinimaImageFilter_h

#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkNumericTraits.h"
#include "itkConceptChecking.h"

namespace itk {
/** \class ValuedRegionalMinimaImageFilter
 * \brief Transforms the image so that any pixel that is not a
 * regional minima is set to the maximum value for the pixel
 * type. Pixels that are regional minima retain their value.
 *
 * Regional minima are flat zones surrounded by pixels of higher
 * value. A completely flat image will be marked as a regional minima
 * by this filter.

 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMaximaImageFilter, ValuedRegionalExtremaImageFilter,
 * \sa HMinimaImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT ValuedRegionalMinimaImageFilter :
    public
    ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::less<typename TInputImage::PixelType>,
             std::less<typename TOutputImage::PixelType>
    >
{
public:
  typedef ValuedRegionalMinimaImageFilter Self;

  typedef ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::less<typename TInputImage::PixelType>,
             std::less<typename TOutputImage::PixelType>  > Superclass;

  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  typedef TInputImage                                       InputImageType;
  typedef typename InputImageType::PixelType                InputImagePixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputPixelTypeComparable,
    (Concept::LessThanComparable<InputImagePixelType>));
  itkConceptMacro(InputHasPixelTraitsCheck,
    (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<InputImagePixelType>));
  /** End concept checking */
#endif

protected:
  ValuedRegionalMinimaImageFilter() 
    {
    SetMarkerValue(NumericTraits<ITK_TYPENAME TOutputImage::PixelType>::max());
    }
  virtual ~ValuedRegionalMinimaImageFilter() {}

private:
  ValuedRegionalMinimaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end ValuedRegionalMinimaImageFilter

} //end namespace itk
#endif
