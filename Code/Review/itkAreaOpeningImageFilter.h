/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAreaOpeningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright ( c ) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAreaOpeningImageFilter_h
#define __itkAreaOpeningImageFilter_h

#include "itkAttributeMorphologyBaseImageFilter.h"

namespace itk
{

/**
 * \class AreaOpeningImageFilter
 * \brief Morphological opening by attributes
 *
 * This is the base class for morphology attribute
 * operations. Attribute openings remove blobs according to criteria
 * such as area. When applied to grayscale images it has the effect of
 * trimming peaks based on area while leaving the rest of the image
 * unchanged. It is possible to use attributes besides area, but no
 * others are implemented yet. This filter uses some dodgy coding
 * practices - most notably copying the image data to a linear buffer
 * to allow direct implementation of the published algorithm. It
 * should therefore be quite a good candidate to carry out tests of
 * itk iterator performance with randomish access patterns. 
 *
 * This filter is implemented using the method of Wilkinson, "A
 * comparison of algorithms for Connected set openings and Closings",
 * A. Meijster and M. H. Wilkinson, PAMI, vol 24, no. 4, April 2002.
 * Attempts at implementing the method from ISMM 2000 are also
 * included, but operation appears incorrect. Check the ifdefs if you
 * are interested.
 *
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AreaOpeningImageFilter :
    public AttributeMorphologyBaseImageFilter<TInputImage, TOutputImage, unsigned long, std::greater<typename TInputImage::PixelType> >

{
public:
  typedef AreaOpeningImageFilter Self;
  typedef AttributeMorphologyBaseImageFilter<TInputImage, TOutputImage, unsigned long, std::greater<typename TInputImage::PixelType> >
                                 Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;
  typedef typename TInputImage::IndexType          IndexType;
  typedef typename TInputImage::OffsetType         OffsetType;
  typedef typename TInputImage::SizeType           SizeType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AreaOpeningImageFilter, 
               AttributeMorphologyBaseImageFilter);

protected:
  AreaOpeningImageFilter(){}
  virtual ~AreaOpeningImageFilter() {}

private:

  AreaOpeningImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // namespace itk
#endif
