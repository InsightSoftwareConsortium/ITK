/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumConditionalImageCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinimumMaximumConditionalImageCalculator_h
#define __itkMinimumMaximumConditionalImageCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** This calculator computes the minimum and the maximum intensity values of
 * pixels in the input image and that are set to a particular value in an image
 * mask provided as second input.  It is templated over input image type and
 * the mask image type.  If only Maximum or Minimum value is needed, just call
 * ComputeMaximum() (ComputeMinimum()) otherwise Compute() will compute both.
 *
 * \ingroup Operators
 */
template <class TInputImage, class TMaskImage>            
class ITK_EXPORT MinimumMaximumConditionalImageCalculator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef MinimumMaximumConditionalImageCalculator Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Object);

  /** Type definition for the input image. */
  typedef TInputImage  ImageType;

  /** Pointer type for the image. */
  typedef typename TInputImage::Pointer  ImagePointer;
  
  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer ImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType PixelType;
  
  /** Type definition for the input image index type. */
  typedef typename TInputImage::IndexType IndexType;
  
  /** Type definition for the input image region type. */
  typedef typename TInputImage::RegionType RegionType;
  
  /** Set the input image. */
  itkSetConstObjectMacro(Image,ImageType);

  /** Type definition for the input image. */
  typedef TMaskImage  MaskImageType;

  /** Pointer type for the image. */
  typedef typename TMaskImage::Pointer  MaskImagePointer;
  
  /** Const Pointer type for the image. */
  typedef typename TMaskImage::ConstPointer MaskImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TMaskImage::PixelType MaskPixelType;
  
  /** Type definition for the input image index type. */
  typedef typename TMaskImage::IndexType MaskIndexType;
  
  /** Type definition for the input image region type. */
  typedef typename TMaskImage::RegionType MaskRegionType;
  
  /** Set the input image. */
  itkSetConstObjectMacro(MaskImage,MaskImageType);


  /** Compute the minimum value of intensity of the input image. */
  void ComputeMinimum(void);

  /** Compute the maximum value of intensity of the input image. */
  void ComputeMaximum(void);

  /** Compute the minimum value of intensity of the input image. */
  void Compute(void);

  /** Return the minimum intensity value. */
  itkGetMacro(Minimum,PixelType);
  
  /** Return the maximum intensity value. */
  itkGetMacro(Maximum,PixelType);

  /** Return the index of the minimum intensity value. */
  itkGetConstReferenceMacro(IndexOfMinimum,IndexType);

  /** Return the index of the maximum intensity value. */
  itkGetConstReferenceMacro(IndexOfMaximum,IndexType);

  /** Set the region over which the values will be computed */
  void SetRegion( const RegionType & region );

  /** Set/Get the mask value for which the minimum and maximum are going to be
   * computed */
  itkGetConstReferenceMacro(MaskValue, MaskPixelType);
  itkSetMacro(MaskValue, MaskPixelType);

protected:
  MinimumMaximumConditionalImageCalculator();
  virtual ~MinimumMaximumConditionalImageCalculator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  MinimumMaximumConditionalImageCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  PixelType            m_Minimum;
  PixelType            m_Maximum;
  ImageConstPointer    m_Image;

  IndexType            m_IndexOfMinimum;
  IndexType            m_IndexOfMaximum;

  RegionType           m_Region;
  bool                 m_RegionSetByUser;

  MaskImageConstPointer m_MaskImage;
  MaskPixelType         m_MaskValue;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumConditionalImageCalculator.txx"
#endif

#endif /* __itkMinimumMaximumConditionalImageCalculator_h */
