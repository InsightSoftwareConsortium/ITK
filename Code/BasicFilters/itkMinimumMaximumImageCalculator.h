/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinimumMaximumImageCalculator_h
#define __itkMinimumMaximumImageCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** This calculator computes the minimum and the maximum intensity values of
 * an image.  It is templated over input image type.  If only Maximum or
 * Minimum value is needed, just call ComputeMaximum() (ComputeMinimum())
 * otherwise Compute() will compute both.
 *
 * \ingroup Operators
 */
template <class TInputImage>            
class ITK_EXPORT MinimumMaximumImageCalculator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef MinimumMaximumImageCalculator Self;
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

protected:
  MinimumMaximumImageCalculator();
  virtual ~MinimumMaximumImageCalculator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  MinimumMaximumImageCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  PixelType            m_Minimum;
  PixelType            m_Maximum;
  ImageConstPointer    m_Image;

  IndexType            m_IndexOfMinimum;
  IndexType            m_IndexOfMaximum;

  RegionType           m_Region;
  bool                 m_RegionSetByUser;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageCalculator.txx"
#endif

#endif /* __itkMinimumMaximumImageCalculator_h */
