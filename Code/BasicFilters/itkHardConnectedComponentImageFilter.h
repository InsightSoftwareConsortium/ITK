/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHardConnectedComponentImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHardConnectedComponentImageFilter_h
#define __itkHardConnectedComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{/**
 * The purpose of this program is to produce the connected components 
 * for any input binary image of dimensionality n.
 *
 * The program does a forward pass line by line through the entire image.
 * Each cell in the foreground is assigned the same label value as cells
 * in its neighborhood. If there is no label among the cells in its neighborhood,
 * a new label value is assigned to the cell. This means that this cell belongs
 * to a different connected component. We set up an equivalence table for each
 * label to indicate the equivalence of the labels stored in the table. After
 * the forward pass goes through the entire image, we merge the different
 * connected components corresponding to the equivalence labels in the table.
 * We implement this strategy in function GenerateData().

 * There are two options in the program. 
 * 1. Take an nD binary image as input, and produce an nD gray image, where intensity indicates label assigned to a connected component.
 * 2. Take an nD binary image and a set of seed points as input, and output an nD binary image containing the cells connected to the seeds.
For option 2, users need to assign the member variable std::list<IndexType> m_Seeds before calling function GenerateData().


 * \sa ImageToImageFilter
 * 
 * 
 *
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT HardConnectedComponentImageFilter : 
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef HardConnectedComponentImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;
  typedef   std::list<IndexType>                  ListType;
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(HardConnectedComponentImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Setting the seed points for specified object. */
  void SetObjectSeed( const IndexType &seed)
  {m_Seeds.push_front(seed);} 

protected:
  HardConnectedComponentImageFilter() {}
  virtual ~HardConnectedComponentImageFilter() {}
  HardConnectedComponentImageFilter(const Self&) {}

  /**
   * Standard pipeline method. 
   */
  void GenerateData();
  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent); }

  
private:
  ListType    m_Seeds;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHardConnectedComponentImageFilter.txx"
#endif

#endif
