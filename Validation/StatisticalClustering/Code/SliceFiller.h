/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SliceFiller.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __SliceFiller_h
#define __SliceFiller_h

#include "itkImageToImageFilter.h"

template< class TImage >
class SliceFiller : public itk::ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard class typedefs. */
  typedef SliceFiller  Self;
  typedef itk::ImageToImageFilter< TImage, TImage >  Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SliceFiller, ImageToImageFilter);

  void SetStartingSliceNumber(int sliceNumber) ;

  void SetDesiredSize(typename TImage::SizeType size) ;

  void SetBackgroundPixelValue(typename TImage::PixelType value) ;
  
protected:
  SliceFiller() ;
  ~SliceFiller() ;

  void GenerateData() ;

private:
  typename TImage::PixelType m_BackgroundPixelValue ;
  typename TImage::SizeType m_DesiredSize ;
  int m_SliceSize ;
  int m_StartingSliceNumber ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "SliceFiller.txx"
#endif

#endif
