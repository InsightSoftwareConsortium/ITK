/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterFileIOToImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterFileIOToImage_h
#define __itkFilterFileIOToImage_h

#include "itkFileIO.h"
#include "itkImageSource.h"
#include "itkImage.h"

namespace itk
{

template <class TOutputImage>
class ITK_EXPORT FilterFileIOToImage : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterFileIOToImage         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

	/**
	 * A useful constructor
	 */
	FilterFileIOToImage(std::string fileName);

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterFileIOToImage, ImageSource);

  /**
   * typedef for Size.
   */
  typedef Size<TOutputImage::ImageDimension>  Size;

  /**
   * typedef for Region.
   */
  typedef ImageRegion<TOutputImage::ImageDimension>  Region;

	/**
	 * Set m_IO
	 */
	void SetIO(FileIO* io);

protected:
  void GenerateData();
	FileIO* m_IO;
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterFileIOToImage.txx"
#endif

#endif // __itkFilterFileIOToImage_h
