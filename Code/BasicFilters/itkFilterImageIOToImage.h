/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageIOToImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageIOToImage_h
#define __itkFilterImageIOToImage_h

#include "itkImageIO.h"
#include "itkImageSource.h"
#include "itkImage.h"

namespace itk
{

template <class TOutputImage>
class ITK_EXPORT FilterImageIOToImage : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageIOToImage         Self;

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
  FilterImageIOToImage(std::string fileName);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterImageIOToImage, ImageSource);

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
  void SetIO(ImageIO* io);

	/**
	 * Get m_IO
	 */
	ImageIO* GetIO();

	/**
	 * Set m_FileToLoad
	 */
	itkSetStringMacro(FileToLoad);

protected:
  void GenerateData();
  FilterImageIOToImage();
	~FilterImageIOToImage();
	void LoadFile();

  ImageIO* m_IO;
	LightObject::Pointer m_LightObjectIO;
	std::string m_FileToLoad;
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageIOToImage.txx"
#endif

#endif // __itkFilterImageIOToImage_h
