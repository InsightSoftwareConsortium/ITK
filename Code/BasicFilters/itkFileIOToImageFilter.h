/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIOToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFileIOToImageFilter_h
#define __itkFileIOToImageFilter_h

#include "itkImageIO.h"
#include "itkImageSource.h"
#include "itkImage.h"

namespace itk
{

template <class TOutputImage>
class ITK_EXPORT FileIOToImageFilter : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FileIOToImageFilter         Self;

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
  FileIOToImageFilter(std::string fileName);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FileIOToImageFilter, ImageSource);

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
  FileIOToImageFilter();
	~FileIOToImageFilter();
	void LoadFile();

  ImageIO* m_IO;
	LightObject::Pointer m_LightObjectIO;
	std::string m_FileToLoad;
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFileIOToImageFilter.txx"
#endif

#endif // __itkFileIOToImageFilter_h
