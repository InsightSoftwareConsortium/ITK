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
#include "itkExceptionObject.h"

namespace itk
{

/**
 * \brief Base exception class for IO conflicts
 */
class FileIOException : public ExceptionObject 
{
public:
  itkTypeMacro( FileIOException, ExceptionObject );

  FileIOException() 
  {
    SetDescription("Problem during File IO");
  }
};


/**
 * \brief Filter to convert input data to a particular internal type
 *
 *  TOutputImage is the type expected by the external users of the 
 *  filter. Data comming from a file can be stored in any other format
 *  (or type) this filter converts data between the file type and the
 *  external expected type.
 *
 *  The Pluggable factory pattern is used. Different kinds of readers
 *  can be registered (even at run time) without having to modify the
 *  code in this class.
 *
 */
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
