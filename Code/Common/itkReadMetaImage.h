/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReadMetaImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkReadMetaImage_h
#define __itkReadMetaImage_h

#include "itkImageSource.h"

namespace itk
{

/** \class ReadMetaImage
 * \brief Read image files in MetaImage format.
 *
 * ReadMetaImage reads data files in MetaImage format. This format
 * was developed at UNC by Stephen Aylward ( aylward@cs.unc.edu ).
 * It is intended to provide an intermediary representation for 
 * DICOM streams.
 *
 */
template <class TOutputImage>
class ITK_EXPORT ReadMetaImage : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ReadMetaImage        Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ReadMetaImage,ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * typedef for Size.
   */
  typedef Size<TOutputImage::ImageDimension>  Size;

  /**
   * typedef for Region.
   */
  typedef ImageRegion<TOutputImage::ImageDimension>  Region;

  /** 
   * Specify the name of the input file.
   */
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the input file.
   */
  itkGetStringMacro(FileName);
  
protected:
  ReadMetaImage();
  ~ReadMetaImage() {};
  ReadMetaImage(const Self&) {}; 
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
  void GenerateData();

private:
  std::string m_FileName;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReadMetaImage.txx"
#endif

#endif
