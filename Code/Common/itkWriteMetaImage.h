/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteMetaImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkWriteMetaImage_h
#define __itkWriteMetaImage_h

#include "itkWriteImage.h"
#include <vector>

namespace itk
{

/** \class WriteMetaImage
 * \brief Write an image (dimension 1-3D) in Meta Image format.
 *
 * WriteMetaImage writes N-D images in Meta Image file format. 
 */
template <class TInputImage>
class ITK_EXPORT WriteMetaImage : public WriteImage<TInputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WriteMetaImage       Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(WriteMetaImage,ImageWriter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Specify the name of the input file.
   */
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the input file.
   */
  itkGetStringMacro(FileName);

   /** 
   * Execute the filter (write the image)
   */
   void Execute(void);
 
   /** 
   * Write Data (abstract function from Writer)
   */
   void WriteData(void);
 

protected:
  WriteMetaImage();
  ~WriteMetaImage() {}
  WriteMetaImage(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

private:
  std::string m_FileName;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWriteMetaImage.txx"
#endif

#endif
