/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVTKImageReader_h
#define __itkVTKImageReader_h

#include "itkImageSource.h"

namespace itk
{

/** \class VTKImageReader
 * \brief Read VTK-formatted image files.
 *
 * VTKImageReader reads VTK-formatted data files. This class requires
 * that the VTK dataset type is STRUCTURED_POINTS, and only the scalar
 * point data is read (1-4 components).
 */
template <class TOutputImage>
class ITK_EXPORT VTKImageReader : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VTKImageReader        Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VTKImageReader,ImageSource);

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
  
protected:
  VTKImageReader();
  ~VTKImageReader() {};
  VTKImageReader(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  void GenerateData();

private:
  std::string m_FileName;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageReader.txx"
#endif

#endif
