/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageBase_h
#define __itkImageBase_h

#include "itkDataObject.h"

namespace itk
{

/** \class ImageBase
 * \brief Base class for templated image classes.
 *
 * ImageBase is the base class for the templated Image base classes.
 * The class maintains the size (the i-j-k dimensions of the image),
 * the image origin coordinates, and the spacing (pixel/voxel width,
 * height, etc.) The interface assumes that the image is of arbitrary
 * dimension.
 */

class ITK_EXPORT ImageBase : public DataObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageBase           Self;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageBase, DataObject);

  /** 
   * Restore object to initialized state.
   */
  void Initialize();

protected:
  ImageBase();
  ~ImageBase();
  ImageBase(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
};

} // end namespace itk

#endif

