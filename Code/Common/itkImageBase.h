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

ITK_NAMESPACE_BEGIN

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

  /** 
   * Overload DataObject method.
   */
  virtual void SetDimension(unsigned int dim);

  /** 
   * Set the size of the image. This method assumes that
   * the dimension of the data has been set. This method
   * allocates memory for the image (hence the virtual).
   */
  virtual void SetSize(unsigned long *size);

  /** 
   * Get the size of the image. 
   */
  const unsigned long *GetSize() const {return m_Size;} ;
  
  /** 
   * Set the spacing of the image. This method assumes that
   * the dimension of the data has been set. 
   */
  void SetSpacing(float *spacing);

  /** 
   * Get the size of the image. 
   */
  const float *GetSpacing() const {return m_Spacing;} ;
  
  /** 
   * Set the origin of the image. This method assumes that
   * the dimension of the data has been set. 
   */
  void SetOrigin(float *origin);

  /** 
   * Get the origin of the image. 
   */
  const float *GetOrigin() const {return m_Origin;} ;
  
protected:
  ImageBase();
  ~ImageBase();
  ImageBase(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  unsigned long *m_Size;
  float         *m_Spacing;
  float         *m_Origin;
  
};

ITK_NAMESPACE_END

#endif

