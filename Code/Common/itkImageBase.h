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
/**
 * ImageBase is the base class for the templated Image base classes.
 */

#ifndef __itkImageBase_h
#define __itkImageBase_h

#include "itkDataObject.h"

namespace itk
{

class ITK_EXPORT ImageBase : public DataObject
{
public:
  /** 
   * Smart pointer typedef support. 
   */
  typedef ImageBase           Self;
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageBase, DataObject);

  /** 
   * Create an empty image. 
   */
  static Pointer New();

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
   * the dimension of the data has been set. 
   */
  void SetSize(unsigned long *size);

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

} // namespace itk

#endif

