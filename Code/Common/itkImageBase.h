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
 * itkImageBase is the base class for the templated itkImage base classes.
 */

#ifndef __itkImageBase_h
#define __itkImageBase_h

#include "itkDataObject.h"

class ITK_EXPORT itkImageBase : public itkDataObject
{
public:
  /** 
   * Smart pointer typedef support. 
   */
  typedef itkSmartPointer<itkImageBase> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkImageBase, itkDataObject);

  /** 
   * Create an empty image. 
   */
  static Pointer New();

  /** 
   * Restore object to initialized state.
   */
  void Initialize();

  /** 
   * Overload itkDataObject method.
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
  itkImageBase();
  ~itkImageBase();
  itkImageBase(const itkImageBase&) {};
  void operator=(const itkImageBase&) {};
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

private:
  unsigned long *m_Size;
  float         *m_Spacing;
  float         *m_Origin;
  
};

#endif

