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
///highest-level base class for templated image classes
/**
 * itkImageBase is the base class for the templated itkImage base classes.
 */

#ifndef __itkImageBase_h
#define __itkImageBase_h

#include "itkDataObject.h"

class ITK_EXPORT itkImageBase : public itkDataObject
{
public:
  /** Smart pointer typedef support */
  typedef itkSmartPointer<itkImageBase> Pointer;

  /** Create an empty image. */
  static itkImageBase::Pointer New();

  /** Restore object to initialized state */
  void Initialize();

  /** Overload itkDataObject method.*/
  virtual void SetDimension(int dim);

  /** Set the dimensions of the image. This method assumes that
   *  the dimension of the data has been set. */
  void SetDimensions(int *dims);

  /** Set the dimensions of the image. This method assumes that
   *  the dimension of the data has been set. */
  void SetSpacing(float *spacing);

  /** Set the dimensions of the image. This method assumes that
   *  the dimension of the data has been set. */
  void SetOrigin(float *origin);

protected:
  itkImageBase();
  ~itkImageBase();
  itkImageBase(const itkImageBase&) {};
  void operator=(const itkImageBase&) {};

private:
  int   *m_Dimensions;
  float *m_Spacing;
  float *m_Origin;
  
};

#endif

