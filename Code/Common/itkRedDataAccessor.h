/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkRedDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRedDataAccessor_h
#define __itkRedDataAccessor_h


#include "itkRGBPixel.h"


namespace itk
{

/**
 * \class RedDataAccessor
 * \brief Give access to the red component of a RGBPixel type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Red component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT RedDataAccessor
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   RedDataAccessor        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef     RGBPixel<T>      InternalType;


  /** 
   * Write access to the Red component
   */
  static inline void Set( InternalType & output, const ExternalType & input ) 
    { output.SetRed( input ); }


  /** 
   * Read access to the Red component
   */
  static inline ExternalType Get( const InternalType & input ) 
    { return input.GetRed(); }


};

  
  
}  // end namespace itk


#endif

