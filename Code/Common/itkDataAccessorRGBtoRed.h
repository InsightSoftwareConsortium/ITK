/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDataAccessorRGBtoRed.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDataAccessorRGBtoRed_h
#define __itkDataAccessorRGBtoRed_h


#include "itkRGB.h"


namespace itk
{

/**
 * \class DataAccessorRGBtoRed
 * \brief Give access to the red component of a RGB type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGB image appear as being
 * of scalar type T, showing only the Red component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT DataAccessorRGBtoRed
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   DataAccessorRGBtoRed        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef     RGB<T>      InternalType;


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

