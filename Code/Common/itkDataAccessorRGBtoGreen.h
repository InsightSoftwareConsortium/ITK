/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDataAccessorRGBtoGreen.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDataAccessorRGBtoGreen_h
#define __itkDataAccessorRGBtoGreen_h


#include "itkRGB.h"


namespace itk
{

/**
 * \class DataAccessorRGBtoGreen
 * \brief Give access to the Green component of a RGB type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGB image appear as being
 * of scalar type T, showing only the Green component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT DataAccessorRGBtoGreen
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   DataAccessorRGBtoGreen        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef     RGB<T>   InternalType;


  /** 
   * Write access to the Green component
   */
  static inline void Set( InternalType & output, const ExternalType & input ) 
    { output.SetGreen( input ); }


  /** 
   * Read access to the Green component
   */
  static inline ExternalType Get( const InternalType & input ) 
    { return input.GetGreen(); }


};

  
} // end namespace itk

#endif

