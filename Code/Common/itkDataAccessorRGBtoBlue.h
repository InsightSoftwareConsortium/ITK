/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDataAccessorRGBtoBlue.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDataAccessorRGBtoBlue_h
#define __itkDataAccessorRGBtoBlue_h


#include "itkRGB.h"


namespace itk
{

/**
 * \class DataAccessorRGBtoBlue
 * \brief Give access to the Blue component of a RGB type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGB image appear as being
 * of scalar type T, showing only the Blue component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT DataAccessorRGBtoBlue
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   DataAccessorRGBtoBlue        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef   RGB<T>    InternalType;


  /** 
   * Write access to the Blue component
   */
  static inline void Set( InternalType & output, const ExternalType & input ) 
    { output.SetBlue( input ); }


  /** 
   * Read access to the Blue component
   */
  static inline ExternalType Get( const InternalType & input ) 
    { return input.GetBlue(); }


};

  
}  // end namespace itk

#endif

