/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkGreenDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkGreenDataAccessor_h
#define __itkGreenDataAccessor_h


#include "itkRGBPixel.h"


namespace itk
{

/**
 * \class GreenDataAccessor
 * \brief Give access to the Green component of a RGBPixel type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Green component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT GreenDataAccessor
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   GreenDataAccessor        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef     RGBPixel<T>   InternalType;


  /** 
   * Write access to the Green component
   */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { output.SetGreen( input ); }


  /** 
   * Read access to the Green component
   */
  inline const ExternalType & Get( const InternalType & input ) const
    { return input.GetGreen(); }


};

  
} // end namespace itk

#endif

