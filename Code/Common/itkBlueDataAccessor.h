/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBlueDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkBlueDataAccessor_h
#define __itkBlueDataAccessor_h


#include "itkRGBPixel.h"


namespace itk
{

/**
 * \class BlueDataAccessor
 * \brief Give access to the Blue component of a RGBPixel type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Blue component.
 *
 * \sa ImageAdaptor
 *
 */

template <class T>
class ITK_EXPORT BlueDataAccessor
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   BlueDataAccessor        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef   RGBPixel<T>    InternalType;


  /** 
   * Write access to the Blue component
   */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { output.SetBlue( input ); }


  /** 
   * Read access to the Blue component
   */
  inline const ExternalType & Get( const InternalType & input ) const
    { return input.GetBlue(); }


};

  
}  // end namespace itk

#endif

