/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDefaultDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDefaultDataAccessor_h
#define __itkDefaultDataAccessor_h


namespace itk
{

/**
 * \class DefaultDataAccessor
 * \brief Give access to partial aspects a type
 *
 * DefaultDataAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized convertion between the internal and external
 * type representations.
 *
 */

template <class TType>
class ITK_EXPORT DefaultDataAccessor  
{
public:

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit.
   */
  typedef TType ExternalType ;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data.
   */
  typedef TType InternalType ;


  static inline void Set(TType & output, const TType & input)
    {output = input;}

  static inline const TType & Get( const TType & input )
    {return input;}

};

  
} // end namespace itk
  

#endif

