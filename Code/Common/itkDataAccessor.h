/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDataAccessor_h
#define __itkDataAccessor_h


namespace itk
{

/**
 * \class DataAccessor
 * \brief Give access to partial aspects a type
 *
 * DataAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized convertion between the internal and external
 * type representations.
 *
 */

template <class TInternalType, class TExternalType >
class ITK_EXPORT DataAccessor  
{
public:

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit.
   */
  typedef TExternalType ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data.
   */
  typedef TInternalType InternalType;


  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType) input;}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)input;}

};

  
} // end namespace itk
  

#endif

