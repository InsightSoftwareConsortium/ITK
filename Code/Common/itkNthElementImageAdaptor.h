/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkNthElementImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNthElementImageAdaptor_h
#define __itkNthElementImageAdaptor_h

#include <itkImageAdaptor.h>
#include <itkNthElementDataAccessor.h>

namespace itk
{
 

 
/**
 * \class NthElementImageAdaptor
 * \brief Presents an image as being composed of the N-th element of its pixels
 *
 * iIt assumes that the pixels are of container type and have in their API
 * an operator[]( unsigned int ) defined.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT NthElementImageAdaptor : public
      ImageAdaptor<TImage,
                   NthElementDataAccessor<
                                      TOutputPixelType,
                                      typename TImage::PixelType> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef NthElementImageAdaptor  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageAdaptor<TImage,
                       NthElementImageAdaptor<
                                       TOutputPixelType,
                                       typename TImage::PixelType > >
                                                            Superclass;
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( NthElementImageAdaptor, ImageAdaptor );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  


  /**
   * Select the element number to be accessed
   */
  void SelectNthElement( unsigned int nth ) 
  { this->GetDataAccessor().SetElementNumber( nth ); 
    this->Modified(); }
    

};

} // end namespace itk

#endif
