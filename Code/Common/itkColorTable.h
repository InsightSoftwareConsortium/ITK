/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColorTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkColorTable_h
#define __itkColorTable_h

#include <itkObject.h>
#include <itkRGBPixel.h>

namespace itk 
{
/** \class ColorTable
 *  itkColorTable Class define a Color table for image visualisation
 *
 */

template<class TPixel>
class ITK_EXPORT ColorTable : public Object
{
public:

	/**
   * Standard "Self" typedef.
   */
  typedef ColorTable   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ColorTable,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  void    useDiscrete(void);
  void    useGray(int n=256);
  void    useHeat(int n=256);

  int     size(void);

  /*Given the position in the table returns an RGB Pixel corresponding*/ 
  RGBPixel<TPixel>*          color(int c);

  /**
   * Given the position in the table and the color 
   * returns the value
   */
  TPixel                     color(int c, char rgb);
  
  char *                     colorName(int c);
   

	protected:

  int                  m_NumberOfColors;
  RGBPixel<TPixel> *   m_Color;
	char **              m_ColorName;

  ColorTable();
  virtual ~ColorTable();
  ColorTable(const Self&) {}
  void operator=(const Self&) {}


};


}// namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkColorTable.txx"
#endif

#endif