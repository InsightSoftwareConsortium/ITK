/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColorTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkColorTable_h
#define __itkColorTable_h

#include <itkObject.h>
#include <stdio.h>
#include <itkRGBPixel.h>
#include <itkObjectFactory.h>

namespace itk 
{
/** \class ColorTable
 *  itkColorTable Class define a Color table for image visualisation
 * 
 * \ingroup DataRepresentation
 */

template<class TPixel>
class ITK_EXPORT ColorTable : public Object
{
public:
  /** Standard class typedefs. */
  typedef ColorTable   Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ColorTable,Object);

  /** Badly named methods that require renaming and documentation. */ 
  void    useDiscrete(void);
  void    useGray(int n=256);
  void    useHeat(int n=256);
  int     size(void);
  RGBPixel<TPixel>*          color(int c);
  
  /** Given the position in the table and the color 
   * returns the value. \todo Needs renaming. */
  TPixel  color(int c, char rgb);
  char *  colorName(int c);
   
protected:
  ColorTable();
  virtual ~ColorTable();

  int                  m_NumberOfColors;
  RGBPixel<TPixel> *   m_Color;
  char **              m_ColorName;

private:
  ColorTable(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};


}// namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkColorTable.txx"
#endif

#endif
