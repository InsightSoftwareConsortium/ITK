/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColorTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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

  itkGetMacro(NumberOfColors, unsigned int);
  unsigned int     size(void);

  RGBPixel<TPixel>*          GetColor(unsigned int colorId);
  RGBPixel<TPixel>*          color(unsigned int c);

  bool    SetColor(unsigned int c, TPixel r, TPixel g, TPixel b,
                   const char * name="UserDefined");
  
  /** Given the position in the table and the color 
   * returns the value. \todo Needs renaming. */
  TPixel  GetColorComponent(unsigned int colorId, char rgb);
  TPixel  color(unsigned int c, char rgb);
  char *  GetColorName(unsigned int colorId);
  char *  colorName(unsigned int c);

  unsigned int GetClosestColorTableId(TPixel r, TPixel g, TPixel b);
   
protected:
  ColorTable();
  void PrintSelf(std::ostream & os, Indent indent) const;
  virtual ~ColorTable();

  unsigned int         m_NumberOfColors;
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
