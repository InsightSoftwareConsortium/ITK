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
  typedef ColorTable                Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ColorTable,Object);

  /** Generate a lookup table of 8 discrete colors. The colors are Red,
    * Purple, Aqua, Yellow, Green, Blue, Grey0.70, White.
    */
  void    UseDiscreteColors(void);
  /** Generate a lookuptable of n grayscale values. A ramp is
    * generated from NonpositiveMin() to max() of the pixel type.
    */
  void    UseGrayColors(unsigned int n=256);
  void    UseHeatColors(unsigned int n=256);
  void    UseRandomColors(unsigned int n=256);

  /** Badly named methods that require renaming and documentation. */ 
  void    useDiscrete(void){UseDiscreteColors();};
  void    useGray(unsigned int n=256){UseGrayColors(n);}
  void    useHeat(unsigned int n=256){UseHeatColors(n);}

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
  void DeleteColors();
};


}// namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ColorTable(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT ColorTable< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef ColorTable< ITK_TEMPLATE_1 x > \
                                            ColorTable##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkColorTable+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkColorTable.txx"
#endif

#endif
