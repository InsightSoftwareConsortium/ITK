/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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

template< class TPixel >
class ITK_EXPORT ColorTable:public Object
{
public:
  /** Standard class typedefs. */
  typedef ColorTable                 Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ColorTable, Object);

  /** Generate a lookup table of 8 discrete colors. The colors are Red,
    * Purple, Aqua, Yellow, Green, Blue, Grey0.70, White.
    */
  void    UseDiscreteColors(void);

  /** Generate a lookuptable of n grayscale values. A ramp is
    * generated from NonpositiveMin() to max() of the pixel type.
    */
  void    UseGrayColors(unsigned int n = 256);

  void    UseHeatColors(unsigned int n = 256);

  void    UseRandomColors(unsigned int n = 256);

  /** Badly named methods that require renaming and documentation. */
  void    useDiscrete(void){ UseDiscreteColors(); }
  void    useGray(unsigned int n = 256){ UseGrayColors(n); }
  void    useHeat(unsigned int n = 256){ UseHeatColors(n); }

  itkGetConstMacro(NumberOfColors, unsigned int);
  unsigned int     size(void);

  RGBPixel< TPixel > *          GetColor(unsigned int colorId);

  RGBPixel< TPixel > *          color(unsigned int c);

  bool    SetColor(unsigned int c, TPixel r, TPixel g, TPixel b,
                   const char *name = "UserDefined");

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

  unsigned int m_NumberOfColors;

  char **m_ColorName;

  RGBPixel< TPixel > *m_Color;
private:
  ColorTable(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void DeleteColors();
};
} // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ColorTable(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                              \
  {                                                          \
  _( 1 ( class EXPORT ColorTable< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                        \
  {                                                          \
  typedef ColorTable< ITK_TEMPLATE_1 TypeX >                 \
  ColorTable##TypeY;                                       \
  }                                                          \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkColorTable+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkColorTable.txx"
#endif

#endif
