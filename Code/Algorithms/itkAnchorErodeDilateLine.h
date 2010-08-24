/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorErodeDilateLine.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorErodeDilateLine_h
#define __itkAnchorErodeDilateLine_h

#include "itkAnchorHistogram.h"
#include "itkIndent.h"
namespace itk
{
/**
 * \class AnchorErodeDilateLine
 * \brief class to implement erosions and dilations using anchor
 * methods. This is the base class that must be instantiated with
 * appropriate definitions of greater, less and so on. There is
 * special code for cases where the structuring element is bigger than
 * the image size that aren't particularly anchor related, but use the
 * same data structures. Hopefully these sections occupy a very minor
 * proportion of the time.
 */
template< class TInputPix, class TFunction1, class TFunction2 >
class ITK_EXPORT AnchorErodeDilateLine
{
public:
  /** Some convenient typedefs. */
  typedef TInputPix InputImagePixelType;

  void DoLine(InputImagePixelType *buffer, InputImagePixelType *inbuffer,
              unsigned bufflength);

  void SetSize(unsigned int size)
  {
    m_Size = size;
  }

  void PrintSelf(std::ostream & os, Indent indent) const;

  AnchorErodeDilateLine();
  ~AnchorErodeDilateLine()
  {
    delete m_Histo;
  }

private:
  unsigned int m_Size;
  TFunction1   m_TF1;
  TFunction2   m_TF2;

  bool m_UseVec;

  typedef MorphologyHistogram< InputImagePixelType >                Histogram;
  typedef MorphologyHistogramVec< InputImagePixelType, TFunction1 > VHistogram;
  typedef MorphologyHistogramMap< InputImagePixelType, TFunction1 > MHistogram;

  bool StartLine(InputImagePixelType *buffer,
                 InputImagePixelType *inbuffer,
                 InputImagePixelType & Extreme,
                 Histogram & histo,
                 int & outLeftP,
                 int & outRightP,
                 int & inLeftP,
                 int & inRightP,
                 int middle, unsigned bufflength);

  void FinishLine(InputImagePixelType *buffer,
                  InputImagePixelType *inbuffer,
                  InputImagePixelType & Extreme,
                  Histogram & histo,
                  int & outLeftP,
                  int & outRightP,
                  int & inLeftP,
                  int & inRightP,
                  int middle, unsigned bufflength);

  bool UseVectorBasedHistogram()
  {
    // bool, short and char are acceptable for vector based algorithm: they do
    // not require
    // too much memory. Other types are not usable with that algorithm
    return typeid( InputImagePixelType ) == typeid( unsigned char )
           || typeid( InputImagePixelType ) == typeid( signed char )
           || typeid( InputImagePixelType ) == typeid( unsigned short )
           || typeid( InputImagePixelType ) == typeid( signed short )
           || typeid( InputImagePixelType ) == typeid( bool );
  }

  Histogram *m_Histo;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnchorErodeDilateLine.txx"
#endif

#endif
