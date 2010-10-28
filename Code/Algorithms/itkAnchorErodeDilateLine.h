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
#ifndef __itkAnchorErodeDilateLine_h
#define __itkAnchorErodeDilateLine_h

#include "itkAnchorHistogram.h"
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
