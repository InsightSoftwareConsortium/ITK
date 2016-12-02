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
#ifndef itkAnchorErodeDilateLine_h
#define itkAnchorErodeDilateLine_h

#include "itkMovingHistogramMorphologyImageFilter.h"

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
 * \ingroup ITKMathematicalMorphology
 */
template< typename TInputPix, typename TCompare >
class ITK_TEMPLATE_EXPORT AnchorErodeDilateLine
{
public:
  /** Some convenient typedefs. */
  typedef TInputPix InputImagePixelType;

  void DoLine(std::vector<TInputPix> & buffer, std::vector<TInputPix> & inbuffer,
              unsigned bufflength);

  void SetSize(unsigned int size)
  {
    m_Size = size;
  }

  void PrintSelf(std::ostream & os, Indent indent) const;

  AnchorErodeDilateLine();
  ~AnchorErodeDilateLine()
  {
  }

private:
  unsigned int m_Size;

  typedef Function::MorphologyHistogram< InputImagePixelType, TCompare >              HistogramType;

  bool StartLine(std::vector<TInputPix> & buffer,
                 std::vector<TInputPix> & inbuffer,
                 InputImagePixelType & Extreme,
                 int & outLeftP,
                 int & outRightP,
                 int & inLeftP,
                 int & inRightP,
                 int middle);

  void FinishLine(std::vector<TInputPix> & buffer,
                  std::vector<TInputPix> & inbuffer,
                  InputImagePixelType & Extreme,
                  int & outLeftP,
                  int & outRightP,
                  int & inLeftP,
                  int & inRightP,
                  int middle);

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

  inline bool StrictCompare( const InputImagePixelType & a, const InputImagePixelType & b )
    {
    TCompare compare;
    return compare( a, b );
    }

  inline bool Compare( const InputImagePixelType & a, const InputImagePixelType & b )
    {
    TCompare compare;
    return compare( a, b ) || Math::AlmostEquals(a, b);
    }

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnchorErodeDilateLine.hxx"
#endif

#endif
