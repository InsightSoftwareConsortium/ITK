/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkParabolicMorphUtils_h
#define itkParabolicMorphUtils_h

#include <itkArray.h>

#include "itkProgressReporter.h"

namespace itk
{
// contact point algorithm
template <typename LineBufferType, typename RealType, bool doDilate>
void
DoLineCP(LineBufferType & LineBuf, LineBufferType & tmpLineBuf, const RealType magnitude, const RealType m_Extreme)
{
  // contact point algorithm
  long koffset = 0, newcontact = 0; // how far away the search starts.

  const long LineLength = LineBuf.size();

  // negative half of the parabola
  for (long pos = 0; pos < LineLength; pos++)
  {
    auto BaseVal = (RealType)m_Extreme; // the base value for
                                        // comparison
    for (long krange = koffset; krange <= 0; krange++)
    {
      // difference needs to be paramaterised
      RealType T = LineBuf[pos + krange] - magnitude * krange * krange;
      // switch on template parameter - hopefully gets optimized away.
      if (doDilate ? (T >= BaseVal) : (T <= BaseVal))
      {
        BaseVal = T;
        newcontact = krange;
      }
    }
    tmpLineBuf[pos] = BaseVal;
    koffset = newcontact - 1;
  }
  // positive half of parabola
  koffset = newcontact = 0;
  for (long pos = LineLength - 1; pos >= 0; pos--)
  {
    auto BaseVal = (RealType)m_Extreme; // the base value for comparison
    for (long krange = koffset; krange >= 0; krange--)
    {
      RealType T = tmpLineBuf[pos + krange] - magnitude * krange * krange;
      if (doDilate ? (T >= BaseVal) : (T <= BaseVal))
      {
        BaseVal = T;
        newcontact = krange;
      }
    }
    LineBuf[pos] = BaseVal;
    koffset = newcontact + 1;
  }
}

// intersection algorithm
// This algorithm has been described a couple of times. First by van
// den Boomgaard and more recently by Felzenszwalb and Huttenlocher,
// in the context of generalized distance transform
template <typename LineBufferType, typename IndexBufferType, typename EnvBufferType, typename RealType, bool doDilate>
void
DoLineIntAlg(LineBufferType &  LineBuf,
             EnvBufferType &   F,
             IndexBufferType & v,
             EnvBufferType &   z,
             const RealType    magnitude)
{
  int k; /* Index of rightmost parabola in lower envelope */
  /* Locations of parabolas in lower envelope */
  /* these need to be int, rather than unsigned, to make boundary
  conditions easy to test */

  // v stores locations of parabolas in the lower envelope
  // z stores thr location of boundaries between parabolas

  // I've gone nuts with the static casts etc, because I seemed to
  // have strange behaviour when I didn't do this. Also managed to get
  // rid of all the warnings by sticking to size_t and equivalents.
  RealType s;

  /* holds precomputed scale*f(q) + q^2 for speedup */
  //  LineBufferType F(LineBuf.size());

  // initialize
  k = 0;
  v[0] = 0;
  z[0] = NumericTraits<int>::NonpositiveMin();
  z[1] = NumericTraits<int>::max();
  F[0] = LineBuf[0] / magnitude;
  const size_t N(LineBuf.size());

  for (size_t q = 1; q < N; q++) /* main loop */
  {
    if (doDilate)
    {
      /* precompute f(q) + q^2 for speedup */
      F[q] = (LineBuf[q] / magnitude) - (static_cast<RealType>(q) * static_cast<RealType>(q));
      k++;
      do
      {
        /* remove last parabola from surface */
        k--;
        /* compute intersection */
        s = (F[q] - F[v[k]]) / (2.0 * (v[k] - static_cast<RealType>(q)));
      } while (s <= z[k]);
      /* bump k to add new parabola */
      k++;
    }
    else
    {
      /* precompute f(q) + q^2 for speedup */
      F[q] = (LineBuf[q] / magnitude) + (static_cast<RealType>(q) * static_cast<RealType>(q));
      k++;
      do
      {
        /* remove last parabola from surface */
        k--;
        /* compute intersection */
        s = (F[q] - F[v[k]]) / (2.0 * (static_cast<RealType>(q) - v[k]));
      } while (s <= z[k]);
      /* bump k to add new parabola */
      k++;
    }
    v[k] = q;
    z[k] = s;
    itkAssertInDebugAndIgnoreInReleaseMacro((size_t)(k + 1) <= N);
    z[k + 1] = NumericTraits<int>::max();
  } /* for q */
  /* now reconstruct output */
  if (doDilate)
  {
    k = 0;
    for (size_t q = 0; q < N; q++)
    {
      while (z[k + 1] < static_cast<typename IndexBufferType::ValueType>(q))
      {
        k++;
      }
      itkAssertInDebugAndIgnoreInReleaseMacro(static_cast<size_t>(v[k]) < N);
      itkAssertInDebugAndIgnoreInReleaseMacro(static_cast<size_t>(v[k]) >= 0);
      LineBuf[q] = static_cast<RealType>(
        (F[v[k]] - (static_cast<RealType>(q) * (static_cast<RealType>(q) - 2 * v[k]))) * magnitude);
    }
  }
  else
  {
    k = 0;
    for (size_t q = 0; q < N; q++)
    {
      while (z[k + 1] < static_cast<typename IndexBufferType::ValueType>(q))
      {
        k++;
      }
      itkAssertInDebugAndIgnoreInReleaseMacro(static_cast<size_t>(v[k]) < N);
      itkAssertInDebugAndIgnoreInReleaseMacro(static_cast<size_t>(v[k]) >= 0);
      LineBuf[q] = ((static_cast<RealType>(q) * (static_cast<RealType>(q) - 2 * v[k]) + F[v[k]]) * magnitude);
    }
  }
}

template <typename TInIter, typename TOutIter, typename RealType, typename OutputPixelType, bool doDilate>
void
doOneDimension(TInIter &          inputIterator,
               TOutIter &         outputIterator,
               ProgressReporter & progress,
               const long         LineLength,
               const unsigned     direction,
               const int          m_MagnitudeSign,
               const bool         m_UseImageSpacing,
               const RealType     m_Extreme,
               const RealType     image_scale,
               const RealType     Sigma,
               int                ParabolicAlgorithmChoice)
{
  enum ParabolicAlgorithm
  {
    NOCHOICE = 0,     // decices based on scale - experimental
    CONTACTPOINT = 1, // sometimes faster at low scale
    INTERSECTION = 2  // default
  };

  //  using LineBufferType = typename std::vector<RealType>;

  // message from M.Starring suggested performance gain using Array
  // instead of std::vector.
  using LineBufferType = typename itk::Array<RealType>;
  RealType iscale = 1.0;
  if (m_UseImageSpacing)
  {
    iscale = image_scale;
  }
  if (ParabolicAlgorithmChoice == NOCHOICE)
  {
    // both set to true or false - use scale to figure it out
    if ((2.0 * Sigma) < 0.2)
    {
      ParabolicAlgorithmChoice = CONTACTPOINT;
    }
    else
    {
      ParabolicAlgorithmChoice = INTERSECTION;
    }
  }

  if (ParabolicAlgorithmChoice == CONTACTPOINT)
  {
    // using the contact point algorithm

    //  const RealType magnitude = m_MagnitudeSign * 1.0/(2.0 *
    //  Sigma/(iscale*iscale));
    // restructure equation to reduce numerical error
    const RealType magnitudeCP = (m_MagnitudeSign * iscale * iscale) / (2.0 * Sigma);

    LineBufferType LineBuf(LineLength);
    LineBufferType tmpLineBuf(LineLength);
    inputIterator.SetDirection(direction);
    outputIterator.SetDirection(direction);
    inputIterator.GoToBegin();
    outputIterator.GoToBegin();

    unsigned count = 0;
    while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
    {
      // process this direction
      // fetch the line into the buffer - this methodology is like
      // the gaussian filters

      unsigned int i = 0;
      while (!inputIterator.IsAtEndOfLine())
      {
        LineBuf[i++] = static_cast<RealType>(inputIterator.Get());
        ++inputIterator;
      }

      DoLineCP<LineBufferType, RealType, doDilate>(LineBuf, tmpLineBuf, magnitudeCP, m_Extreme);
      // copy the line back
      unsigned int j = 0;
      while (!outputIterator.IsAtEndOfLine())
      {
        outputIterator.Set(static_cast<OutputPixelType>(LineBuf[j++]));
        ++outputIterator;
      }

      ++count;
      // now onto the next line
      inputIterator.NextLine();
      outputIterator.NextLine();
      progress.CompletedPixel();
    }
  }
  else
  {
    // using the Intersection algorithm
    using IndexBufferType = typename itk::Array<int>;

    const RealType  magnitudeInt = (iscale * iscale) / (2.0 * Sigma);
    LineBufferType  LineBuf(LineLength);
    LineBufferType  Fbuf(LineLength);
    IndexBufferType Vbuf(LineLength);
    LineBufferType  Zbuf(LineLength + 1);

    inputIterator.SetDirection(direction);
    outputIterator.SetDirection(direction);
    inputIterator.GoToBegin();
    outputIterator.GoToBegin();

    unsigned count = 0;
    while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
    {
      // process this direction
      // fetch the line into the buffer - this methodology is like
      // the gaussian filters

      unsigned int i = 0;
      while (!inputIterator.IsAtEndOfLine())
      {
        LineBuf[i++] = static_cast<RealType>(inputIterator.Get());
        ++inputIterator;
      }
      DoLineIntAlg<LineBufferType, IndexBufferType, LineBufferType, RealType, doDilate>(
        LineBuf, Fbuf, Vbuf, Zbuf, magnitudeInt);
      // copy the line back
      unsigned int j = 0;
      while (!outputIterator.IsAtEndOfLine())
      {
        outputIterator.Set(static_cast<OutputPixelType>(LineBuf[j++]));
        ++outputIterator;
      }

      ++count;
      // now onto the next line
      inputIterator.NextLine();
      outputIterator.NextLine();
      progress.CompletedPixel();
    }
  }
}
} // namespace itk
#endif
