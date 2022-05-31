/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLabelSetUtils_h
#define itkLabelSetUtils_h

#include <itkArray.h>

#include "itkProgressReporter.h"
#include <vector>
namespace itk
{
namespace LabSet
{
template <class LineBufferType, class RealType>
void
DoLineErodeFirstPass(LineBufferType & LineBuf,
                     RealType         leftend,
                     RealType         rightend,
                     const RealType   magnitude,
                     const RealType   Sigma)
{
  // This is the first pass algorithm. We can write down the values
  // because we know the inputs are binary

  const long LineLength = LineBuf.size();

  for (long pos = 0; pos < LineLength; pos++)
  {
    // compute the height of the parabola starting at each end and
    // keep the minimum
    RealType left, right;
    unsigned offset = LineLength - pos;
    left = leftend - magnitude * (pos + 1) * (pos + 1);
    right = rightend - magnitude * offset * offset;
    // note hard coded value here - could be a parameter
    //    LineBuf[pos] = std::min(std::min(left, right),
    // itk::NumericTraits<RealType>::One);
    LineBuf[pos] = std::min(std::min(left, right), Sigma);
  }
}

template <class LineBufferType, class LabLineBufferType, class RealType>
void
DoLineDilateFirstPass(LineBufferType &    LineBuf,
                      LineBufferType &    tmpLineBuf,
                      LabLineBufferType & LabBuf,
                      LabLineBufferType & NewLabBuf,
                      const RealType      magnitude)
{
  // need to propagate the labels here
  const long LineLength = LineBuf.size();
  long       lastcontact = 0;
  RealType   lastval = LineBuf[0];

  for (long pos = 0; pos < LineLength; pos++)
  {
    // left pass
    RealType krange = pos - lastcontact;
    RealType thisval = lastval - magnitude * krange * krange;

    if (LineBuf[pos] >= LineBuf[lastcontact])
    {
      lastcontact = pos;
      lastval = LineBuf[pos];
    }
    tmpLineBuf[pos] = std::max(LineBuf[pos], thisval);
    if (thisval > LineBuf[pos])
    {
      NewLabBuf[pos] = LabBuf[lastcontact];
    }
    else
    {
      NewLabBuf[pos] = LabBuf[pos];
    }
  }

  lastcontact = LineLength - 1;
  lastval = tmpLineBuf[lastcontact];
  for (long pos = LineLength - 1; pos >= 0; pos--)
  {
    // right pass
    RealType krange = lastcontact - pos;
    RealType thisval = lastval - magnitude * krange * krange;

    if (tmpLineBuf[pos] >= tmpLineBuf[lastcontact])
    {
      lastcontact = pos;
      lastval = tmpLineBuf[pos];
    }
    LineBuf[pos] = std::max(tmpLineBuf[pos], thisval);
    if (thisval > tmpLineBuf[pos])
    {
      NewLabBuf[pos] = LabBuf[lastcontact];
    }
    // only need to do this bit on the first pass - it doubles as a
    // way of initializing NewLabPos
    // else
    //   {
    //   NewLabBuf[pos] = LabBuf[pos];
    //   }
  }
}

template <class LineBufferType, class RealType, bool doDilate>
void
DoLine(LineBufferType & LineBuf, LineBufferType & tmpLineBuf, const RealType magnitude, const RealType m_Extreme)
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

template <class LineBufferType, class LabBufferType, class RealType, bool doDilate>
void
DoLineLabelProp(LineBufferType & LineBuf,
                LineBufferType & tmpLineBuf,
                LabBufferType &  LabelBuf,
                LabBufferType &  tmpLabelBuf,
                const RealType   magnitude,
                const RealType   m_Extreme)
{
  // contact point algorithm
  long koffset = 0, newcontact = 0; // how far away the search starts.

  using LabelType = typename LabBufferType::ValueType;

  const long LineLength = LineBuf.size();
  // negative half of the parabola
  for (long pos = 0; pos < LineLength; pos++)
  {
    auto BaseVal = (RealType)m_Extreme; // the base value for
                                        // comparison
    LabelType BaseLab = LabelBuf[pos];
    for (long krange = koffset; krange <= 0; krange++)
    {
      // difference needs to be paramaterised
      RealType T = LineBuf[pos + krange] - magnitude * krange * krange;
      // switch on template parameter - hopefully gets optimized away.
      if (doDilate ? (T >= BaseVal) : (T <= BaseVal))
      {
        BaseVal = T;
        newcontact = krange;
        BaseLab = LabelBuf[pos + krange];
      }
    }
    tmpLineBuf[pos] = BaseVal;
    tmpLabelBuf[pos] = BaseLab;
    koffset = newcontact - 1;
  }
  // positive half of parabola
  koffset = newcontact = 0;
#if 1
  for (long pos = LineLength - 1; pos >= 0; pos--)
  {
    auto BaseVal = (RealType)m_Extreme; // the base value for comparison
    // initialize the label to the previously pro
    LabelType BaseLab = tmpLabelBuf[pos];
    for (long krange = koffset; krange >= 0; krange--)
    {
      RealType T = tmpLineBuf[pos + krange] - magnitude * krange * krange;
      if (doDilate ? (T >= BaseVal) : (T <= BaseVal))
      {
        BaseVal = T;
        newcontact = krange;
        BaseLab = tmpLabelBuf[pos + krange];
      }
    }
    LineBuf[pos] = BaseVal;
    LabelBuf[pos] = BaseLab;
    koffset = newcontact + 1;
  }
#else
  for (long pos = LineLength - 1; pos >= 0; pos--)
  {
    LineBuf[pos] = tmpLineBuf[pos];
    LabelBuf[pos] = tmpLabelBuf[pos];
  }

#endif
}

template <class TInIter, class TOutDistIter, class TOutLabIter, class RealType>
void
doOneDimensionErodeFirstPass(TInIter &          inputIterator,
                             TOutDistIter &     outputIterator,
                             TOutLabIter &      outputLabIterator,
                             ProgressReporter & progress,
                             const unsigned     LineLength,
                             const unsigned     direction,
                             const int          m_MagnitudeSign,
                             const bool         m_UseImageSpacing,
                             const RealType     image_scale,
                             const RealType     Sigma,
                             const bool         lastpass)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (m_UseImageSpacing)
  {
    iscale = image_scale;
  }
  // restructure equation to reduce numerical error
  //  const RealType magnitude = (m_MagnitudeSign * iscale * iscale)/(2.0 *
  // Sigma);
  const RealType  magnitude = (m_MagnitudeSign * iscale * iscale) / (2.0);
  LineBufferType  LineBuf(LineLength);
  LabelBufferType LabBuf(LineLength);

  inputIterator.SetDirection(direction);
  outputIterator.SetDirection(direction);
  outputLabIterator.SetDirection(direction);

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();
  outputLabIterator.GoToBegin();

  while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
  {
    // process this direction
    // fetch the line into the buffer - this methodology is like
    // the gaussian filters
    unsigned int i = 0;

    // copy the scanline to a buffer
    while (!inputIterator.IsAtEndOfLine())
    {
      LabBuf[i] = (inputIterator.Get());
      if (LabBuf[i])
      {
        LineBuf[i] = 1.0;
      }
      ++i;
      ++inputIterator;
    }
    // runlength encode the line buffer (could be integrated with extraction)

    using EndType = std::vector<unsigned>;
    EndType firsts;
    EndType lasts;

    for (unsigned idx = 0; idx < LineLength; idx++)
    {
      RealType val = LabBuf[idx];
      if (val != 0)
      {
        // found a run
        firsts.push_back(idx);
        unsigned idxend = idx;
        for (; idxend < LineLength; idxend++)
        {
          if (val != LabBuf[idxend])
          {
            break;
          }
        }
        lasts.push_back(idxend - 1);
        idx = idxend - 1;
      }
    }

    for (unsigned R = 0; R < firsts.size(); R++)
    {
      unsigned       first = firsts[R];
      unsigned       last = lasts[R];
      unsigned       SLL = last - first + 1;
      LineBufferType ShortLineBuf(SLL);
      // if one end of the run touches the image edge, then we leave
      // the value as 1
      RealType leftend = 0, rightend = 0;
      if (first == 0)
      {
        leftend = Sigma;
      }
      if (last == LineLength - 1)
      {
        rightend = Sigma;
      }

      DoLineErodeFirstPass<LineBufferType, RealType>(ShortLineBuf, leftend, rightend, magnitude, Sigma);
      // copy the segment back into the full line buffer
      std::copy(ShortLineBuf.begin(), ShortLineBuf.end(), &(LineBuf[first]));
    }
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputIterator.IsAtEndOfLine())
    {
      outputIterator.Set(static_cast<typename TOutDistIter::PixelType>(LineBuf[j++]));
      ++outputIterator;
    }

    if (lastpass)
    {
      // copy to the output image - this would be a weird case of only
      // using a one dimensional SE
      unsigned j2 = 0;
      while (!outputLabIterator.IsAtEndOfLine())
      {
        typename TInIter::PixelType val = 0;
        if (LineBuf[j2] == Sigma)
        {
          val = LabBuf[j2];
        }
        outputLabIterator.Set(val);
        ++outputLabIterator;
        ++j2;
      }
      outputLabIterator.NextLine();
    }

    // now onto the next line
    inputIterator.NextLine();
    outputIterator.NextLine();
    progress.CompletedPixel();
  }
}

template <class TInIter, class TOutDistIter, class TOutLabIter, class RealType>
void
doOneDimensionDilateFirstPass(TInIter &          inputIterator,
                              TOutDistIter &     outputIterator,
                              TOutLabIter &      outputLabIterator,
                              ProgressReporter & progress,
                              const unsigned     LineLength,
                              const unsigned     direction,
                              const int          m_MagnitudeSign,
                              const bool         m_UseImageSpacing,
                              const RealType     image_scale,
                              const RealType     Sigma)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (m_UseImageSpacing)
  {
    iscale = image_scale;
  }
  // restructure equation to reduce numerical error
  // const RealType magnitude = (m_MagnitudeSign * iscale * iscale)/(2.0 *
  // Sigma);
  const RealType  magnitude = (m_MagnitudeSign * iscale * iscale) / (2.0);
  LineBufferType  LineBuf(LineLength);
  LabelBufferType LabBuf(LineLength);
  LineBufferType  tmpLineBuf(LineLength);
  LabelBufferType newLabBuf(LineLength);

  inputIterator.SetDirection(direction);
  outputIterator.SetDirection(direction);
  outputLabIterator.SetDirection(direction);

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();
  outputLabIterator.GoToBegin();

  while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
  {
    // process this direction
    // fetch the line into the buffer - this methodology is like
    // the gaussian filters
    unsigned int i = 0;

    // copy the scanline to a buffer
    while (!inputIterator.IsAtEndOfLine())
    {
      LabBuf[i] = (inputIterator.Get());
      if (LabBuf[i])
      {
        LineBuf[i] = Sigma;
      }
      else
      {
        LineBuf[i] = 0;
      }
      ++i;
      ++inputIterator;
    }

    DoLineDilateFirstPass<LineBufferType, LabelBufferType, RealType>(LineBuf, tmpLineBuf, LabBuf, newLabBuf, magnitude);
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputIterator.IsAtEndOfLine())
    {
      outputIterator.Set(static_cast<typename TOutDistIter::PixelType>(LineBuf[j]));
      outputLabIterator.Set(newLabBuf[j]);
      ++outputLabIterator;
      ++outputIterator;
      ++j;
    }

    // now onto the next line
    inputIterator.NextLine();
    outputIterator.NextLine();
    outputLabIterator.NextLine();
    progress.CompletedPixel();
  }
}

template <class TInIter, class TDistIter, class TOutLabIter, class TOutDistIter, class RealType>
void
doOneDimensionErode(TInIter &          inputIterator,
                    TDistIter &        inputDistIterator,
                    TOutDistIter &     outputDistIterator,
                    TOutLabIter &      outputLabIterator,
                    ProgressReporter & progress,
                    const unsigned     LineLength,
                    const unsigned     direction,
                    const int          m_MagnitudeSign,
                    const bool         m_UseImageSpacing,
                    const RealType     m_Extreme,
                    const RealType     image_scale,
                    const RealType     Sigma,
                    const RealType     BaseSigma,
                    const bool         lastpass)
{
  // traditional erosion - can't optimise the same way as the first pass
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (m_UseImageSpacing)
  {
    iscale = image_scale;
  }
  const RealType  magnitude = (m_MagnitudeSign * iscale * iscale) / (2.0 * Sigma);
  LineBufferType  LineBuf(LineLength);
  LabelBufferType LabBuf(LineLength);

  inputIterator.SetDirection(direction);
  outputDistIterator.SetDirection(direction);
  inputDistIterator.SetDirection(direction);
  outputLabIterator.SetDirection(direction);

  inputIterator.GoToBegin();
  outputDistIterator.GoToBegin();
  inputDistIterator.GoToBegin();
  outputLabIterator.GoToBegin();

  while (!inputIterator.IsAtEnd() && !outputDistIterator.IsAtEnd())
  {
    // process this direction
    // fetch the line into the buffer - this methodology is like
    // the gaussian filters
    unsigned int i = 0;

    // copy the scanline to a buffer
    while (!inputIterator.IsAtEndOfLine())
    {
      LineBuf[i] = static_cast<RealType>(inputDistIterator.Get());
      LabBuf[i] = inputIterator.Get();
      ++i;
      ++inputDistIterator;
      ++inputIterator;
    }
    // runlength encode the line buffer (could be integrated with extraction)
    using EndType = std::vector<unsigned>;
    EndType firsts;
    EndType lasts;
    for (unsigned idx = 0; idx < LineLength; idx++)
    {
      RealType val = LabBuf[idx];
      if (val != 0)
      {
        // found a run
        firsts.push_back(idx);
        unsigned idxend = idx;
        for (; idxend < LineLength; idxend++)
        {
          if (val != LabBuf[idxend])
          {
            break;
          }
        }
        lasts.push_back(idxend - 1);
        idx = idxend - 1;
      }
    }

    for (unsigned R = 0; R < firsts.size(); R++)
    {
      unsigned       first = firsts[R];
      unsigned       last = lasts[R];
      unsigned       SLL = last - first + 1;
      LineBufferType ShortLineBuf(SLL + 2);
      LineBufferType tmpShortLineBuf(SLL + 2);

      // if one end of the run touches the image edge, then we leave
      // the value as 1
      RealType leftend = 0, rightend = 0;
      if (first == 0)
      {
        leftend = BaseSigma;
      }
      if (last == LineLength - 1)
      {
        rightend = BaseSigma;
      }

      ShortLineBuf[0] = leftend;
      ShortLineBuf[SLL + 1] = rightend;

      std::copy(&(LineBuf[first]), &(LineBuf[last + 1]), &(ShortLineBuf[1]));

      DoLine<LineBufferType, RealType, false>(ShortLineBuf, tmpShortLineBuf, magnitude, m_Extreme);
      // copy the segment back into the full line buffer
      std::copy(&(ShortLineBuf[1]), &(ShortLineBuf[SLL + 1]), &(LineBuf[first]));
    }
    // copy the line buffer back to the image - don't need to do it on
    // the last pass - move when we are sure it is working
    unsigned j = 0;
    while (!outputDistIterator.IsAtEndOfLine())
    {
      outputDistIterator.Set(static_cast<typename TOutDistIter::PixelType>(LineBuf[j++]));
      ++outputDistIterator;
    }

    if (lastpass)
    {
      unsigned j2 = 0;
      while (!outputLabIterator.IsAtEndOfLine())
      {
        typename TInIter::PixelType val = 0;
        if (LineBuf[j2] == BaseSigma)
        {
          val = LabBuf[j2];
        }
        outputLabIterator.Set(val);
        ++outputLabIterator;
        ++j2;
      }
      outputLabIterator.NextLine();
    }
    // now onto the next line
    inputIterator.NextLine();
    inputDistIterator.NextLine();
    outputDistIterator.NextLine();
    progress.CompletedPixel();
  }
}

template <class TInIter, class TDistIter, class TOutLabIter, class TOutDistIter, class RealType>
void
doOneDimensionDilate(TInIter &          inputIterator,
                     TDistIter &        inputDistIterator,
                     TOutDistIter &     outputDistIterator,
                     TOutLabIter &      outputLabIterator,
                     ProgressReporter & progress,
                     const unsigned     LineLength,
                     const unsigned     direction,
                     const int          m_MagnitudeSign,
                     const bool         m_UseImageSpacing,
                     const RealType     m_Extreme,
                     const RealType     image_scale,
                     const RealType     Sigma)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (m_UseImageSpacing)
  {
    iscale = image_scale;
  }
  // restructure equation to reduce numerical error
  const RealType magnitude = (m_MagnitudeSign * iscale * iscale) / (2.0 * Sigma);
  //  const RealType magnitude = (m_MagnitudeSign * iscale * iscale)/(2.0 );
  LineBufferType  LineBuf(LineLength);
  LabelBufferType LabBuf(LineLength);
  LineBufferType  tmpLineBuf(LineLength);
  LabelBufferType newLabBuf(LineLength);
  LabelBufferType tmpLabBuf(LineLength);

  inputIterator.SetDirection(direction);
  inputDistIterator.SetDirection(direction);
  outputDistIterator.SetDirection(direction);
  outputLabIterator.SetDirection(direction);

  inputIterator.GoToBegin();
  inputDistIterator.GoToBegin();
  outputDistIterator.GoToBegin();
  outputLabIterator.GoToBegin();

  while (!inputDistIterator.IsAtEnd() && !outputLabIterator.IsAtEnd())
  {
    // process this direction
    // fetch the line into the buffer - this methodology is like
    // the gaussian filters
    unsigned int i = 0;

    // copy the scanline to a buffer
    while (!inputDistIterator.IsAtEndOfLine())
    {
      LineBuf[i] = inputDistIterator.Get();
      LabBuf[i] = inputIterator.Get();
      ++i;
      ++inputIterator;
      ++inputDistIterator;
    }

    DoLineLabelProp<LineBufferType, LabelBufferType, RealType, true>(
      LineBuf, tmpLineBuf, LabBuf, tmpLabBuf, magnitude, m_Extreme);
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputDistIterator.IsAtEndOfLine())
    {
      outputDistIterator.Set(static_cast<typename TOutDistIter::PixelType>(LineBuf[j]));
      outputLabIterator.Set(LabBuf[j]);
      ++outputDistIterator;
      ++outputLabIterator;
      j++;
    }

    // now onto the next line
    inputIterator.NextLine();
    outputLabIterator.NextLine();
    inputDistIterator.NextLine();
    outputDistIterator.NextLine();
    progress.CompletedPixel();
  }
}
} // namespace LabSet
} // namespace itk
#endif
