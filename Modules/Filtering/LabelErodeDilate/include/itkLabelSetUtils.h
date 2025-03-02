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
DoLineErodeFirstPass(LineBufferType & lineBuf,
                     RealType         leftend,
                     RealType         rightend,
                     const RealType   magnitude,
                     const RealType   sigma)
{
  // This is the first pass algorithm. We can write down the values
  // because we know the inputs are binary

  const long lineLength = lineBuf.size();

  for (long pos = 0; pos < lineLength; pos++)
  {
    // compute the height of the parabola starting at each end and
    // keep the minimum
    RealType left, right;
    unsigned offset = lineLength - pos;
    left = leftend - magnitude * (pos + 1) * (pos + 1);
    right = rightend - magnitude * offset * offset;
    // note hard coded value here - could be a parameter
    //    lineBuf[pos] = std::min(std::min(left, right),
    // itk::NumericTraits<RealType>::One);
    lineBuf[pos] = std::min(std::min(left, right), sigma);
  }
}

template <class LineBufferType, class LabLineBufferType, class RealType>
void
DoLineDilateFirstPass(LineBufferType &    lineBuf,
                      LineBufferType &    tmpLineBuf,
                      LabLineBufferType & labBuf,
                      LabLineBufferType & NewLabBuf,
                      const RealType      magnitude)
{
  // need to propagate the labels here
  const long lineLength = lineBuf.size();
  long       lastcontact = 0;
  RealType   lastval = lineBuf[0];

  for (long pos = 0; pos < lineLength; pos++)
  {
    // left pass
    RealType krange = pos - lastcontact;
    RealType thisval = lastval - magnitude * krange * krange;

    if (lineBuf[pos] >= lineBuf[lastcontact])
    {
      lastcontact = pos;
      lastval = lineBuf[pos];
    }
    tmpLineBuf[pos] = std::max(lineBuf[pos], thisval);
    if (thisval > lineBuf[pos])
    {
      NewLabBuf[pos] = labBuf[lastcontact];
    }
    else
    {
      NewLabBuf[pos] = labBuf[pos];
    }
  }

  lastcontact = lineLength - 1;
  lastval = tmpLineBuf[lastcontact];
  for (long pos = lineLength - 1; pos >= 0; pos--)
  {
    // right pass
    RealType krange = lastcontact - pos;
    RealType thisval = lastval - magnitude * krange * krange;

    if (tmpLineBuf[pos] >= tmpLineBuf[lastcontact])
    {
      lastcontact = pos;
      lastval = tmpLineBuf[pos];
    }
    lineBuf[pos] = std::max(tmpLineBuf[pos], thisval);
    if (thisval > tmpLineBuf[pos])
    {
      NewLabBuf[pos] = labBuf[lastcontact];
    }
    // only need to do this bit on the first pass - it doubles as a
    // way of initializing NewLabPos
    // else
    //   {
    //   NewLabBuf[pos] = labBuf[pos];
    //   }
  }
}

template <class LineBufferType, class RealType, bool doDilate>
void
DoLine(LineBufferType & lineBuf, LineBufferType & tmpLineBuf, const RealType magnitude, const RealType extreme)
{
  // contact point algorithm
  long koffset = 0, newcontact = 0; // how far away the search starts.

  const long lineLength = lineBuf.size();

  // negative half of the parabola
  for (long pos = 0; pos < lineLength; pos++)
  {
    auto baseVal = (RealType)extreme; // the base value for comparison
    for (long krange = koffset; krange <= 0; krange++)
    {
      // difference needs to be paramaterised
      RealType T = lineBuf[pos + krange] - magnitude * krange * krange;
      // switch on template parameter - hopefully gets optimized away.
      if (doDilate ? (T >= baseVal) : (T <= baseVal))
      {
        baseVal = T;
        newcontact = krange;
      }
    }
    tmpLineBuf[pos] = baseVal;
    koffset = newcontact - 1;
  }
  // positive half of parabola
  koffset = newcontact = 0;
  for (long pos = lineLength - 1; pos >= 0; pos--)
  {
    auto baseVal = (RealType)extreme; // the base value for comparison
    for (long krange = koffset; krange >= 0; krange--)
    {
      RealType T = tmpLineBuf[pos + krange] - magnitude * krange * krange;
      if (doDilate ? (T >= baseVal) : (T <= baseVal))
      {
        baseVal = T;
        newcontact = krange;
      }
    }
    lineBuf[pos] = baseVal;
    koffset = newcontact + 1;
  }
}

template <class LineBufferType, class LabBufferType, class RealType, bool doDilate>
void
DoLineLabelProp(LineBufferType & lineBuf,
                LineBufferType & tmpLineBuf,
                LabBufferType &  labelBuf,
                LabBufferType &  tmpLabelBuf,
                const RealType   magnitude,
                const RealType   extreme)
{
  // contact point algorithm
  long koffset = 0, newcontact = 0; // how far away the search starts.

  using LabelType = typename LabBufferType::ValueType;

  const long lineLength = lineBuf.size();
  // negative half of the parabola
  for (long pos = 0; pos < lineLength; pos++)
  {
    auto      baseVal = (RealType)extreme; // the base value for comparison
    LabelType baseLab = labelBuf[pos];
    for (long krange = koffset; krange <= 0; krange++)
    {
      // difference needs to be paramaterised
      RealType T = lineBuf[pos + krange] - magnitude * krange * krange;
      // switch on template parameter - hopefully gets optimized away.
      if (doDilate ? (T >= baseVal) : (T <= baseVal))
      {
        baseVal = T;
        newcontact = krange;
        baseLab = labelBuf[pos + krange];
      }
    }
    tmpLineBuf[pos] = baseVal;
    tmpLabelBuf[pos] = baseLab;
    koffset = newcontact - 1;
  }
  // positive half of parabola
  koffset = newcontact = 0;
#if 1
  for (long pos = lineLength - 1; pos >= 0; pos--)
  {
    auto baseVal = (RealType)extreme; // the base value for comparison
    // initialize the label to the previously pro
    LabelType baseLab = tmpLabelBuf[pos];
    for (long krange = koffset; krange >= 0; krange--)
    {
      RealType T = tmpLineBuf[pos + krange] - magnitude * krange * krange;
      if (doDilate ? (T >= baseVal) : (T <= baseVal))
      {
        baseVal = T;
        newcontact = krange;
        baseLab = tmpLabelBuf[pos + krange];
      }
    }
    lineBuf[pos] = baseVal;
    labelBuf[pos] = baseLab;
    koffset = newcontact + 1;
  }
#else
  for (long pos = lineLength - 1; pos >= 0; pos--)
  {
    lineBuf[pos] = tmpLineBuf[pos];
    labelBuf[pos] = tmpLabelBuf[pos];
  }

#endif
}

template <class TInIter, class TOutDistIter, class TOutLabIter, class RealType>
void
doOneDimensionErodeFirstPass(TInIter &          inputIterator,
                             TOutDistIter &     outputIterator,
                             TOutLabIter &      outputLabIterator,
                             ProgressReporter & progress,
                             const unsigned     lineLength,
                             const unsigned     direction,
                             const int          magnitudeSign,
                             const bool         useImageSpacing,
                             const RealType     imageScale,
                             const RealType     sigma,
                             const bool         lastpass)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (useImageSpacing)
  {
    iscale = imageScale;
  }
  // restructure equation to reduce numerical error
  //  const RealType magnitude = (magnitudeSign * iscale * iscale)/(2.0 *
  // sigma);
  const RealType  magnitude = (magnitudeSign * iscale * iscale) / (2.0);
  LineBufferType  lineBuf(lineLength);
  LabelBufferType labBuf(lineLength);

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
      labBuf[i] = (inputIterator.Get());
      if (labBuf[i])
      {
        lineBuf[i] = 1.0;
      }
      ++i;
      ++inputIterator;
    }
    // runlength encode the line buffer (could be integrated with extraction)

    using EndType = std::vector<unsigned>;
    EndType firsts;
    EndType lasts;

    for (unsigned idx = 0; idx < lineLength; idx++)
    {
      RealType val = labBuf[idx];
      if (val != 0)
      {
        // found a run
        firsts.push_back(idx);
        unsigned idxend = idx;
        for (; idxend < lineLength; idxend++)
        {
          if (val != labBuf[idxend])
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
        leftend = sigma;
      }
      if (last == lineLength - 1)
      {
        rightend = sigma;
      }

      DoLineErodeFirstPass<LineBufferType, RealType>(ShortLineBuf, leftend, rightend, magnitude, sigma);
      // copy the segment back into the full line buffer
      std::copy(ShortLineBuf.begin(), ShortLineBuf.end(), &(lineBuf[first]));
    }
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputIterator.IsAtEndOfLine())
    {
      outputIterator.Set(static_cast<typename TOutDistIter::PixelType>(lineBuf[j++]));
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
        if (lineBuf[j2] == sigma)
        {
          val = labBuf[j2];
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
                              const unsigned     lineLength,
                              const unsigned     direction,
                              const int          magnitudeSign,
                              const bool         useImageSpacing,
                              const RealType     imageScale,
                              const RealType     sigma)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (useImageSpacing)
  {
    iscale = imageScale;
  }
  // restructure equation to reduce numerical error
  // const RealType magnitude = (magnitudeSign * iscale * iscale)/(2.0 *
  // sigma);
  const RealType  magnitude = (magnitudeSign * iscale * iscale) / (2.0);
  LineBufferType  lineBuf(lineLength);
  LabelBufferType labBuf(lineLength);
  LineBufferType  tmpLineBuf(lineLength);
  LabelBufferType newLabBuf(lineLength);

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
      labBuf[i] = (inputIterator.Get());
      if (labBuf[i])
      {
        lineBuf[i] = sigma;
      }
      else
      {
        lineBuf[i] = 0;
      }
      ++i;
      ++inputIterator;
    }

    DoLineDilateFirstPass<LineBufferType, LabelBufferType, RealType>(lineBuf, tmpLineBuf, labBuf, newLabBuf, magnitude);
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputIterator.IsAtEndOfLine())
    {
      outputIterator.Set(static_cast<typename TOutDistIter::PixelType>(lineBuf[j]));
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
                    const unsigned     lineLength,
                    const unsigned     direction,
                    const int          magnitudeSign,
                    const bool         useImageSpacing,
                    const RealType     extreme,
                    const RealType     imageScale,
                    const RealType     sigma,
                    const RealType     BaseSigma,
                    const bool         lastpass)
{
  // traditional erosion - can't optimise the same way as the first pass
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (useImageSpacing)
  {
    iscale = imageScale;
  }
  const RealType  magnitude = (magnitudeSign * iscale * iscale) / (2.0 * sigma);
  LineBufferType  lineBuf(lineLength);
  LabelBufferType labBuf(lineLength);

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
      lineBuf[i] = static_cast<RealType>(inputDistIterator.Get());
      labBuf[i] = inputIterator.Get();
      ++i;
      ++inputDistIterator;
      ++inputIterator;
    }
    // runlength encode the line buffer (could be integrated with extraction)
    using EndType = std::vector<unsigned>;
    EndType firsts;
    EndType lasts;
    for (unsigned idx = 0; idx < lineLength; idx++)
    {
      RealType val = labBuf[idx];
      if (val != 0)
      {
        // found a run
        firsts.push_back(idx);
        unsigned idxend = idx;
        for (; idxend < lineLength; idxend++)
        {
          if (val != labBuf[idxend])
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
      if (last == lineLength - 1)
      {
        rightend = BaseSigma;
      }

      ShortLineBuf[0] = leftend;
      ShortLineBuf[SLL + 1] = rightend;

      std::copy(&(lineBuf[first]), &(lineBuf[last + 1]), &(ShortLineBuf[1]));

      DoLine<LineBufferType, RealType, false>(ShortLineBuf, tmpShortLineBuf, magnitude, extreme);
      // copy the segment back into the full line buffer
      std::copy(&(ShortLineBuf[1]), &(ShortLineBuf[SLL + 1]), &(lineBuf[first]));
    }
    // copy the line buffer back to the image - don't need to do it on
    // the last pass - move when we are sure it is working
    unsigned j = 0;
    while (!outputDistIterator.IsAtEndOfLine())
    {
      outputDistIterator.Set(static_cast<typename TOutDistIter::PixelType>(lineBuf[j++]));
      ++outputDistIterator;
    }

    if (lastpass)
    {
      unsigned j2 = 0;
      while (!outputLabIterator.IsAtEndOfLine())
      {
        typename TInIter::PixelType val = 0;
        if (lineBuf[j2] == BaseSigma)
        {
          val = labBuf[j2];
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
                     const unsigned     lineLength,
                     const unsigned     direction,
                     const int          magnitudeSign,
                     const bool         useImageSpacing,
                     const RealType     extreme,
                     const RealType     imageScale,
                     const RealType     sigma)
{
  // specialised version for binary erosion during first pass. We can
  // compute the results directly because the inputs are flat.
  using LineBufferType = typename itk::Array<RealType>;
  using LabelBufferType = typename itk::Array<typename TInIter::PixelType>;
  RealType iscale = 1.0;
  if (useImageSpacing)
  {
    iscale = imageScale;
  }
  // restructure equation to reduce numerical error
  const RealType magnitude = (magnitudeSign * iscale * iscale) / (2.0 * sigma);
  //  const RealType magnitude = (magnitudeSign * iscale * iscale)/(2.0 );
  LineBufferType  lineBuf(lineLength);
  LabelBufferType labBuf(lineLength);
  LineBufferType  tmpLineBuf(lineLength);
  LabelBufferType newLabBuf(lineLength);
  LabelBufferType tmpLabBuf(lineLength);

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
      lineBuf[i] = inputDistIterator.Get();
      labBuf[i] = inputIterator.Get();
      ++i;
      ++inputIterator;
      ++inputDistIterator;
    }

    DoLineLabelProp<LineBufferType, LabelBufferType, RealType, true>(
      lineBuf, tmpLineBuf, labBuf, tmpLabBuf, magnitude, extreme);
    // copy the line buffer back to the image
    unsigned j = 0;
    while (!outputDistIterator.IsAtEndOfLine())
    {
      outputDistIterator.Set(static_cast<typename TOutDistIter::PixelType>(lineBuf[j]));
      outputLabIterator.Set(labBuf[j]);
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
