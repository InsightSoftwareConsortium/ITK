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
#ifndef itkVoronoiSegmentationImageFilterBase_hxx
#define itkVoronoiSegmentationImageFilterBase_hxx


#include "itkImageRegionIteratorWithIndex.h"
#include "itkVoronoiDiagram2DGenerator.h"
#include "itkPrintHelper.h"
#include <cmath>

namespace itk
{

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::VoronoiSegmentationImageFilterBase()
  : m_WorkingVD(VoronoiDiagram::New())
  , m_VDGenerator(VoronoiDiagramGenerator::New())
{
  m_Size.Fill(0);
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::PrintSelf(std::ostream & os,
                                                                                            Indent         indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "Size: " << static_cast<typename NumericTraits<SizeType>::PrintType>(m_Size) << std::endl;
  os << indent << "NumberOfSeeds: " << m_NumberOfSeeds << std::endl;
  os << indent << "MinRegion: " << static_cast<typename NumericTraits<SizeValueType>::PrintType>(m_MinRegion)
     << std::endl;
  os << indent << "Steps: " << m_Steps << std::endl;
  os << indent << "LastStepSeeds: " << m_LastStepSeeds << std::endl;
  os << indent << "NumberOfSeedsToAdded: " << m_NumberOfSeedsToAdded << std::endl;
  os << indent << "NumberOfBoundary: " << m_NumberOfBoundary << std::endl;

  os << indent << "NumberOfPixels: " << m_NumberOfPixels << std::endl;
  os << indent << "Label: " << m_Label << std::endl;

  os << indent << "MeanDeviation: " << m_MeanDeviation << std::endl;
  itkPrintSelfBooleanMacro(UseBackgroundInAPrior);
  itkPrintSelfBooleanMacro(OutputBoundary);

  itkPrintSelfBooleanMacro(InteractiveSegmentation);

  itkPrintSelfObjectMacro(WorkingVD);
  itkPrintSelfObjectMacro(VDGenerator);

  os << indent << "SeedsToAdded: " << m_SeedsToAdded << std::endl;
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::GetPixelIndexFromPolygon(
  PointTypeDeque vertlist,
  IndexList *    PixelPool)
{
  PointType currP = vertlist.front();
  vertlist.pop_front();
  PointType leftP = vertlist.front();
  while (currP[1] > leftP[1])
  {
    vertlist.push_back(currP);
    currP = vertlist.front();
    vertlist.pop_front();
    leftP = vertlist.front();
  }
  PointType rightP = vertlist.back();
  while (currP[1] > rightP[1])
  {
    vertlist.push_front(currP);
    currP = vertlist.back();
    vertlist.pop_back();
    rightP = vertlist.back();
  }
  leftP = vertlist.front();
  PointTypeDeque tmpQ;
  tmpQ.clear();
  if (leftP[0] > rightP[0])
  {
    while (!(vertlist.empty()))
    {
      tmpQ.push_back(vertlist.front());
      vertlist.pop_front();
    }
    while (!(tmpQ.empty()))
    {
      vertlist.push_front(tmpQ.front());
      tmpQ.pop_front();
    }
  }
  tmpQ.clear();
  leftP = vertlist.front();
  rightP = vertlist.back();

  double leftendy = leftP[1];
  double rightendy = rightP[1];
  double endy = (leftendy > rightendy) ? rightendy : leftendy;
  bool   RorL = (leftendy > rightendy) ? true : false;

  double beginx = currP[0];
  double beginy = currP[1];
  double endx = currP[0];
  double leftDx = (Math::AlmostEquals(leftP[1], beginy)) ? 1.0 : (leftP[0] - beginx) / (leftP[1] - beginy);
  double rightDx = (Math::AlmostEquals(rightP[1], beginy)) ? 1.0 : (rightP[0] - endx) / (rightP[1] - beginy);

  double leftheadx = beginx;
  double rightheadx = endx;
  double leftheady = beginy;
  double rightheady = beginy;

  auto      intbeginy = static_cast<int>(std::ceil(beginy));
  IndexType idx;
  idx[1] = intbeginy;
  auto intendy = static_cast<int>(std::floor(endy));
  if (intbeginy > intendy)
  { // no scanline
    if (RorL)
    {
      beginy = rightP[1];
    }
    else
    {
      beginy = leftP[1];
    }
  }
  else if ((intbeginy == intendy) && (intbeginy == 0))
  { // only one scanline at 0;
    if (RorL)
    {
      endx = rightP[0];
    }
    else
    {
      beginx = leftP[0];
    }
    for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
    {
      idx[0] = i;
      PixelPool->push_back(idx);
    }
    idx[1] = idx[1] + 1;
  }
  else
  { // normal case some scanlines
    const double offset = static_cast<double>(intbeginy) - beginy;
    endx += offset * rightDx;
    beginx += offset * leftDx;
    while (idx[1] <= intendy)
    {
      for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
      {
        idx[0] = i;
        PixelPool->push_back(idx);
      }
      endx += rightDx;
      beginx += leftDx;
      idx[1] = idx[1] + 1;
    }
    beginy = endy;
  }

  auto vsize = static_cast<int>(vertlist.size());
  while (vsize > 2)
  {
    --vsize;
    if (RorL)
    {
      vertlist.pop_back();
      currP = rightP;
      rightheadx = currP[0];
      rightheady = currP[1];
      endx = currP[0];
      beginx = leftheadx + leftDx * (beginy - leftheady);
      rightP = vertlist.back();
      if (Math::AlmostEquals(rightP[1], currP[1]))
      {
        rightDx = 1.0;
      }
      else
      {
        rightDx = (rightP[0] - currP[0]) / (rightP[1] - currP[1]);
      }
    }
    else
    {
      vertlist.pop_front();
      currP = leftP;
      leftheadx = currP[0];
      leftheady = currP[1];
      beginx = currP[0];
      endx = rightheadx + rightDx * (beginy - rightheady);
      leftP = vertlist.front();
      if (Math::AlmostEquals(leftP[1], currP[1]))
      {
        leftDx = 1.0;
      }
      else
      {
        leftDx = (leftP[0] - currP[0]) / (leftP[1] - currP[1]);
      }
    }

    leftendy = leftP[1];
    rightendy = rightP[1];
    if (leftendy > rightendy)
    {
      RorL = true;
      endy = rightendy;
    }
    else
    {
      RorL = false;
      endy = leftendy;
    }

    intendy = static_cast<int>(std::floor(endy));
    intbeginy = static_cast<int>(std::ceil(beginy));

    if (intbeginy > intendy)
    { // no scanline
      if (RorL)
      {
        beginy = rightP[1];
      }
      else
      {
        beginy = leftP[1];
      }
    }
    else
    { // normal case some scanlines
      const double offset = static_cast<double>(intbeginy) - beginy;
      endx += offset * rightDx;
      beginx += offset * leftDx;
      while (idx[1] <= intendy)
      {
        for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
        {
          idx[0] = i;
          PixelPool->push_back(idx);
        }
        endx += rightDx;
        beginx += leftDx;
        idx[1] = idx[1] + 1;
      }
      beginy = idx[1];
    }
  }

  if (RorL)
  {
    beginy = rightP[1];
    endy = leftP[1];
  }
  else
  {
    beginy = leftP[1];
    endy = rightP[1];
  }
  intbeginy = static_cast<int>(std::ceil(beginy));
  intendy = static_cast<int>(std::floor(endy));
  if (intbeginy <= intendy)
  {
    if (RorL)
    {
      if (Math::AlmostEquals(rightP[1], leftP[1]))
      {
        rightDx = 1.0;
      }
      else
      {
        rightDx = (rightP[0] - leftP[0]) / (rightP[1] - leftP[1]);
      }
      endx = rightP[0];
      beginx = leftP[0] + leftDx * (rightP[1] - leftP[1]);
    }
    else
    {
      if (Math::AlmostEquals(rightP[1], leftP[1]))
      {
        leftDx = 1.0;
      }
      else
      {
        leftDx = (rightP[0] - leftP[0]) / (rightP[1] - leftP[1]);
      }
      beginx = leftP[0];
      endx = rightP[0] + rightDx * (leftP[1] - rightP[1]);
    }
    const double offset = static_cast<double>(intbeginy) - beginy;
    beginx += offset * leftDx;
    endx += offset * rightDx;
    while (idx[1] <= intendy)
    {
      for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
      {
        idx[0] = i;
        PixelPool->push_back(idx);
      }
      endx += rightDx;
      beginx += leftDx;
      idx[1] = idx[1] + 1;
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::ClassifyDiagram()
{
  for (int i = 0; i < m_NumberOfSeeds; ++i)
  {
    CellAutoPointer currCell;
    m_WorkingVD->GetCellId(i, currCell);
    PointIdIterator currPitEnd = currCell->PointIdsEnd();
    PointTypeDeque  VertList;
    for (PointIdIterator currPit = currCell->PointIdsBegin(); currPit != currPitEnd; ++currPit)
    {
      PointType currP;
      m_WorkingVD->GetPoint((*currPit), &(currP));
      VertList.push_back(currP);
    }
    IndexList PixelPool;
    this->GetPixelIndexFromPolygon(VertList, &PixelPool);
    m_NumberOfPixels[i] = static_cast<int>(PixelPool.size());
    m_Label[i] = this->TestHomogeneity(PixelPool);
  }

  m_NumberOfBoundary = 0;
  for (int i = 0; i < m_NumberOfSeeds; ++i)
  {
    // if not homogeneous
    if (m_Label[i] == 0)
    {
      auto itend = m_WorkingVD->NeighborIdsEnd(i);
      auto it = m_WorkingVD->NeighborIdsBegin(i);
      bool bnd = false;
      // and any adjacent region is homogeneous
      while ((it != itend) && (!bnd))
      {
        bnd = (m_Label[*it] == 1);
        ++it;
      }
      // then this must be a boundary region
      if (bnd)
      {
        m_Label[i] = 2;
        ++m_NumberOfBoundary;
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::GenerateAddingSeeds()
{
  auto eitend = m_WorkingVD->EdgeEnd();

  for (auto eit = m_WorkingVD->EdgeBegin(); eit != eitend; ++eit)
  {
    Point<int, 2> seeds = m_WorkingVD->GetSeedsIDAroundEdge(&*eit);
    if (((m_Label[seeds[0]] == 2) || (m_Label[seeds[1]] == 2)) && (m_NumberOfPixels[seeds[0]] > m_MinRegion) &&
        (m_NumberOfPixels[seeds[1]] > m_MinRegion))
    {
      PointType adds;
      adds[0] = (eit->m_Left[0] + eit->m_Right[0]) * 0.5;
      adds[1] = (eit->m_Left[1] + eit->m_Right[1]) * 0.5;
      m_SeedsToAdded.push_back(adds);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::RunSegmentOneStep()
{
  m_NumberOfPixels.resize(m_NumberOfSeeds);
  m_Label.resize(m_NumberOfSeeds);
  m_SeedsToAdded.clear();
  m_VDGenerator->Update();
  m_WorkingVD = m_VDGenerator->GetOutput();
  this->ClassifyDiagram();
  this->GenerateAddingSeeds();
  m_NumberOfSeedsToAdded = static_cast<int>(m_SeedsToAdded.size());

  if (m_InteractiveSegmentation)
  {
    if (m_OutputBoundary)
    {
      this->MakeSegmentBoundary();
    }
    else
    {
      this->MakeSegmentObject();
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::RunSegment()
{
  bool ok = true;

  if (m_Steps == 0)
  {
    SizeValueType count = 1;
    this->RunSegmentOneStep();
    // guess at a progress
    this->UpdateProgress(static_cast<float>(count) / static_cast<float>(NumericTraits<SizeValueType>::max()));
    if (m_NumberOfBoundary == 0)
    {
      ok = false;
    }
    while ((m_NumberOfSeedsToAdded != 0) && ok)
    {
      ++count;
      m_VDGenerator->AddSeeds(m_NumberOfSeedsToAdded, m_SeedsToAdded.begin());
      m_LastStepSeeds = m_NumberOfSeeds;
      m_NumberOfSeeds += m_NumberOfSeedsToAdded;
      this->RunSegmentOneStep();
      // guess at a progress
      this->UpdateProgress(static_cast<float>(count) / static_cast<float>(NumericTraits<SizeValueType>::max()));
    }
  }
  else if (m_Steps == 1)
  {
    this->RunSegmentOneStep();
    this->UpdateProgress(1.0);
  }
  else
  {
    this->RunSegmentOneStep();
    if (m_Steps == 0)
    {
      this->UpdateProgress(1.0);
    }
    else
    {
      this->UpdateProgress(1.0 / static_cast<float>(m_Steps));
    }
    if (m_NumberOfBoundary == 0)
    {
      ok = false;
    }
    int i = 1;
    while ((i < m_Steps) && ok)
    {
      m_VDGenerator->AddSeeds(m_NumberOfSeedsToAdded, m_SeedsToAdded.begin());
      m_LastStepSeeds = m_NumberOfSeeds;
      m_NumberOfSeeds += m_NumberOfSeedsToAdded;
      this->RunSegmentOneStep();
      ++i;
      this->UpdateProgress(static_cast<float>(i) / static_cast<float>(m_Steps));
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::GenerateData()
{
  // Allocate the output
  this->GetOutput()->SetBufferedRegion(this->GetOutput()->GetRequestedRegion());
  this->GetOutput()->Allocate();

  // This ivar should be necessary
  m_Size = this->GetInput()->GetRequestedRegion().GetSize();

  VoronoiDiagram::PointType VDsize;
  VDsize[0] = (VoronoiDiagram::CoordinateType)(m_Size[0] - 0.1);
  VDsize[1] = (VoronoiDiagram::CoordinateType)(m_Size[1] - 0.1);
  m_VDGenerator->SetBoundary(VDsize);
  m_VDGenerator->SetRandomSeeds(m_NumberOfSeeds);

  this->RunSegment();
  if (m_OutputBoundary)
  {
    this->MakeSegmentBoundary();
  }
  else
  {
    this->MakeSegmentObject();
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::BeforeNextStep()
{
  m_VDGenerator->AddSeeds(m_NumberOfSeedsToAdded, m_SeedsToAdded.begin());
  m_LastStepSeeds = m_NumberOfSeeds;
  m_NumberOfSeeds += m_NumberOfSeedsToAdded;
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::MakeSegmentBoundary()
{
  const RegionType region = this->GetInput()->GetRequestedRegion();

  itk::ImageRegionIteratorWithIndex<OutputImageType> oit(this->GetOutput(), region);
  while (!oit.IsAtEnd())
  {
    oit.Set(0);
    ++oit;
  }

  for (int i = 0; i < m_NumberOfSeeds; ++i)
  {
    if (m_Label[i] == 2)
    {
      const auto nitend = m_WorkingVD->NeighborIdsEnd(i);
      for (auto nit = m_WorkingVD->NeighborIdsBegin(i); nit != nitend; ++nit)
      {
        if (((*nit) > i) && (m_Label[*nit] == 2))
        {
          drawLine(m_WorkingVD->GetSeed(i), m_WorkingVD->GetSeed(*nit));
        }
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::MakeSegmentObject()
{
  const RegionType region = this->GetInput()->GetRequestedRegion();

  itk::ImageRegionIteratorWithIndex<OutputImageType> oit(this->GetOutput(), region);
  while (!oit.IsAtEnd())
  {
    oit.Set(0);
    ++oit;
  }

  for (int i = 0; i < m_NumberOfSeeds; ++i)
  {
    if (m_Label[i] == 1)
    {
      CellAutoPointer currCell;
      m_WorkingVD->GetCellId(i, currCell);
      PointIdIterator currPitEnd = currCell->PointIdsEnd();
      PointTypeDeque  VertList;
      for (PointIdIterator currPit = currCell->PointIdsBegin(); currPit != currPitEnd; ++currPit)
      {
        PointType currP;
        m_WorkingVD->GetPoint((*currPit), &(currP));
        VertList.push_back(currP);
      }
      FillPolygon(VertList);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::FillPolygon(PointTypeDeque  vertlist,
                                                                                              OutputPixelType color)
{
  TOutputImage * output = this->GetOutput();

  PointType currP = vertlist.front();
  vertlist.pop_front();
  PointType leftP = vertlist.front();
  while (currP[1] > leftP[1])
  {
    vertlist.push_back(currP);
    currP = vertlist.front();
    vertlist.pop_front();
    leftP = vertlist.front();
  }
  PointType rightP = vertlist.back();
  while (currP[1] > rightP[1])
  {
    vertlist.push_front(currP);
    currP = vertlist.back();
    vertlist.pop_back();
    rightP = vertlist.back();
  }
  leftP = vertlist.front();
  PointTypeDeque tmpQ;
  tmpQ.clear();
  if (leftP[0] > rightP[0])
  {
    while (!(vertlist.empty()))
    {
      tmpQ.push_back(vertlist.front());
      vertlist.pop_front();
    }
    while (!(tmpQ.empty()))
    {
      vertlist.push_front(tmpQ.front());
      tmpQ.pop_front();
    }
  }
  tmpQ.clear();
  leftP = vertlist.front();
  rightP = vertlist.back();

  double    beginy = currP[1];
  auto      intbeginy = static_cast<int>(std::ceil(beginy));
  IndexType idx;
  idx[1] = intbeginy;
  double leftendy = leftP[1];
  double rightendy = rightP[1];
  double beginx = currP[0];
  double endx = currP[0];

  double leftheadx = beginx;
  double rightheadx = endx;
  double leftheady = beginy;
  double rightheady = beginy;

  double endy = (leftendy > rightendy) ? rightendy : leftendy;
  bool   RorL = (leftendy > rightendy) ? true : false;

  double leftDx = (Math::AlmostEquals(leftP[1], beginy)) ? 1.0 : (leftP[0] - beginx) / (leftP[1] - beginy);
  double rightDx = (Math::AlmostEquals(rightP[1], beginy)) ? 1.0 : (rightP[0] - endx) / (rightP[1] - beginy);

  auto intendy = static_cast<int>(std::floor(endy));
  if (intbeginy > intendy)
  { // no scanline
    if (RorL)
    {
      beginy = rightP[1];
    }
    else
    {
      beginy = leftP[1];
    }
  }
  else if ((intbeginy == intendy) && (intbeginy == 0))
  { // only one scanline at 0;
    if (RorL)
    {
      endx = rightP[0];
    }
    else
    {
      beginx = leftP[0];
    }
    for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
    {
      idx[0] = i;
      output->SetPixel(idx, color);
    }
    idx[1] = idx[1] + 1;
  }
  else
  { // normal case some scanlines
    const double offset = static_cast<double>(intbeginy) - beginy;
    endx += offset * rightDx;
    beginx += offset * leftDx;
    while (idx[1] <= intendy)
    {
      for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
      {
        idx[0] = i;
        output->SetPixel(idx, color);
      }
      endx += rightDx;
      beginx += leftDx;
      idx[1] = idx[1] + 1;
    }
    beginy = endy;
  }

  auto vsize = static_cast<int>(vertlist.size());
  while (vsize > 2)
  {
    --vsize;
    if (RorL)
    {
      vertlist.pop_back();
      currP = rightP;
      rightheadx = currP[0];
      rightheady = currP[1];
      endx = currP[0];
      beginx = leftheadx + leftDx * (beginy - leftheady);
      rightP = vertlist.back();
      if (Math::AlmostEquals(rightP[1], currP[1]))
      {
        rightDx = 1.0;
      }
      else
      {
        rightDx = (rightP[0] - currP[0]) / (rightP[1] - currP[1]);
      }
    }
    else
    {
      vertlist.pop_front();
      currP = leftP;
      leftheadx = currP[0];
      leftheady = currP[1];
      beginx = currP[0];
      endx = rightheadx + rightDx * (beginy - rightheady);
      leftP = vertlist.front();
      if (Math::AlmostEquals(leftP[1], currP[1]))
      {
        leftDx = 1.0;
      }
      else
      {
        leftDx = (leftP[0] - currP[0]) / (leftP[1] - currP[1]);
      }
    }

    leftendy = leftP[1];
    rightendy = rightP[1];
    if (leftendy > rightendy)
    {
      RorL = true;
      endy = rightendy;
    }
    else
    {
      RorL = false;
      endy = leftendy;
    }

    intendy = static_cast<int>(std::floor(endy));
    intbeginy = static_cast<int>(std::ceil(beginy));

    if (intbeginy > intendy)
    { // no scanline
      if (RorL)
      {
        beginy = rightP[1];
      }
      else
      {
        beginy = leftP[1];
      }
    }
    else
    { // normal case some scanlines
      const double offset = static_cast<double>(intbeginy) - beginy;
      endx += offset * rightDx;
      beginx += offset * leftDx;
      while (idx[1] <= intendy)
      {
        for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
        {
          idx[0] = i;
          output->SetPixel(idx, color);
        }
        endx += rightDx;
        beginx += leftDx;
        idx[1] = idx[1] + 1;
      }
      beginy = idx[1];
    }
  }

  if (RorL)
  {
    beginy = rightP[1];
    endy = leftP[1];
  }
  else
  {
    beginy = leftP[1];
    endy = rightP[1];
  }
  intbeginy = static_cast<int>(std::ceil(beginy));
  intendy = static_cast<int>(std::floor(endy));
  if (intbeginy <= intendy)
  {
    if (RorL)
    {
      if (Math::AlmostEquals(rightP[1], leftP[1]))
      {
        rightDx = 1.0;
      }
      else
      {
        rightDx = (rightP[0] - leftP[0]) / (rightP[1] - leftP[1]);
      }
      endx = rightP[0];
      beginx = leftP[0] + leftDx * (rightP[1] - leftP[1]);
    }
    else
    {
      if (Math::AlmostEquals(rightP[1], leftP[1]))
      {
        leftDx = 1.0;
      }
      else
      {
        leftDx = (rightP[0] - leftP[0]) / (rightP[1] - leftP[1]);
      }
      beginx = leftP[0];
      endx = rightP[0] + rightDx * (leftP[1] - rightP[1]);
    }
    const double offset = static_cast<double>(intbeginy) - beginy;
    beginx += offset * leftDx;
    endx += offset * rightDx;
    while (idx[1] <= intendy)
    {
      for (int i = static_cast<int>(std::ceil(beginx)); i <= static_cast<int>(std::floor(endx)); ++i)
      {
        idx[0] = i;
        output->SetPixel(idx, color);
      }
      endx += rightDx;
      beginx += leftDx;
      idx[1] = idx[1] + 1;
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::drawLine(PointType p1, PointType p2)
{
  TOutputImage * output = this->GetOutput();

  auto x1 = static_cast<int>(p1[0] + 0.5);
  auto x2 = static_cast<int>(p2[0] + 0.5);
  auto y1 = static_cast<int>(p1[1] + 0.5);
  auto y2 = static_cast<int>(p2[1] + 0.5);

  if (x1 == static_cast<int>(m_Size[0]))
  {
    --x1;
  }
  if (x2 == static_cast<int>(m_Size[0]))
  {
    --x2;
  }
  if (y1 == static_cast<int>(m_Size[1]))
  {
    --y1;
  }
  if (y2 == static_cast<int>(m_Size[1]))
  {
    --y2;
  }

  int       dx = x1 - x2;
  const int adx = (dx > 0) ? dx : -dx;
  int       dy = y1 - y2;
  const int ady = (dy > 0) ? dy : -dy;


  if (adx > ady)
  {
    if (x1 > x2)
    {
      const int save = x1;
      x1 = x2;
      x2 = save;
      y1 = y2;
    }
    auto curr = static_cast<float>(y1);
    if (dx == 0)
    {
      dx = 1;
    }
    const float offset = static_cast<float>(dy) / dx;
    for (int i = x1; i <= x2; ++i)
    {
      const IndexType idx = { i, y1 };
      output->SetPixel(idx, 1);
      curr += offset;
      y1 = static_cast<int>(curr + 0.5);
    }
  }
  else
  {
    if (y1 > y2)
    {
      x1 = x2;
      const int save = y1;
      y1 = y2;
      y2 = save;
    }
    auto curr = static_cast<float>(x1);
    if (dy == 0)
    {
      dy = 1;
    }
    const float offset = static_cast<float>(dx) / dy;
    for (int i = y1; i <= y2; ++i)
    {
      const IndexType idx = { x1, i };
      output->SetPixel(idx, 1);
      curr += offset;
      x1 = static_cast<int>(curr + 0.5);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::DrawDiagram(VDImagePointer result,
                                                                                              unsigned char  incolor,
                                                                                              unsigned char  outcolor,
                                                                                              unsigned char  boundcolor)
{
  const RegionType region = this->GetInput()->GetRequestedRegion();

  itk::ImageRegionIteratorWithIndex<VDImage> vdit(result, region);
  while (!vdit.IsAtEnd())
  {
    vdit.Set(0);
    ++vdit;
  }

  EdgeIterator  eit;
  auto          eitend = m_WorkingVD->EdgeEnd();
  Point<int, 2> seeds;
  for (eit = m_WorkingVD->EdgeBegin(); eit != eitend; ++eit)
  {
    seeds = m_WorkingVD->GetSeedsIDAroundEdge(&*eit);
    if ((m_Label[seeds[0]] == 2) || (m_Label[seeds[1]] == 2))
    {
      drawVDline(result, eit->m_Left, eit->m_Right, boundcolor);
    }
    else if (m_Label[seeds[0]])
    {
      drawVDline(result, eit->m_Left, eit->m_Right, incolor);
    }
    else
    {
      drawVDline(result, eit->m_Left, eit->m_Right, outcolor);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::drawVDline(VDImagePointer result,
                                                                                             PointType      p1,
                                                                                             PointType      p2,
                                                                                             unsigned char  color)
{
  auto x1 = static_cast<int>(p1[0] + 0.5);
  auto x2 = static_cast<int>(p2[0] + 0.5);
  auto y1 = static_cast<int>(p1[1] + 0.5);
  auto y2 = static_cast<int>(p2[1] + 0.5);

  if (x1 == static_cast<int>(m_Size[0]))
  {
    --x1;
  }
  if (x2 == static_cast<int>(m_Size[0]))
  {
    --x2;
  }
  if (y1 == static_cast<int>(m_Size[1]))
  {
    --y1;
  }
  if (y2 == static_cast<int>(m_Size[1]))
  {
    --y2;
  }
  int       dx = x1 - x2;
  const int adx = (dx > 0) ? dx : -dx;
  int       dy = y1 - y2;
  const int ady = (dy > 0) ? dy : -dy;

  if (adx > ady)
  {
    if (x1 > x2)
    {
      int save = x1;
      x1 = x2;
      x2 = save;
      save = y1;
      y1 = y2;
      y2 = save;
    }
    auto curr = static_cast<float>(y1);
    if (dx == 0)
    {
      dx = 1;
    }
    const float offset = static_cast<float>(dy) / dx;
    for (int i = x1; i <= x2; ++i)
    {
      const IndexType idx{ i, y1 };
      result->SetPixel(idx, color);
      curr += offset;
      y1 = static_cast<int>(curr + 0.5);
    }
  }
  else
  {
    if (y1 > y2)
    {
      int save = x1;
      x1 = x2;
      x2 = save;
      save = y1;
      y1 = y2;
      y2 = save;
    }
    auto curr = static_cast<float>(x1);
    if (dy == 0)
    {
      dy = 1;
    }
    const float offset = static_cast<float>(dx) / dy;
    for (int i = y1; i <= y2; ++i)
    {
      const IndexType idx = { x1, i };
      result->SetPixel(idx, color);
      curr += offset;
      x1 = static_cast<int>(curr + 0.5);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::GenerateInputRequestedRegion()
{
  // call the superclass's method
  Superclass::GenerateInputRequestedRegion();

  // set the input requested region to the LargestPossibleRegion
  {
    const InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
    if (input)
    {
      input->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage>
void
VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  // call the superclass's method
  Superclass::EnlargeOutputRequestedRegion(output);

  // set the output requested region to the LargestPossibleRegion
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}
} // namespace itk

#endif
