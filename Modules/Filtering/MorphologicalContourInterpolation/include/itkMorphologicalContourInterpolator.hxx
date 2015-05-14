#ifndef __itkMorphologicalContourInterpolator_hxx
#define __itkMorphologicalContourInterpolator_hxx

#include "itkMorphologicalContourInterpolator.h"
#include "itkObjectFactory.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <utility>

namespace itk
{
template <class TImage>
void
MorphologicalContourInterpolator<TImage>::ExpandRegion(typename TImage::RegionType & region,
                                                       typename TImage::IndexType    index)
{
  for (int a = 0; a < TImage::ImageDimension; a++)
  {
    if (region.GetIndex(a) > index[a])
      region.SetIndex(a, index[a]);
    else if (region.GetIndex(a) + region.GetSize(a) < index[a])
      region.SetSize(a, index[a] - region.GetIndex(a) + 1);
    // else it is already within
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::DetermineSliceOrientations()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  m_Orientations.clear();
  typename TImage::RegionType                    region = output->GetRequestedRegion();
  itk::ImageRegionConstIteratorWithIndex<TImage> it(input, region);

  OrientationType zeros = OrientationType();

  while (!it.IsAtEnd())
  {
    typename TImage::IndexType indPrev, indNext, ind = it.GetIndex();
    typename TImage::PixelType val = input->GetPixel(ind);
    if (val != 0 || (m_Label != 0 && val == m_Label))
    {
      typename TImage::RegionType bb1;
      bb1.SetIndex(ind);
      for (int a = 0; a < TImage::ImageDimension; a++)
        bb1.SetSize(a, 1);
      std::pair<BoundingBoxesType::iterator, bool> resBB = m_BoundingBoxes.insert(std::make_pair(val, bb1));
      if (!resBB.second) // include this index in existing BB
        ExpandRegion(resBB.first->second, ind);

      std::pair<OrientationsType::iterator, bool> res = m_Orientations.insert(std::make_pair(val, zeros));
      OrientationsType::iterator                  oRef = res.first;

      for (int a = 0; a < TImage::ImageDimension; a++)
      {
        indPrev = ind;
        indPrev[a]--;
        indNext = ind;
        indNext[a]++;
        if (input->GetLargestPossibleRegion().IsInside(indPrev) && input->GetLargestPossibleRegion().IsInside(indNext))
        {
          if (input->GetPixel(indPrev) == 0 && input->GetPixel(indNext) == 0)
            oRef->second[a]++;
        }
      }
    }
    ++it;
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateAlong(int axis, typename TImage::Pointer out)
{}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  this->AllocateOutputs();

  if (m_Axis == -1)
  {
    this->DetermineSliceOrientations();
    // do more computation
    // invoke interpolation along all axes then do median voting
  }
  else
    InterpolateAlong(m_Axis, output);
}
} // namespace itk


#endif //__itkMorphologicalContourInterpolator_hxx
