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
    if (val != 0)
    {
      std::pair<OrientationsType::iterator, bool> res = m_Orientations.insert(make_pair(val, zeros));
      OrientationsType::iterator                  oRef = res.first;

      for (int a = 0; a < TImage::ImageDimension; a++)
      {
        indPrev = ind;
        indPrev[a]--;
        indNext = ind;
        indNext[a]++;
        if (region.IsInside(indPrev) && region.IsInside(indNext))
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
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  this->AllocateOutputs();

  this->DetermineSliceOrientations();

  ImageAlgorithm::Copy(
    input.GetPointer(), output.GetPointer(), output->GetRequestedRegion(), output->GetRequestedRegion());

  itk::Index<3>              cornerPixel = input->GetLargestPossibleRegion().GetIndex();
  typename TImage::PixelType newValue = 3;

  output->SetPixel(cornerPixel, newValue);
}
} // namespace itk


#endif //__itkMorphologicalContourInterpolator_hxx
