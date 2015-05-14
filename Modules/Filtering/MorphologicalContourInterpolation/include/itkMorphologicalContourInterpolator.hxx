#ifndef __itkMorphologicalContourInterpolator_hxx
#define __itkMorphologicalContourInterpolator_hxx

#include "itkMorphologicalContourInterpolator.h"
#include "itkObjectFactory.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  this->AllocateOutputs();

  ImageAlgorithm::Copy(
    input.GetPointer(), output.GetPointer(), output->GetRequestedRegion(), output->GetRequestedRegion());

  itk::Index<3>              cornerPixel = input->GetLargestPossibleRegion().GetIndex();
  typename TImage::PixelType newValue = 3;

  output->SetPixel(cornerPixel, newValue);
}

} // namespace itk


#endif //__itkMorphologicalContourInterpolator_hxx
