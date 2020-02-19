#ifndef _multilabelDilation_h
#define _multilabelDilation_h

#include "itkinstance.h"

#include <itkMaskImageFilter.h>
#include <itkBinaryThresholdImageFilter.h>
#include <itkSignedMaurerDistanceMapImageFilter.h>
#include <itkDanielssonDistanceMapImageFilter.h>
#include <itkMorphologicalWatershedFromMarkersImageFilter.h>


// ITK code for my watershed based algorithm.
// based on a version from Dzenan Zukic courtesy of
// changes are to toss the binary dilate step and threshold the distance transform instead
template <class LabelImageType>
typename LabelImageType::Pointer
multilabelDilation(typename LabelImageType::Pointer LabelIm, float radius)
{
  using MaskImageType = typename itk::Image<unsigned char, LabelImageType::ImageDimension>;
  using InternalImageType = typename itk::Image<double, LabelImageType::ImageDimension>;

  // I think we need to do this to get the right form for Maurer
  itk::Instance<itk::BinaryThresholdImageFilter<LabelImageType, MaskImageType>> Thresh;
  Thresh->SetInput(LabelIm);
  Thresh->SetUpperThreshold(0);
  Thresh->SetInsideValue(0);
  Thresh->SetOutsideValue(1);


  using DistanceMapType = typename itk::SignedMaurerDistanceMapImageFilter<MaskImageType, InternalImageType>;

  typename DistanceMapType::Pointer dm = DistanceMapType::New();
  dm->SetInput(Thresh->GetOutput());
  dm->SetUseImageSpacing(true);
  dm->InsideIsPositiveOff();
  dm->SquaredDistanceOn();

  // Get our dilation by thresholding the distance map
  itk::Instance<itk::BinaryThresholdImageFilter<InternalImageType, MaskImageType>> Dilate;
  Dilate->SetInput(dm->GetOutput());
  Dilate->SetUpperThreshold(radius * radius);
  Dilate->SetInsideValue(1);
  Dilate->SetOutsideValue(0);

  // writeIm<InternalImageType>(dm->GetOutput(), "/tmp/maurer.nii.gz");

  using morphoWSfMType = typename itk::MorphologicalWatershedFromMarkersImageFilter<InternalImageType, LabelImageType>;
  typename morphoWSfMType::Pointer ws = morphoWSfMType::New();
  ws->SetInput1(dm->GetOutput());
  ws->SetInput2(LabelIm);
  ws->SetMarkWatershedLine(false);
  ws->SetFullyConnected(false);
  ws->Update();

  using MaskType = typename itk::MaskImageFilter<LabelImageType, MaskImageType>;
  typename MaskType::Pointer mask = MaskType::New();
  mask->SetInput1(ws->GetOutput());
  mask->SetInput2(Dilate->GetOutput());
  mask->Update();
  // writeIm<LabelImageType>(mask->GetOutput(), "4multiplied.nrrd"); //debug

  typename LabelImageType::Pointer result = mask->GetOutput();
  result->Update();
  result->DisconnectPipeline();
  return result;
}

// Alternative version that uses the voronoi tesselation produced by
// the Danielsson filter.

template <class LabelImageType>
typename LabelImageType::Pointer
multilabelDilationDanielsson(typename LabelImageType::Pointer LabelIm, float radius)
{
  using MaskImageType = typename itk::Image<unsigned char, LabelImageType::ImageDimension>;
  using InternalImageType = typename itk::Image<double, LabelImageType::ImageDimension>;

  using DistanceMapType = typename itk::DanielssonDistanceMapImageFilter<LabelImageType, InternalImageType>;


  typename DistanceMapType::Pointer dm = DistanceMapType::New();
  //  dm->SetInput(Thresh->GetOutput());
  dm->SetInput(LabelIm);
  dm->SetUseImageSpacing(true);
  dm->SquaredDistanceOn();

  itk::Instance<itk::BinaryThresholdImageFilter<InternalImageType, MaskImageType>> Thresh;
  Thresh->SetInput(dm->GetOutput());
  Thresh->SetUpperThreshold(radius * radius);
  Thresh->SetInsideValue(1);
  Thresh->SetOutsideValue(0);

  // writeIm<InternalImageType>(dm->GetOutput(), "/tmp/dan.nii.gz");
  using MaskType = typename itk::MaskImageFilter<LabelImageType, MaskImageType>;
  typename MaskType::Pointer mask = MaskType::New();
  mask->SetInput1(dm->GetVoronoiMap());
  mask->SetInput2(Thresh->GetOutput());

  typename LabelImageType::Pointer result = mask->GetOutput();
  result->Update();
  result->DisconnectPipeline();
  return result;
}


#endif
