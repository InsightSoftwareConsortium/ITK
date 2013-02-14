#ifndef _multilabelDilation_h
#define _multilabelDilation_h

#include "itkinstance.h"

#include <itkMaskImageFilter.h>
#include <itkBinaryThresholdImageFilter.h>
#include <itkSignedMaurerDistanceMapImageFilter.h>
#include <itkDanielssonDistanceMapImageFilter.h>
#include <itkMorphologicalWatershedFromMarkersImageFilter.h>
#include <itkWatershedImageFilter.h>

// ITK code for my watershed based algorithm.
// based on a version from Dzenan Zukic courtesy of
// changes are to toss the binary dilate step and threshold the distance transform instead
template <class LabelImageType>
typename LabelImageType::Pointer multilabelDilation(typename LabelImageType::Pointer LabelIm, float radius)
{
  typedef typename itk::Image<unsigned char, LabelImageType::ImageDimension> MaskImageType;
  typedef typename itk::Image<float, LabelImageType::ImageDimension> InternalImageType;

  // I think we need to do this to get the right form for Maurer
  itk::Instance<itk::BinaryThresholdImageFilter<LabelImageType, MaskImageType> > Thresh;
  Thresh->SetInput(LabelIm);
  Thresh->SetUpperThreshold(0);
  Thresh->SetInsideValue(0);
  Thresh->SetOutsideValue(1);


  typedef typename itk::SignedMaurerDistanceMapImageFilter<MaskImageType, InternalImageType> DistanceMapType;

  typename DistanceMapType::Pointer dm=DistanceMapType::New();
  dm->SetInput(Thresh->GetOutput());
  dm->SetUseImageSpacing(true);
  dm->InsideIsPositiveOff();

  // Get our dilation by thresholding the distance map
  itk::Instance<itk::BinaryThresholdImageFilter<InternalImageType, MaskImageType> > Dilate;
  Dilate->SetInput(dm->GetOutput());
  Dilate->SetUpperThreshold(radius);
  Dilate->SetInsideValue(1);
  Dilate->SetOutsideValue(0);

  typedef typename itk::MorphologicalWatershedFromMarkersImageFilter<InternalImageType, LabelImageType> morphoWSfMType;
  typename morphoWSfMType::Pointer ws=morphoWSfMType::New();
  ws->SetInput1(dm->GetOutput());
  ws->SetInput2(LabelIm);
  ws->SetMarkWatershedLine(false);
  ws->Update();
  
  typedef typename itk::MaskImageFilter<LabelImageType, MaskImageType> MaskType;
  typename MaskType::Pointer mask=MaskType::New();
  mask->SetInput1(ws->GetOutput());
  mask->SetInput2(Dilate->GetOutput());
  mask->Update();

  typedef itk::WatershedImageFilter<InternalImageType> wsType;
  wsType::Pointer ws2=wsType::New();
  ws2->SetThreshold(0);
  ws2->SetLevel(0);
  ws2->SetInput(dm->GetOutput());
  ws2->Update();
  typedef itk::CastImageFilter<wsType::OutputImageType, LabelImageType> castType;
  castType::Pointer caster=castType::New();
  caster->SetInput(ws2->GetOutput());
  caster->Update();
  //writeIm<LabelImageType>(caster->GetOutput(), "C:/temp/debugWS_noMask.mha"); //debug
  //writeIm<MaskImageType>(Dilate->GetOutput(), "C:/temp/debugWS_Dilate.mha"); //debug
  MaskType::Pointer masker2=MaskType::New();
  masker2->SetInput1(caster->GetOutput());
  masker2->SetInput2(Dilate->GetOutput());
  masker2->Update();
  writeIm<MaskImageType>(masker2->GetOutput(), "C:/temp/debugWS.mha"); //debug

  //typedef typename itk::MaskImageFilter<InternalImageType, MaskImageType> MaskTypeDM;
  //typename MaskTypeDM::Pointer maskDM=MaskTypeDM::New();
  //maskDM->SetInput1(dm->GetOutput());
  //maskDM->SetInput2(Dilate->GetOutput());
  //maskDM->Update();
  //writeIm<InternalImageType>(maskDM->GetOutput(), "C:/temp/debugDM.mha"); //debug

  typename LabelImageType::Pointer result = mask->GetOutput();
  result->Update();
  result->DisconnectPipeline();
  return result;
}

// Alternative version that uses the voronoi tesselation produced by
// the Danielsson filter.

template <class LabelImageType>
typename LabelImageType::Pointer multilabelDilationDanielsson(typename LabelImageType::Pointer LabelIm, float radius)
{
  typedef typename itk::Image<unsigned char, LabelImageType::ImageDimension> MaskImageType;
  typedef typename itk::Image<float, LabelImageType::ImageDimension> InternalImageType;

  typedef typename itk::DanielssonDistanceMapImageFilter<LabelImageType, InternalImageType> DistanceMapType;


  typename DistanceMapType::Pointer dm=DistanceMapType::New();
  //  dm->SetInput(Thresh->GetOutput());
  dm->SetInput(LabelIm);
  dm->SetUseImageSpacing(true);

  itk::Instance<itk::BinaryThresholdImageFilter<InternalImageType, MaskImageType> > Thresh;
  Thresh->SetInput(dm->GetOutput());
  Thresh->SetUpperThreshold(radius);
  Thresh->SetInsideValue(1);
  Thresh->SetOutsideValue(0);


  typedef typename itk::MaskImageFilter<LabelImageType, MaskImageType> MaskType;
  typename MaskType::Pointer mask=MaskType::New();
  mask->SetInput1(dm->GetVoronoiMap());
  mask->SetInput2(Thresh->GetOutput());

  typename LabelImageType::Pointer result = mask->GetOutput();
  result->Update();
  result->DisconnectPipeline();
  return result;
}


#endif
