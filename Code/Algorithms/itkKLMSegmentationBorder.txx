/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{

template<class TInputImage, class TOutputImage>
KLMSegmentationBorder<TInputImage,TOutputImage>
::KLMSegmentationBorder(void)
{

}

template<class TInputImage, class TOutputImage>
KLMSegmentationBorder<TInputImage,TOutputImage>
::~KLMSegmentationBorder()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
KLMSegmentationBorder<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region border KLM object" << std::endl;

}// end PrintSelf

template<class TInputImage, class TOutputImage>
void
KLMSegmentationBorder<TInputImage,TOutputImage>
::SetRegion1(KLMSegmentationRegion<TInputImage,TOutputImage> *Region1)
{

  m_Region1 = Region1; 

}// end SetRegion1

template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage> *
KLMSegmentationBorder<TInputImage,TOutputImage>
::GetRegion1()
{

  return m_Region1; 

}// end GetRegion2

template<class TInputImage, class TOutputImage>
void
KLMSegmentationBorder<TInputImage,TOutputImage>
::SetRegion2(KLMSegmentationRegion<TInputImage,TOutputImage> *Region2)
{

  m_Region2 = Region2; 

}// end SetRegion2

template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage> *
KLMSegmentationBorder<TInputImage,TOutputImage>
::GetRegion2()
{

  return m_Region2; 

}// end GetRegion2

template<class TInputImage, class TOutputImage>
void
KLMSegmentationBorder<TInputImage,TOutputImage>
::EvaluateLambda()
{
  // Get the regions corresponding to the border in question
  KLMSegmentationRegion<TInputImage,TOutputImage> *preg1 = this->GetRegion1();
  KLMSegmentationRegion<TInputImage,TOutputImage> *preg2 = this->GetRegion2();

  VecDblType region1Mean = preg1->GetMeanRegionIntensity();
  VecDblType region2Mean = preg2->GetMeanRegionIntensity();
  VecDblType region1_2MeanDiff = region1Mean - region2Mean;

  // Eventhough this implementation uses the definition of a 
  // Matrix in reality it is a vector, hence number of colums
  // is 1.
  int numRows = region1_2MeanDiff.rows();
  for( int i = 0; i < numRows; i++ )
    region1_2MeanDiff[i][0] *= region1_2MeanDiff[i][0];

  int region1Area = preg1->GetRegionArea();
  int region2Area = preg2->GetRegionArea();
 
  double scaleArea = (( double )( region1Area * region2Area ) /
    ( double )( region1Area + region2Area ) );

  VecDblType LambdaMat = 
    (scaleArea / this->GetBorderLength() ) * region1_2MeanDiff;
                         

  // Assuming equal weights to all the channels
  // FIXME: For different channel weights modify this part of the
  // code.
  m_Lambda = 0.0;
  for( int i = 0; i < numRows; i++ ) m_Lambda += LambdaMat[i][0];

}//end EvaluateLambda()

template<class TInputImage, class TOutputImage>
void
KLMSegmentationBorder<TInputImage,TOutputImage>
::PrintBorderInfo()
{

  std::cout << "------------------------------" << std::endl;
  std::cout << "Location      : " << this << std::endl;
  std::cout << "Lambda        : " << m_Lambda << std::endl;
  std::cout << "Region1       : " << this->GetRegion1() << std::endl;
  std::cout << "Region 1 Label: " << (this->GetRegion1()->GetRegionLabel()) 
                                                            << std::endl;
  std::cout << "Region2       : " << this->GetRegion2() << std::endl;
  std::cout << "Region 2 Label: " << (this->GetRegion2()->GetRegionLabel()) 
                                                            << std::endl;
  std::cout << "++++++++++++++++++++++++++++++" << std::endl;
          
  std::cout << "------------------------------" << std::endl;
  std::cout << "------------------------------" << std::endl;

}//end PrintBorderResults


} // namespace itk










