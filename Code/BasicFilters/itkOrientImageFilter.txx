/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOrientImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOrientImageFilter_txx
#define __itkOrientImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkOrientImageFilter.h"
#include <itkIOCommon.h>
#include <itkCastImageFilter.h>
#include <itkConstantPadImageFilter.h>
#include <itkExtractImageFilter.h>
#include "itkMetaDataObject.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
OrientImageFilter<TInputImage, TOutputImage>
::OrientImageFilter()
  : m_GivenCoordinateOrientation  ( SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP ),
    m_DesiredCoordinateOrientation( SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP ),
    m_UseImageDirection (false)

{
  // Map between axis string labels and SpatialOrientation
  m_StringToCode["RIP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
  m_StringToCode["LIP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP;
  m_StringToCode["RSP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP;
  m_StringToCode["LSP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP;
  m_StringToCode["RIA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA;
  m_StringToCode["LIA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA;
  m_StringToCode["RSA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA;
  m_StringToCode["LSA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA;
  m_StringToCode["IRP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP;
  m_StringToCode["ILP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP;
  m_StringToCode["SRP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP;
  m_StringToCode["SLP"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP;
  m_StringToCode["IRA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA;
  m_StringToCode["ILA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA;
  m_StringToCode["SRA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA;
  m_StringToCode["SLA"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA;
  m_StringToCode["RPI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
  m_StringToCode["LPI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI;
  m_StringToCode["RAI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI;
  m_StringToCode["LAI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI;
  m_StringToCode["RPS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS;
  m_StringToCode["LPS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS;
  m_StringToCode["RAS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS;
  m_StringToCode["LAS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS;
  m_StringToCode["PRI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI;
  m_StringToCode["PLI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI;
  m_StringToCode["ARI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI;
  m_StringToCode["ALI"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI;
  m_StringToCode["PRS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS;
  m_StringToCode["PLS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS;
  m_StringToCode["ARS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS;
  m_StringToCode["ALS"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS;
  m_StringToCode["IPR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR;
  m_StringToCode["SPR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR;
  m_StringToCode["IAR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR;
  m_StringToCode["SAR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR;
  m_StringToCode["IPL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL;
  m_StringToCode["SPL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL;
  m_StringToCode["IAL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL;
  m_StringToCode["SAL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL;
  m_StringToCode["PIR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
  m_StringToCode["PSR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR;
  m_StringToCode["AIR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR;
  m_StringToCode["ASR"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR;
  m_StringToCode["PIL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL;
  m_StringToCode["PSL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL;
  m_StringToCode["AIL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL;
  m_StringToCode["ASL"] = SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL;

  // Map between axis string labels and SpatialOrientation
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP] = "RIP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP] = "LIP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP] = "RSP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP] = "LSP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA] = "RIA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA] = "LIA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA] = "RSA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA] = "LSA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP] = "IRP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP] = "ILP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP] = "SRP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP] = "SLP";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA] = "IRA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA] = "ILA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA] = "SRA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA] = "SLA";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI] = "RPI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI] = "LPI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI] = "RAI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI] = "LAI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS] = "RPS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS] = "LPS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS] = "RAS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS] = "LAS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI] = "PRI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI] = "PLI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI] = "ARI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI] = "ALI";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS] = "PRS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS] = "PLS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS] = "ARS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS] = "ALS";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR] = "IPR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR] = "SPR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR] = "IAR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR] = "SAR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL] = "IPL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL] = "SPL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL] = "IAL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL] = "SAL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR] = "PIR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR] = "PSR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR] = "AIR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR] = "ASR";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL] = "PIL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL] = "PSL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL] = "AIL";
  m_CodeToString[SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL] = "ASL";
}

template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = const_cast<TInputImage *> (this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }


  typedef PermuteAxesImageFilter< InputImageType >  PermuteFilterType;
  typedef FlipImageFilter < InputImageType > FlipFilterType;
  typedef CastImageFilter < InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput(permute->GetOutput());
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput(flip->GetOutput());
  cast->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());

// The input to the minipipeline is the input to this filter
// minipipeline. Therefore, the requested region of the minipipeline
// is the one needed by this filter.  
  cast->GetOutput()->UpdateOutputInformation();
  cast->GetOutput()->PropagateRequestedRegion();
}


template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>
::DeterminePermutationsAndFlips(
  const SpatialOrientation::ValidCoordinateOrientationFlags fixed_orient,
  const SpatialOrientation::ValidCoordinateOrientationFlags moving_orient)
{
  //std::cout <<"DEBUG Received Codes " <<fixed_orient <<"  and  " <<moving_orient <<std::endl;
  //3-dimensional version of code system only.  The 3-axis testing is unrolled.
  const unsigned int                 NumDims = 3;  //InputImageDimension is regarded as 3.
  const unsigned int               CodeField = 15; //4 bits wide
  const unsigned int           CodeAxisField = 14; //3 bits wide, above the 0-place bit.
  const unsigned int CodeAxisIncreasingField = 1;
  unsigned int fixed_codes[NumDims];
  unsigned int moving_codes[NumDims];
  fixed_codes[0]  = (fixed_orient  >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor) & CodeField;
  fixed_codes[1]  = (fixed_orient  >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor) & CodeField;
  fixed_codes[2]  = (fixed_orient  >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor) & CodeField;
  moving_codes[0] = (moving_orient >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor) & CodeField;
  moving_codes[1] = (moving_orient >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor) & CodeField;
  moving_codes[2] = (moving_orient >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor) & CodeField;
  //std::cout <<"DEBUG Fixed Codes " <<fixed_codes[0]  <<",  " <<fixed_codes[1]  <<"  and  " <<fixed_codes[2]  <<std::endl;
  //std::cout <<"DEBUG Moving Codes " <<moving_codes[0]  <<",  " <<moving_codes[1]  <<"  and  " <<moving_codes[2]  <<std::endl;

  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for (unsigned int i = 0; i<NumDims-1; i++)
    {
    if ((fixed_codes[i] & CodeAxisField) != (moving_codes[i] & CodeAxisField))
      {
      for (unsigned int j = 0; j<NumDims; j++)
        {
        if ((moving_codes[i] & CodeAxisField) == (fixed_codes[j] & CodeAxisField))
          {
          if (i==j)
            { //Axis i is already in place.
            continue;
            }
          else if ((moving_codes[j] & CodeAxisField) == (fixed_codes[i] & CodeAxisField))
            { //The cyclic permutation (i j) applies.  Therefore the remainder is (k), i.e., stationary.
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
            //std::cout <<"DEBUG DeterminePermutationsAndFlips: coded the swap of axes " <<i <<" and " <<j <<std::endl;
            }
          else
            { //Need to work out an (i j k) cyclic permutation:
            for (unsigned int k = 0; k<NumDims; k++)
              {
              if ((moving_codes[j] & CodeAxisField) == (fixed_codes[k] & CodeAxisField))
                {
                //At this point, we can pick off (i j k).
                m_PermuteOrder[i] = k;
                m_PermuteOrder[j] = i;
                m_PermuteOrder[k] = j;
                //std::cout <<"DEBUG DeterminePermutationsAndFlips: coded the swap of axes " <<i <<", " <<j <<" and " <<k <<std::endl;
                break;
                }
              }
            // Effectively, if (k==3) continue;
            }
          break;
          }
        }
      // Effectively, if (j==3) continue;
      }
    }

  for (unsigned int i = 0; i<NumDims; i++)
    {
    const unsigned int j = m_PermuteOrder[i];
    //std::cout <<"DEBUG comparing fixed code " <<fixed_codes[i] <<" with moving code " <<moving_codes[j] <<std::endl;
    if ((moving_codes[j] & CodeAxisIncreasingField) != (fixed_codes[i] & CodeAxisIncreasingField))
      {
      m_FlipAxes[i] = true;
      //std::cout <<"DEBUG DeterminePermutationsAndFlips: coded the flip of axis " <<i <<std::endl;
      }
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage,TOutputImage>
::SetGivenCoordinateOrientation(CoordinateOrientationCode newCode)
{
  m_GivenCoordinateOrientation = newCode;


  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    m_PermuteOrder[j] = j;
    }

  m_FlipAxes.Fill( false );

  this->DeterminePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage,TOutputImage>
::SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode)
{
  if (m_DesiredCoordinateOrientation != newCode)
    {
    m_DesiredCoordinateOrientation = newCode;

    for ( unsigned int j = 0; j < InputImageDimension; j++ )
      {
      m_PermuteOrder[j] = j;
      }
    
    m_FlipAxes.Fill( false );

    this->DeterminePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
    this->Modified();
    }
}


/** Returns true if a permute is required. Return false otherwise */
template<class TInputImage, class TOutputImage>
bool
OrientImageFilter<TInputImage, TOutputImage>
::NeedToPermute()
{
  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    if ( m_PermuteOrder[j] != j ) { return true; }
    }
  return false;
}


/** Returns true if flipping is required. Return false otherwise */
template<class TInputImage, class TOutputImage>
bool
OrientImageFilter<TInputImage, TOutputImage>
::NeedToFlip()
{
  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    if ( m_FlipAxes[j] ) { return true; }
    }
  return false;
}

template<class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  // Create a process accumulator for tracking the progress of this minipipeline
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // No need to allocate the output since the minipipeline does it
  // this->AllocateOutputs();

  typedef PermuteAxesImageFilter< InputImageType >  PermuteFilterType;
  typedef FlipImageFilter < InputImageType > FlipFilterType;
  typedef CastImageFilter < InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();

  progress->RegisterInternalFilter(permute,.3333333);
  progress->RegisterInternalFilter(flip,.3333333);
  progress->RegisterInternalFilter(cast,.3333333);

  InputImagePointer permuteInput = const_cast< TInputImage *> (this->GetInput());
  InputImagePointer flipInput = permuteInput;
  InputImagePointer castInput = permuteInput;

  // Only run those filters that will do something
  if (NeedToPermute())
    {
    permute->SetInput(permuteInput);
    permute->SetOrder(m_PermuteOrder);
    permute->ReleaseDataFlagOn();

    flipInput = permute->GetOutput();
    castInput = permute->GetOutput();
    }
  else
    {
    itkDebugMacro("No need to permute");
    }
  if (NeedToFlip())
    {
    flip->SetInput(flipInput);
    flip->SetFlipAxes(m_FlipAxes);
    flip->ReleaseDataFlagOn();

    castInput = flip->GetOutput();
    }
  else
    {
    itkDebugMacro( << "No need to flip");
    }

  if (typeid(TInputImage) != typeid(TOutputImage))
    {
    cast->SetInput(castInput);
    cast->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    cast->Update();
    this->GraftOutput( cast->GetOutput() );
    }
  else
    {
    castInput->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    castInput->Update();
    this->GraftOutput( castInput );
    itkDebugMacro( << "No need to cast");
    }

  this->GetOutput()->SetMetaDataDictionary( this->GetInput()->GetMetaDataDictionary() );

  itk::EncapsulateMetaData<SpatialOrientation::ValidCoordinateOrientationFlags>( this->GetOutput()->GetMetaDataDictionary(), ITK_CoordinateOrientation, m_DesiredCoordinateOrientation );

}

// Determine the "labeling" of a direction cosine. The axis labels
// depend upon the convention of the labels. In this class, axes are
// labeled using the negative end of the axis. For example, a
// right/left axis would be labeled right ("R").
// This code was copied and modified from code written by David Clunie
// (dclunie at dcluine.com)
static const double obliquityThresholdCosineValue = 0.8;
template <class TInputImage, class TOutputImage>
std::string
OrientImageFilter<TInputImage,TOutputImage>
::GetMajorAxisFromPatientRelativeDirectionCosine(double x, double y, double z)
{
  std::string axis;
  
  std::string orientationX = x < 0 ? "L" : "R";
  std::string orientationY = y < 0 ? "P" : "A";
  std::string orientationZ = z < 0 ? "S" : "I";
  
  double absX = vnl_math_abs(x);
  double absY = vnl_math_abs(y);
  double absZ = vnl_math_abs(z);
  
  // The tests here really don't need to check the other dimensions,
  // just the threshold, since the sum of the squares should be == 1.0
  // but just in case ...
  
  if (absX>obliquityThresholdCosineValue && absX>absY && absX>absZ)
    {
    axis=orientationX;
    }
  else if (absY>obliquityThresholdCosineValue && absY>absX && absY>absZ)
    {
    axis=orientationY;
    }
  else if (absZ>obliquityThresholdCosineValue && absZ>absX && absZ>absY)
    {
    axis=orientationZ;
    }
  return axis;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer  inputPtr  = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

// Either use the direciton cosines of the image or the user-specified
// orientation
  if (m_UseImageDirection)
    {
    // Compute the GivenOrientation from the image's direction cosines
    std::string orientation =
      this->GetMajorAxisFromPatientRelativeDirectionCosine(
        inputPtr->GetDirection()[0][0],
        inputPtr->GetDirection()[1][0],
        inputPtr->GetDirection()[2][0])
      + this->GetMajorAxisFromPatientRelativeDirectionCosine(
        inputPtr->GetDirection()[0][1],
        inputPtr->GetDirection()[1][1],
        inputPtr->GetDirection()[2][1])
      + this->GetMajorAxisFromPatientRelativeDirectionCosine(
        inputPtr->GetDirection()[0][2],
        inputPtr->GetDirection()[1][2],
        inputPtr->GetDirection()[2][2]);

    this->SetGivenCoordinateOrientation (m_StringToCode[orientation]);
    }

  typedef PermuteAxesImageFilter< InputImageType >  PermuteFilterType;
  typedef FlipImageFilter < InputImageType > FlipFilterType;
  typedef CastImageFilter < InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput(permute->GetOutput());
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput(flip->GetOutput());
  cast->UpdateOutputInformation();

  outputPtr->CopyInformation(cast->GetOutput());
}


template<class TInputImage, class TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  std::map<CoordinateOrientationCode,std::string>::const_iterator axes;

  axes = m_CodeToString.find(m_DesiredCoordinateOrientation);
  os << indent << "Desired Coordinate Orientation: "
     << m_DesiredCoordinateOrientation
     << " (" << (*axes).second << ")"
     << std::endl;
  axes = m_CodeToString.find(m_GivenCoordinateOrientation);
  os << indent << "Given Coordinate Orientation: "
     << m_GivenCoordinateOrientation
     << " (" << (*axes).second << ")"
     << std::endl;
  os << indent << "Use Image Direction: "
     << m_UseImageDirection
     << std::endl;
  os << indent << "Permute Axes: "
     << m_PermuteOrder
     << std::endl;
  os << indent << "Flip Axes: "
     << m_FlipAxes
     << std::endl;
}

}// end namespace itk
#endif
