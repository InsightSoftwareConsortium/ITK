/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkOrientImageFilter_hxx
#define itkOrientImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkOrientImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkMetaDataObject.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
OrientImageFilter< TInputImage, TOutputImage >
::OrientImageFilter():
  m_GivenCoordinateOrientation  (SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP),
  m_DesiredCoordinateOrientation(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP),
  m_UseImageDirection (false),
  m_FlipAxes(false)
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

  for( unsigned int dimension = 0; dimension < InputImageDimension; ++dimension )
    {
    this->m_PermuteOrder[dimension] = dimension;
    }
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  InputImagePointer  inputPtr = const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  typedef PermuteAxesImageFilter< InputImageType >           PermuteFilterType;
  typedef FlipImageFilter< InputImageType >                  FlipFilterType;
  typedef CastImageFilter< InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput( permute->GetOutput() );
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput( flip->GetOutput() );
  cast->GetOutput()->SetRequestedRegion( outputPtr->GetRequestedRegion() );

  // The input to the minipipeline is the input to this filter
  // minipipeline. Therefore, the requested region of the minipipeline
  // is the one needed by this filter.
  cast->GetOutput()->UpdateOutputInformation();
  cast->GetOutput()->PropagateRequestedRegion();
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::DeterminePermutationsAndFlips(
  const SpatialOrientation::ValidCoordinateOrientationFlags fixed_orient,
  const SpatialOrientation::ValidCoordinateOrientationFlags moving_orient)
{
  // 3-dimensional version of code system only. The 3-axis testing is unrolled.
  const unsigned int NumDims = 3;                  // InputImageDimension is
                                                   // regarded as 3.
  const unsigned int CodeField = 15;               // 4 bits wide
  const unsigned int CodeAxisField = 14;           // 3 bits wide, above the
                                                   // 0-place bit.
  const unsigned int CodeAxisIncreasingField = 1;
  unsigned int       fixed_codes[NumDims];
  unsigned int       moving_codes[NumDims];

  fixed_codes[0]  = ( fixed_orient  >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor ) & CodeField;
  fixed_codes[1]  = ( fixed_orient  >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor ) & CodeField;
  fixed_codes[2]  = ( fixed_orient  >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor ) & CodeField;
  moving_codes[0] = ( moving_orient >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor ) & CodeField;
  moving_codes[1] = ( moving_orient >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor ) & CodeField;
  moving_codes[2] = ( moving_orient >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor ) & CodeField;

  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for ( unsigned int i = 0; i < NumDims - 1; i++ )
    {
    if ( ( fixed_codes[i] & CodeAxisField ) != ( moving_codes[i] & CodeAxisField ) )
      {
      for ( unsigned int j = 0; j < NumDims; j++ )
        {
        if ( ( moving_codes[i] & CodeAxisField ) == ( fixed_codes[j] & CodeAxisField ) )
          {
          if ( i == j )
            {
            // Axis i is already in place
            continue;
            }
          else if ( ( moving_codes[j] & CodeAxisField ) == ( fixed_codes[i] & CodeAxisField ) )
            {
            //The cyclic permutation (i j) applies. Therefore the remainder
            // is (k), i.e., stationary
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
            }
          else
            {
            // Need to work out an (i j k) cyclic permutation
            for ( unsigned int k = 0; k < NumDims; k++ )
              {
              if ( ( moving_codes[j] & CodeAxisField ) == ( fixed_codes[k] & CodeAxisField ) )
                {
                // At this point, we can pick off (i j k)
                m_PermuteOrder[i] = k;
                m_PermuteOrder[j] = i;
                m_PermuteOrder[k] = j;
                break;
                }
              }
            // Effectively, if (k==3) continue
            }
          break;
          }
        }
      // Effectively, if (j==3) continue
      }
    }

  for ( unsigned int i = 0; i < NumDims; i++ )
    {
    const unsigned int j = m_PermuteOrder[i];
    if ( ( moving_codes[j] & CodeAxisIncreasingField ) != ( fixed_codes[i] & CodeAxisIncreasingField ) )
      {
      m_FlipAxes[i] = true;
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::SetGivenCoordinateOrientation(CoordinateOrientationCode newCode)
{
  m_GivenCoordinateOrientation = newCode;

  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    m_PermuteOrder[j] = j;
    }

  m_FlipAxes.Fill(false);

  this->DeterminePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode)
{
  if ( m_DesiredCoordinateOrientation != newCode )
    {
    m_DesiredCoordinateOrientation = newCode;

    for ( unsigned int j = 0; j < InputImageDimension; j++ )
      {
      m_PermuteOrder[j] = j;
      }

    m_FlipAxes.Fill(false);

    this->DeterminePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
bool
OrientImageFilter< TInputImage, TOutputImage >
::NeedToPermute()
{
  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    if ( m_PermuteOrder[j] != j ) { return true; }
    }
  return false;
}

template< typename TInputImage, typename TOutputImage >
bool
OrientImageFilter< TInputImage, TOutputImage >
::NeedToFlip()
{
  for ( unsigned int j = 0; j < InputImageDimension; j++ )
    {
    if ( m_FlipAxes[j] ) { return true; }
    }
  return false;
}

//#define __DEBUG_ORIENT__
#if defined( __DEBUG_ORIENT__ )
#define DEBUG_EXECUTE(X) X

typedef itk::SpatialOrientation::ValidCoordinateOrientationFlags
SO_OrientationType;
std::string SO_OrientationToString(SO_OrientationType in)
{
  switch ( in )
    {
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      return std::string("RIP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      return std::string("LIP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      return std::string("RSP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      return std::string("LSP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      return std::string("RIA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      return std::string("LIA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      return std::string("RSA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      return std::string("LSA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      return std::string("IRP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      return std::string("ILP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
      return std::string("SRP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      return std::string("SLP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      return std::string("IRA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      return std::string("ILA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      return std::string("SRA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      return std::string("SLA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      return std::string("RPI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      return std::string("LPI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      return std::string("RAI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      return std::string("LAI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      return std::string("RPS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      return std::string("LPS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      return std::string("RAS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      return std::string("LAS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      return std::string("PRI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      return std::string("PLI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      return std::string("ARI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      return std::string("ALI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      return std::string("PRS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      return std::string("PLS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      return std::string("ARS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      return std::string("ALS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      return std::string("IPR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      return std::string("SPR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      return std::string("IAR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      return std::string("SAR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      return std::string("IPL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      return std::string("SPL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      return std::string("IAL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      return std::string("SAL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      return std::string("PIR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      return std::string("PSR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      return std::string("AIR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      return std::string("ASR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      return std::string("PIL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      return std::string("PSL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      return std::string("AIL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      return "ASL";
    default:
      {
      std::stringstream x;
      x << ( in & 0xff ) << ", " << ( ( in >> 8 ) & 0xff ) << ", " << ( ( in >> 16 ) && 0xff );
      return x.str();
      }
    }
}

template< typename ImageType >
void
DumpDirections(const std::string & prompt, const typename ImageType::Pointer & image)
{
  const typename ImageType::DirectionType & dir =
    image->GetDirection();
  std::cerr << prompt << " "
            << SO_OrientationToString( itk::SpatialOrientationAdapter().FromDirectionCosines(dir) )
            <<    std::endl;
  for ( unsigned i = 0; i < 3; i++ )
    {
    for ( unsigned j = 0; j < 3; j++ )
      {
      std::cerr << dir[i][j] << " ";
      }
    std::cerr << std::endl;
    }
}

#else //__DEBUG_ORIENT__
#define DEBUG_EXECUTE(X)
#endif

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // No need to allocate the output since the minipipeline does it
  // this->AllocateOutputs();

  typedef PermuteAxesImageFilter< InputImageType >           PermuteFilterType;
  typedef FlipImageFilter< InputImageType >                  FlipFilterType;
  typedef CastImageFilter< InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();

  progress->RegisterInternalFilter(permute, .3333333f);
  progress->RegisterInternalFilter(flip, .3333333f);
  progress->RegisterInternalFilter(cast, .3333333f);

  InputImagePointer permuteInput = const_cast< TInputImage * >( this->GetInput() );
  InputImagePointer flipInput = permuteInput;
  InputImagePointer castInput = permuteInput;

  // Only run those filters that will do something
  if ( NeedToPermute() )
    {
    DEBUG_EXECUTE(DumpDirections< TInputImage >("before permute", permuteInput); )
    permute->SetInput(permuteInput);
    permute->SetOrder(m_PermuteOrder);
    permute->ReleaseDataFlagOn();
    DEBUG_EXECUTE(
      std::cerr << "Permute Axes: ";
      for ( unsigned i = 0; i < 3; i++ )
        {
        std::cerr << m_PermuteOrder[i] << " ";
        }
      std::cerr << std::endl;
      permute->Update();
      DumpDirections< TInputImage >( "after permute", permute->GetOutput() );
      )
    flipInput = permute->GetOutput();
    castInput = permute->GetOutput();
    }
  else
    {
    itkDebugMacro("No need to permute");
    }
  if ( NeedToFlip() )
    {
    flip->SetInput(flipInput);
    flip->SetFlipAxes(m_FlipAxes);
    flip->FlipAboutOriginOff();
    flip->ReleaseDataFlagOn();
    DEBUG_EXECUTE(
      std::cerr << "Flip Axes: ";
      for ( unsigned i = 0; i < 3; i++ )
        {
        std::cerr << m_FlipAxes[i] << " ";
        }
      std::cerr << std::endl;
      flip->Update();
      DumpDirections< TInputImage >( "after flip", flip->GetOutput() );
      )
    castInput = flip->GetOutput();
    }
  else
    {
    itkDebugMacro(<< "No need to flip");
    }

  //
  // Cast might not be necessary, but CastImagefilter is optimized for
  // the case where the InputImageType == OutputImageType
  cast->SetInput(castInput);
  cast->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  cast->Update();
  this->GraftOutput( cast->GetOutput() );

  this->GetOutput()->SetMetaDataDictionary( this->GetInput()->GetMetaDataDictionary() );
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImageConstPointer inputPtr  = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Either use the direciton cosines of the image or the user-specified
  // orientation
  if ( m_UseImageDirection )
    {
    // Compute the GivenOrientation from the image's direction cosines
    this->SetGivenCoordinateOrientation
      ( SpatialOrientationAdapter().FromDirectionCosines( inputPtr->GetDirection() ) );
    }

  typedef PermuteAxesImageFilter< InputImageType >           PermuteFilterType;
  typedef FlipImageFilter< InputImageType >                  FlipFilterType;
  typedef CastImageFilter< InputImageType, OutputImageType > CastToOutputFilterType;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput( permute->GetOutput() );
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput( flip->GetOutput() );
  cast->UpdateOutputInformation();

  outputPtr->CopyInformation( cast->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
OrientImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  std::map< CoordinateOrientationCode, std::string >::const_iterator axes;

  axes = m_CodeToString.find(m_DesiredCoordinateOrientation);
  os << indent << "Desired Coordinate Orientation: "
     << static_cast< long >( this->GetDesiredCoordinateOrientation() )
     << " (" << ( *axes ).second << ")"
     << std::endl;
  axes = m_CodeToString.find(m_GivenCoordinateOrientation);
  os << indent << "Given Coordinate Orientation: "
     << static_cast< long >( this->GetGivenCoordinateOrientation() )
     << " (" << ( *axes ).second << ")"
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
} // end namespace itk
#endif
