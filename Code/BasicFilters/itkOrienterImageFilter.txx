/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOrienterImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOrienterImageFilter_txx
#define __itkOrienterImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkOrienterImageFilter.h"
#include <itkCastImageFilter.h>
#include <itkConstantPadImageFilter.h>
#include <itkExtractImageFilter.h>
#include "itkProgressAccumulator.h"

namespace itk {

    template <class TInputImage, class TOutputImage>
        OrienterImageFilter<TInputImage, TOutputImage>
        ::OrienterImageFilter()
        : m_GivenCoordinateOrientation  ( itk::IOCommon::ITK_COORDINATE_ORIENTATION_RIP ),
    m_DesiredCoordinateOrientation( itk::IOCommon::ITK_COORDINATE_ORIENTATION_RIP )

        {
        }

    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage, TOutputImage>
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

            // we need to compute the input requested region (size and start index)
            unsigned int i;
            const typename TOutputImage::SizeType& outputRequestedRegionSize         = outputPtr->GetRequestedRegion().GetSize();
            const typename TOutputImage::IndexType& outputRequestedRegionStartIndex  = outputPtr->GetRequestedRegion().GetIndex();

            typename TInputImage::SizeType  inputRequestedRegionSize;
            typename TInputImage::IndexType inputRequestedRegionStartIndex;

            for (i = 0; i < TInputImage::ImageDimension; i++)
                {
                inputRequestedRegionSize[i]       =       outputRequestedRegionSize[m_PermuteOrder[i]];
                inputRequestedRegionStartIndex[i] = outputRequestedRegionStartIndex[m_PermuteOrder[i]];
                }

            typename TInputImage::RegionType inputRequestedRegion;
            inputRequestedRegion.SetSize( inputRequestedRegionSize );
            inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

            inputPtr->SetRequestedRegion( inputRequestedRegion );
            }


    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage, TOutputImage>
        ::EnlargeOutputRequestedRegion(DataObject *)
            {
            this->GetOutput()
                ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
            }


    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage, TOutputImage>
        ::determinePermutationsAndFlips(const itk::IOCommon::ValidCoordinateOrientationFlags fixed_orient, const itk::IOCommon::ValidCoordinateOrientationFlags moving_orient)
            {
            //std::cout <<"DEBUG Received Codes " <<fixed_orient <<"  and  " <<moving_orient <<std::endl;
            //3-dimensional version of code system only.  The 3-axis testing is unrolled.
            const unsigned int                 NumDims = 3;  //InputImageDimension is regarded as 3.
            const unsigned int               CodeField = 15; //4 bits wide
            const unsigned int           CodeAxisField = 14; //3 bits wide, above the 0-place bit.
            const unsigned int CodeAxisIncreasingField = 1;
            unsigned int fixed_codes[NumDims];
            unsigned int moving_codes[NumDims];
            fixed_codes[0]  = (fixed_orient  >> itk::IOCommon::ITK_COORDINATE_PrimaryMinor) & CodeField;
            fixed_codes[1]  = (fixed_orient  >> itk::IOCommon::ITK_COORDINATE_SecondaryMinor) & CodeField;
            fixed_codes[2]  = (fixed_orient  >> itk::IOCommon::ITK_COORDINATE_TertiaryMinor) & CodeField;
            moving_codes[0] = (moving_orient >> itk::IOCommon::ITK_COORDINATE_PrimaryMinor) & CodeField;
            moving_codes[1] = (moving_orient >> itk::IOCommon::ITK_COORDINATE_SecondaryMinor) & CodeField;
            moving_codes[2] = (moving_orient >> itk::IOCommon::ITK_COORDINATE_TertiaryMinor) & CodeField;
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
                                //std::cout <<"DEBUG determinePermutationsAndFlips: coded the swap of axes " <<i <<" and " <<j <<std::endl;
                                }
                            else
                                { //Need to work out an (i j k) cyclic permutation:
                                for (unsigned int k = 0; k<NumDims; k++)
                                    {
                                    if ((moving_codes[j] & CodeAxisField) == (fixed_codes[k] & CodeAxisField))
                                        {
                                        //At this point, we can pick off (i j k).
                                        m_PermuteOrder[i] = j;
                                        m_PermuteOrder[j] = k;
                                        m_PermuteOrder[k] = i;
                                        //std::cout <<"DEBUG determinePermutationsAndFlips: coded the swap of axes " <<i <<", " <<j <<" and " <<k <<std::endl;
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
                    //std::cout <<"DEBUG determinePermutationsAndFlips: coded the flip of axis " <<i <<std::endl;
                    }
                }
            }


    /**
     *
     */
    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage,TOutputImage>
        ::SetGivenCoordinateOrientation(CoordinateOrientationCode newCode)
            {
            m_GivenCoordinateOrientation = newCode;


            for ( unsigned int j = 0; j < InputImageDimension; j++ )
                {
                m_PermuteOrder[j] = j;
                }

            m_FlipAxes.Fill( false );

            determinePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
            }


    /**
     *
     */
    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage,TOutputImage>
        ::SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode)
            {
            m_DesiredCoordinateOrientation = newCode;

            for ( unsigned int j = 0; j < InputImageDimension; j++ )
                {
                m_PermuteOrder[j] = j;
                }

            m_FlipAxes.Fill( false );

            determinePermutationsAndFlips (m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
            }


    /** Returns true if a permute is required. Return false otherwise */
    template<class TInputImage, class TOutputImage>
        bool
        OrienterImageFilter<TInputImage, TOutputImage>
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
        OrienterImageFilter<TInputImage, TOutputImage>
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
        OrienterImageFilter<TInputImage, TOutputImage>
        ::GenerateData()
            {

            // Create a process accumulator for tracking the progress of this minipipeline
            typename itk::ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
            progress->SetMiniPipelineFilter(this);


            // Allocate the output
            this->AllocateOutputs();

            //The indented stuff here is from bkItkPermuteAxes.txx (brains2)

            const unsigned int Dimension = 3;

            typedef itk::Image< typename InputImageType::PixelType, Dimension > CubeImageType;

            typename InputImageType::SizeType originalSize;
            originalSize = this->GetInput()->GetLargestPossibleRegion().GetSize();
            int dims[Dimension];
            for (unsigned int i=0; i<Dimension; i++)
                {
                dims[i] = originalSize[i];
                }

            /* Now we are going to build up the ITK pipeline for processing */

            // Convenient typedefs
            typedef itk::ConstantPadImageFilter < InputImageType, CubeImageType > PadInputFilterType;
            typedef itk::ExtractImageFilter < CubeImageType, CubeImageType > ExtractFilterType;
            typedef itk::FlipImageFilter < CubeImageType > FlipFilterType;
            typedef itk::CastImageFilter < CubeImageType, OutputImageType > CastToOutputFilterType;

            // Create the casting filters
            typename PadInputFilterType::Pointer to_cube_padded = PadInputFilterType::New();
            typename ExtractFilterType::Pointer from_cube = ExtractFilterType::New();
            typename CastToOutputFilterType::Pointer to_output = CastToOutputFilterType::New();

            int maxDim = dims[0];
            if (maxDim < (int) dims[1]) maxDim = dims[1];
            if (maxDim < (int) dims[2]) maxDim = dims[2];
            unsigned long sizeData[3];
            sizeData[0] = (maxDim - dims[0]);
            sizeData[1] = (maxDim - dims[1]);
            sizeData[2] = (maxDim - dims[2]);
            //to_cube_padded->SetPadLowerBound( sizeData );
            to_cube_padded->SetPadUpperBound( sizeData );

            to_cube_padded->SetConstant( 0 );


            typedef itk::PermuteAxesImageFilter< CubeImageType >  PermuteFilterType;

            typename PermuteFilterType::Pointer permuteAxesFilter = PermuteFilterType::New();
            typename  FlipFilterType::Pointer  flipAxesFilter  = FlipFilterType::New();


            permuteAxesFilter->SetOrder( m_PermuteOrder );


            /* Set the ITK image size based on the size of the BRAINS2 image */
            typename InputImageType::SizeType  xsize;
            xsize[0]  = (int) dims[m_PermuteOrder[0]];  // size along X
            xsize[1]  = (int) dims[m_PermuteOrder[1]];  // size along Y
            xsize[2]  = (int) dims[m_PermuteOrder[2]];  // size along Z

            typename InputImageType::IndexType xstart;
            xstart.Fill(0);

            typename InputImageType::RegionType xregion;
            xregion.SetIndex( xstart );
            xregion.SetSize( xsize );

            from_cube->SetExtractionRegion( xregion );


            flipAxesFilter->SetFlipAxes( m_FlipAxes );
            //std::cout <<"DEBUG: FlipAxes are " <<flipAxesFilter->GetFlipAxes() <<std::endl;

            // Connect the pipeline
            to_cube_padded->SetInput( this->GetInput() );
            permuteAxesFilter->SetInput( to_cube_padded->GetOutput() );
            from_cube->SetInput( permuteAxesFilter->GetOutput() );
            flipAxesFilter->SetInput( from_cube->GetOutput() );
            to_output->SetInput( flipAxesFilter->GetOutput() );


            //std::cout <<"DEBUG: before to_output->GraftOutput( this->GetOutput() );" <<std::endl;
            // graft our output to the subtract filter to force the proper regions
            // to be generated
            to_output->GraftOutput( this->GetOutput() );

            // run the algorithm
            // March down the pipeline to show what the problem is.
            progress->RegisterInternalFilter(to_output,1.0f);

            flipAxesFilter->Update();

                {
                float flipOrigin[Dimension];
                for (unsigned int i=0; i<Dimension; i++)
                    {
                    flipOrigin[i] = 0.0;
                    }
                flipAxesFilter->GetOutput()->SetOrigin(flipOrigin);
                }

            //std::cout <<"DEBUG: before to_output->Update();" <<std::endl;
            to_output->Update();

            // graft the output of the subtract filter back onto this filter's
            // output. this is needed to get the appropriate regions passed
            // back.
            //std::cout <<"DEBUG: before this->GraftOutput( to_output->GetOutput() );" <<std::endl;
                {
                typename CastToOutputFilterType::OutputImageType::Pointer tempImage = to_output->GetOutput();
                std::cout <<(tempImage) <<std::endl;
                }
            this->GraftOutput( to_output->GetOutput() );
            this->GetOutput()->SetMetaDataDictionary( this->GetInput()->GetMetaDataDictionary() );
            EncapsulateMetaData < itk::IOCommon::ValidCoordinateOrientationFlags > ( this->GetOutput()->GetMetaDataDictionary(), ITK_CoordinateOrientation, m_DesiredCoordinateOrientation );

            }

    /**
     *
     */
    template <class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage,TOutputImage>
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

            // We need to compute the output spacing, the output image size, and the output image start index.
            unsigned int i;
            const double                           *inputSpacing     = inputPtr->GetSpacing();
            const typename TInputImage::SizeType&   inputSize        = inputPtr->GetLargestPossibleRegion().GetSize();
            const typename TInputImage::IndexType&  inputStartIndex  = inputPtr->GetLargestPossibleRegion().GetIndex();

            float    outputSpacing[TOutputImage::ImageDimension];
            typename TOutputImage::SizeType     outputSize;
            typename TOutputImage::IndexType    outputStartIndex;

            for (i = 0; i < TOutputImage::ImageDimension; i++)
                {
                outputSpacing[i]    =    inputSpacing[m_PermuteOrder[i]];
                outputSize[i]       =       inputSize[m_PermuteOrder[i]];
                outputStartIndex[i] = inputStartIndex[m_PermuteOrder[i]];
                }

            outputPtr->SetSpacing( outputSpacing );

            typename TOutputImage::RegionType outputLargestPossibleRegion;
            outputLargestPossibleRegion.SetSize( outputSize );
            outputLargestPossibleRegion.SetIndex( outputStartIndex );

            outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
            }


    template<class TInputImage, class TOutputImage>
        void
        OrienterImageFilter<TInputImage, TOutputImage>
        ::PrintSelf(std::ostream &os, Indent indent) const
            {
            Superclass::PrintSelf(os, indent);

            os << indent << "Desired Orientation Code: "
                << m_DesiredCoordinateOrientation
                << std::endl;
            os << indent << "Given Orientation Code: "
                << m_GivenCoordinateOrientation
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
