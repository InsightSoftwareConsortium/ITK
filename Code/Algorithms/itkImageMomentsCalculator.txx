/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMomentsCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000-2001 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <stdlib.h>            // For abort

#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"

namespace itk
{ 
    template<class TPixel, int VRank> const char* 
    ImageMomentsCalculator<TPixel, VRank>::notvalid
    = "No valid image moments are available.";

    //----------------------------------------------------------------------
    // Construct without computing moments
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::ImageMomentsCalculator(void) {
	m_valid = 0;
    }

    //-----------------------------------------------------------------------
    // Construct and compute moments
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::
    ImageMomentsCalculator(
	ImageType &image) 
	{
	    ComputeMoments(image);
	}

    //----------------------------------------------------------------------
    // Destructor
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::
    ~ImageMomentsCalculator()
    {
    }

    //----------------------------------------------------------------------
    // Compute moments for a new or modified image
    template<class TPixel, int VRank>
    void
    ImageMomentsCalculator<TPixel, VRank>::
    ComputeMoments(
	ImageType &image)
    {

        /* Check for a *positive* number of dimensions */
        if (VRank <= 0)
            Error("Sorry! Number of dimensions may not be zero");

	/* Determine image dimensions */
	const Size<VRank>& size = image->GetLargestPossibleRegion().GetSize();
        unsigned long ncol = size[VRank-1];

	/* Zero moments before accumulating new ones */
	m_m0 = 0;
	for (int i = 0; i < VRank; i++) {
	    m_m1[i] = 0;
	    for (int j = 0; j < VRank; j++) {
		m_m2[i][j] = 0;
	    }
	}

	/* Loop over all rows of the image */
	double pix;
	Index<VRank> index;
	unsigned long coord[VRank];
        int iaxis = 0;
        int i, j;
        
        for (i = 0; i < VRank; i++)
            coord[i] = 0;
        do {

            /* Compute moments for current row */
            double r0 = 0, rx = 0, rxx = 0;
            for (unsigned long x = 0; x < ncol; x++) {

                /* Get the next pixel from the image */
                coord[VRank-1] = x;
                index.SetIndex(coord);
                pix = image->GetPixel(index);

                /* Accumulate moments within current row */
                r0  += pix;
                rx  += x*pix;
                rxx += (double)x*x*pix;
            }
		
            /* Accumulate moments over entire image */
            m_m0  += r0;
            m_m1[VRank-1]  += rx;
            m_m2[VRank-1][VRank-1] += rxx;
            for (i = 0; i < VRank-1; i++) {
                m_m1[i] += coord[i] * r0;
                m_m2[VRank-1][i] += coord[i] * rx;
                for (j = 0; j <= i; j++) {
                    m_m2[i][j] += (double)coord[i] * coord[j] * r0;
                }
            }

            /* Step to next row in the image */
            for (iaxis = VRank-2; iaxis >= 0; iaxis--) {
                if ( ++(coord[iaxis]) < size[iaxis] )
                    break;
                else
                    coord[iaxis] = 0;
            } 
	} while (iaxis >= 0);

	/* Reflect across diagonal */
        for (i = 0; i < VRank-1; i++)
            for (j = i+1; j < VRank; j++) 
                m_m2[i][j] = m_m2[j][i];

	/* Compute center of gravity and central moments */
	for (int r = 0; r < VRank; r++) {
	    m_cg[r] = m_m1[r] / m_m0;
	    m_cm[r][r] = m_m2[r][r] 
		- 2.0*m_cg[r]*m_m1[r] + m_cg[r]*m_cg[r]*m_m0;
	    for (int s = 0; s < r; s++) {
		m_cm[r][s] = m_m2[r][s]
		    - m_cg[r]*m_m1[s] - m_cg[s]*m_m1[r]
		    + m_cg[r]*m_cg[s]*m_m0;
	    }
	}

	/* Convert cg and central moments to physical units */
	double const *org = image->GetOrigin();
	double const *spc = image->GetSpacing();
	for (int r = 0; r < VRank; r++) {
	    m_cg[r] = spc[r] * m_cg[r] + org[r];
	    m_cm[r][r] *= spc[r] * spc[r];
	    for (int s = 0; s < r; s++) {
		m_cm[r][s] *= spc[r] * spc[s];
		m_cm[s][r] = m_cm[r][s];   // Reflect across diagonal
	    }
	}

	/* Compute principal moments and axes */
	vnl_symmetric_eigensystem<double> eigen(m_cm);
	vnl_diag_matrix<double> pm = eigen.D;
	for ( int i = 0; i < VRank; i++ )
	    m_pm[i] = pm(i,i);
	m_pa = eigen.V.transpose();

        /* Add a final reflection if needed for a proper rotation,
           by multiplying the last row by the determinant. */
        /* FIXME:  This is a really klutzy implementation; the right
           way would be to use an eigensystem solver in the step above
           that either preserves parity, or that at least counts
           the number of reflections that it does. */
        vnl_real_eigensystem eigenrot(m_pa);
        vnl_diag_matrix<vnl_double_complex> eigenval = eigenrot.D;
        vnl_double_complex det(1.0, 0.0);
        for ( int i = 0 ; i < VRank; ++i) {
            det *= eigenval(i,i);
        }
        for ( int i = 0; i < VRank; ++i) {
            m_pa[VRank-1][i] *= std::real(det);
        }
	
	/* Remember that the moments are valid */
	m_valid = 1;
    }


    //---------------------------------------------------------------------
    // Get sum of intensities
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel,VRank>::ScalarType
    ImageMomentsCalculator<TPixel,VRank>::
    GetTotalMass()
    {
	if (!m_valid)    Error(notvalid);
	return m_m0;
    }

    //--------------------------------------------------------------------
    // Get first moments about origin, in index coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel,VRank>::VectorType
    ImageMomentsCalculator<TPixel,VRank>::
    GetFirstMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_m1;
    }

    //--------------------------------------------------------------------
    // Get second moments about origin, in index coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel,VRank>::MatrixType
    ImageMomentsCalculator<TPixel,VRank>::
    GetSecondMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_m2;
    }

    //--------------------------------------------------------------------
    // Get center of gravity, in physical coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::VectorType
    ImageMomentsCalculator<TPixel, VRank>::
    GetCenterOfGravity()
    {
	if (!m_valid)    Error(notvalid);
	return m_cg;
    }

    //--------------------------------------------------------------------
    // Get second central moments, in physical coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::MatrixType
    ImageMomentsCalculator<TPixel, VRank>::
    GetCentralMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_cm;
    }

    //--------------------------------------------------------------------
    // Get principal moments, in physical coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::VectorType
    ImageMomentsCalculator<TPixel, VRank>::
    GetPrincipalMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_pm;
    }

    //--------------------------------------------------------------------
    // Get principal axes, in physical coordinates
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::MatrixType
    ImageMomentsCalculator<TPixel, VRank>::
    GetPrincipalAxes()
    {
	if (!m_valid)    Error(notvalid);
	return m_pa;
    }

    //--------------------------------------------------------------------
    // Get principal axes to physical axes transform
    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::AffineTransformType
    ImageMomentsCalculator<TPixel, VRank>::
    GetPrincipalAxesToPhysicalAxesTransform(void) const
    {
        AffineTransformType::MatrixType matrix;
        AffineTransformType::VectorType offset;
        for (int i = 0; i < VRank; i++) {
            for (int j = 0; j < VRank; j++)
                matrix[j][i] = m_pa[i][j];    // Note the transposition
            offset[i]    = m_cg [i];
        }

        AffineTransformType result(matrix, offset);
        result.SetMatrix(matrix);
        result.SetOffset(offset);

        return result;
    }


    //--------------------------------------------------------------------
    // Get physical axes to principal axes transform

    template<class TPixel, int VRank>
    ImageMomentsCalculator<TPixel, VRank>::AffineTransformType
    ImageMomentsCalculator<TPixel, VRank>::
    GetPhysicalAxesToPrincipalAxesTransform(void) const
    {
        AffineTransformType::MatrixType matrix;
        AffineTransformType::VectorType offset;
        for (int i = 0; i < VRank; i++) {
            for (int j = 0; j < VRank; j++)
                matrix[j][i] = m_pa[i][j];    // Note the transposition
            offset[i]    = m_cg [i];
        }

        AffineTransformType result(matrix, offset);
        result.SetMatrix(matrix);
        result.SetOffset(offset);

        return result.Inverse();
    }

    //--------------------------------------------------------------------
    /**
     * This private and interim method reports a error by printing
     * a given char string to standard error and aborting.  This
     * is a purely temporary method used as a placeholder until
     * the right implementation of error handling in itk is designed.
     */
    /* FIXME:  Use more general approach to error reporting */
    template<class TPixel, int VRank>
    void
    ImageMomentsCalculator<TPixel, VRank>::
    Error (const char *string) {
	std::cerr << string << "\n";
	abort();
    }


} // end namespace itk



// Define file characteristics for emacs
// FIXME:  Create .emacs commands for the Insight indentation style
// Local variables: ***
// END: ***
