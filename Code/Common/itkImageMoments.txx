/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMoments.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <stdlib.h>            // For abort

#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"

namespace itk
{ 
  template<class TPixel, int VRank> const char* 
  ImageMoments<TPixel, VRank>::notvalid
  = "No valid image moments are available.";

    /** 
     * Construct an ImageMoments object.
     *
     * This method constructs a new ImageMoments object that contains no
     * stored moments information; this information can be added later
     * by calling the ComputeMoments method.
     */    
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::ImageMoments(void) {
	m_valid = 0;
    }

    /**
     * Compute moments of an image and save in an ImageMoments object.
     *
     * This method constructs a new ImageMoments object and stores in
     * it the moments of the image given as argument.  The values of
     * these moments and related parameters can be retrieved by using
     * other methods of the object constructed.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::
    ImageMoments(
	ImageType &image) 
	{
	    ComputeMoments(image);
	}

    /**
     * Destroy an ImageMoments object.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::
    ~ImageMoments()
    {
    }

    /**
     * Compute moments of a new or modified image.
     *
     * This method computes the moments of the image given as a
     * parameter and stores them in the object.  The values of these
     * moments and related parameters can then be retrieved by using
     * other methods of this object.
     *
     *  FIXME: Algorithm works for rank 3 images only.
     *
     */
    template<class TPixel, int VRank>
    void
    ImageMoments<TPixel, VRank>::
    ComputeMoments(
	ImageType &image)
    {
	/* FIXME:  Algorithm currently works for 3D only */
	if ( VRank != 3) {
	    Error("Sorry! Currently supported for 3D only");
	    }

	/* Determine image dimensions */
	const Size<VRank>& size = image->GetLargestPossibleRegion().GetSize();
	const unsigned long nslices = size[0];
	const unsigned long nrows   = size[1];
        const unsigned long ncols   = size[2];

	/* Zero moments before accumlating new ones */
	m_m0 = 0;
	for (int i = 0; i < VRank; i++) {
	    m_m1[i] = 0;
	    for (int j = 0; j < VRank; j++) {
		m_m2[i][j] = 0;
	    }
	}

	/* Loop over all rows and pixels to compute non-central moments */
	/* FIXME:  Find or write an iterator that does this in n-D */
	double pix;
	Index<3> index;
	long coord[3];

	for (unsigned long z = 0; z < nslices; z++) {
	    coord[0] = z;
	    for (unsigned long y = 0; y < nrows; y++) {
		coord[1] = y;

		/* Compute moments for current row */
		double r0 = 0, rx = 0, rxx = 0;
		for (unsigned long x = 0; x < ncols; x++) {

		    /* Get the next pixel from the image */
		    coord[2] = x;
		    index.SetIndex(coord);
		    pix = image->GetPixel(index);

		    /* Accumulate moments within current row */
		    r0  += pix;
		    rx  += x*pix;
		    rxx += (double)x*x*pix;
		}
		
		/* Accumulate moments over entire image */
		m_m0  += r0;
		m_m1[2]  += rx;
		m_m1[1]  += y * r0;
		m_m1[0]  += z * r0;
		m_m2[2][2] += rxx;
		m_m2[2][1] += y * rx;
		m_m2[2][0] += z * rx;
		m_m2[1][1] += (double)y * y * r0;
		m_m2[0][0] += (double)z * z * r0;
		m_m2[1][0] += (double)y * z * r0;
	    }
	}

	/* Reflect across diagonal */
	m_m2[1][2] = m_m2[2][1];
	m_m2[0][2] = m_m2[2][0];
	m_m2[0][1] = m_m2[1][0];

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

    /**
     * Return the total mass (or zeroth moment) of an image.
     *
     * This method returns the sun of pixel intensities (also known as
     * the zeroth moment or the total mass) of the image whose moments
     * were last computed by this object.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel,VRank>::ScalarType
    ImageMoments<TPixel,VRank>::
    GetTotalMass()
    {
	if (!m_valid)    Error(notvalid);
	return m_m0;
    }

    /**
     * Return first moments about origin, in index coordinates.
     *
     * This method returns the first moments around the origin of the
     * image whose moments were last computed by this object.  For
     * simplicity, these moments are computed in index coordinates
     * rather than physical coordinates.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel,VRank>::VectorType
    ImageMoments<TPixel,VRank>::
    GetFirstMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_m1;
    }

    /**
     * Return second moments about origin, in index coordinates.
     *
     * This method returns the second moments around the origin
     * of the image whose moments were last computed by this object.
     * For simplicity, these moments are computed in index coordinates
     * rather than physical coordinates.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel,VRank>::MatrixType
    ImageMoments<TPixel,VRank>::
    GetSecondMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_m2;
    }

    /**
     * Return center of gravity, in physical coordinates.
     *
     * This method returns the center of gravity of the image whose
     * moments were last computed by this object.  The center of
     * gravity is computed in physical coordinates.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::VectorType
    ImageMoments<TPixel, VRank>::
    GetCenterOfGravity()
    {
	if (!m_valid)    Error(notvalid);
	return m_cg;
    }

    /**
     * Return second central moments, in physical coordinates.
     *
     * This method returns the central second moments of the image
     * whose moments were last computed by this object.  The central
     * moments are computed in physical coordinates.
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::MatrixType
    ImageMoments<TPixel, VRank>::
    GetCentralMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_cm;
    }

    /**
     * Return principal moments, in physical coordinates.
     *
     * This method returns the principal moments of the image whose
     * moments were last computed by this object.  The moments are
     * returned as a vector, with the principal moments ordered from
     * smallest to largest.  The moments are computed in physical
     * coordinates.  
     */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::VectorType
    ImageMoments<TPixel, VRank>::
    GetPrincipalMoments()
    {
	if (!m_valid)    Error(notvalid);
	return m_pm;
    }

    /**
     * Return principal axes, in physical coordinates.
     *
     * This method returns the principal axes of the image whose
     * moments were last computed by this object.  The moments are
     * returned as an orthogonal matrix, each row of which corresponds
     * to one principal moment; for example, the principal axis
     * corresponding to the smallest principal moment is the vector
     * m[0], where m is the value returned by this method.  The matrix
     * of principal axes is guaranteed to be a proper rotation; that
     * is, to have determinant +1 and to preserve parity.  (Unless you
     * have foolishly made one or more of the spacing values negative;
     * in that case, _you_ get to figure out the consequences.)  The
     * moments are computed in physical coordinates.  */
    template<class TPixel, int VRank>
    ImageMoments<TPixel, VRank>::MatrixType
    ImageMoments<TPixel, VRank>::
    GetPrincipalAxes()
    {
	if (!m_valid)    Error(notvalid);
	return m_pa;
    }

    /**
     * This private and interim method reports a error by printing
     * a given char string to standard error and aborting.  This
     * is a purely temporary method used as a placeholder until
     * the right implementation of error handling in itk is designed.
     */
    /* FIXME:  Use more general approach to error reporting */
    template<class TPixel, int VRank>
    void
    ImageMoments<TPixel, VRank>::
    Error (const char *string) {
	std::cerr << string << "\n";
	abort();
    }


} // end namespace itk



// Define file characteristics for emacs
// FIXME:  Create .emacs commands for the Insight indentation style
// Local variables: ***
// END: ***
