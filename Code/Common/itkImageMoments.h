/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSFile: $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkImageMoments_h
#define __itkImageMoments_h

#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_diag_matrix.h"

ITK_NAMESPACE_BEGIN

/** 
 * Compute moments of an n-dimensional image.
 *
 * This class provides methods for computing the moments and related
 * properties of a single-echo image.  Computing the (non-central)
 * moments of a large image can easily take a million times longer
 * than computing the various other values derived from them, so we
 * compute the moments only on explicit request, and save their values
 * (in an ImageMoments object) for later retrieval by the user.
 *
 * The non-central moments computed by this class are not really 
 * intended for general use and are therefore in index coordinates;
 * that is, we pretend that the index that selects a particular
 * pixel also equals its physical coordinates.  The center of gravity,
 * central moments, principal moments and principal axes are all
 * more generally useful and are computed in the physical coordinates
 * defined by the Origin and Spacing parameters of the image.
 *
 * The methods that return values return the values themselves rather
 * than references because the cost is small compared to the cost of
 * computing the moments and doing so simplifies memory management for
 * the caller.
 *
 * FIXME: It's not yet clear how multi-echo images should be handled here.
 *
 * FIXME: The current implementation supports rank 3 images only.
 *
 */

// FIXME:  Find right incantation to doxygenate the template parameters
template <
    class TPixel,             // Type of the image data
    int   VRank >             // Rank = Number of dimensions
class ImageMoments
{
public:

    // Abbreviated types for use in this class
    /// Standard scalar type within this class.
    typedef double                                 ScalarT;
    /// Standard vector type within this class.
    typedef vnl_vector_fixed<double,VRank>         VectorT;
    /// Standard matrix type within this class.
    typedef vnl_matrix_fixed<double,VRank,VRank>   MatrixT;
    /// Standard image type within this class.
    typedef Image<TPixel, VRank>::Pointer          ImageT;

    
    // Compute moments for a new or modified image
    void ComputeMoments(ImageT &image);

    // Get sum of intensities
    ScalarT GetTotalMass();

    // Get first moments about origin, in index coordinates
    VectorT GetFirstMoments();

    // Get second moments about origin, in index coordinates
    MatrixT GetSecondMoments();

    // Get center of gravity, in physical coordinates
    VectorT GetCenterOfGravity();

    // Get second central moments, in physical coordinates
    MatrixT GetCentralMoments();

    // Get principal moments, in physical coordinates
    VectorT GetPrincipalMoments();

    // Get principal axes, in physical coordinates
    MatrixT GetPrincipalAxes();

    /* Constructors  */
    ImageMoments();            // Create w/o summing moments
    ImageMoments(              // Create and sum moments of an image
	ImageT &image);

    /* Destructor */
    ~ImageMoments();

private:
    bool m_valid;                   // Have moments been computed yet?
    ScalarT m_m0;                   // Zeroth moment
    VectorT m_m1;                   // First moments about origin
    MatrixT m_m2;                   // Second moments about origin
    VectorT m_cg;                   // Center of gravity (physical units)
    MatrixT m_cm;                   // Second central moments (physical)
    VectorT m_pm;                   // Principal moments (physical)
    MatrixT m_pa;                   // Principal axes (physical)

    static const char               // Error message when m_valid == 0
	notvalid[] = "No valid image moments are available.";

    void Error(const char *message);  // Report an error message and abort

};  // class ImageMoments

ITK_NAMESPACE_END


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageMoments.txx"
#endif

#endif /* __itkImageMoments_h */
