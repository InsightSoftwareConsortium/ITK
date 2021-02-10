/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Friday, October 10, 1997
 */


#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Oprivate.h"
#include "H5VMprivate.h"

/* Local typedefs */
typedef struct H5VM_memcpy_ud_t {
    unsigned char *dst;         /* Pointer to destination buffer */
    const unsigned char *src;   /* Pointer to source buffer */
} H5VM_memcpy_ud_t;

/* Local macros */
#define H5VM_HYPER_NDIMS H5O_LAYOUT_NDIMS

/* Local prototypes */
static void
H5VM_stride_optimize1(unsigned *np/*in,out*/, hsize_t *elmt_size/*in,out*/,
		     const hsize_t *size, hsize_t *stride1);
static void
H5VM_stride_optimize2(unsigned *np/*in,out*/, hsize_t *elmt_size/*in,out*/,
		     const hsize_t *size, hsize_t *stride1, hsize_t *stride2);
#ifdef LATER
static void
H5VM_stride_copy2(hsize_t nelmts, hsize_t elmt_size,
     unsigned dst_n, const hsize_t *dst_size, const ssize_t *dst_stride, void *_dst,
     unsigned src_n, const hsize_t *src_size, const ssize_t *src_stride, const void *_src);
#endif /* LATER */


/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_optimize1
 *
 * Purpose:	Given a stride vector which references elements of the
 *		specified size, optimize the dimensionality, the stride
 *		vector, and the element size to minimize the dimensionality
 *		and the number of memory accesses.
 *
 *		All arguments are passed by reference and their values may be
 *		modified by this function.
 *
 * Return:	None
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5VM_stride_optimize1(unsigned *np/*in,out*/, hsize_t *elmt_size/*in,out*/,
		     const hsize_t *size, hsize_t *stride1)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /*
     * This has to be true because if we optimize the dimensionality down to
     * zero we still must make one reference.
     */
    HDassert(1 == H5VM_vector_reduce_product(0, NULL));

    /*
     * Combine adjacent memory accesses
     */
    while (*np && stride1[*np-1]>0 &&
           (hsize_t)(stride1[*np-1])==*elmt_size) {
        *elmt_size *= size[*np-1];
        if (--*np)
            stride1[*np-1] += size[*np] * stride1[*np];
    }

    FUNC_LEAVE_NOAPI_VOID
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_optimize2
 *
 * Purpose:	Given two stride vectors which reference elements of the
 *		specified size, optimize the dimensionality, the stride
 *		vectors, and the element size to minimize the dimensionality
 *		and the number of memory accesses.
 *
 *		All arguments are passed by reference and their values may be
 *		modified by this function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *              Unrolled loops for common cases
 *              Quincey Koziol
 *		?, ? ?, 2001?
 *
 *-------------------------------------------------------------------------
 */
static void
H5VM_stride_optimize2(unsigned *np/*in,out*/, hsize_t *elmt_size/*in,out*/,
		     const hsize_t *size, hsize_t *stride1, hsize_t *stride2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /*
     * This has to be true because if we optimize the dimensionality down to
     * zero we still must make one reference.
     */
    HDassert(1 == H5VM_vector_reduce_product(0, NULL));
    HDassert(*elmt_size>0);

    /*
     * Combine adjacent memory accesses
     */

    /* Unroll loop for common cases */
    switch(*np) {
        case 1: /* For 0-D datasets (dunno if this ever gets used...) */
            if(stride1[0] == *elmt_size && stride2[0] == *elmt_size) {
                *elmt_size *= size[0];
                --*np;  /* *np decrements to a value of 0 now */
            } /* end if */
            break;

        case 2: /* For 1-D datasets */
            if(stride1[1] == *elmt_size && stride2[1] == *elmt_size) {
                *elmt_size *= size[1];
                --*np;  /* *np decrements to a value of 1 now */
                stride1[0] += size[1] * stride1[1];
                stride2[0] += size[1] * stride2[1];

                if(stride1[0] == *elmt_size && stride2[0] == *elmt_size) {
                    *elmt_size *= size[0];
                    --*np;  /* *np decrements to a value of 0 now */
                } /* end if */
            } /* end if */
            break;

        case 3: /* For 2-D datasets */
            if(stride1[2] == *elmt_size && stride2[2] == *elmt_size) {
                *elmt_size *= size[2];
                --*np;  /* *np decrements to a value of 2 now */
                stride1[1] += size[2] * stride1[2];
                stride2[1] += size[2] * stride2[2];

                if(stride1[1] == *elmt_size && stride2[1] == *elmt_size) {
                    *elmt_size *= size[1];
                    --*np;  /* *np decrements to a value of 1 now */
                    stride1[0] += size[1] * stride1[1];
                    stride2[0] += size[1] * stride2[1];

                    if(stride1[0] == *elmt_size && stride2[0] == *elmt_size) {
                        *elmt_size *= size[0];
                        --*np;  /* *np decrements to a value of 0 now */
                    } /* end if */
                } /* end if */
            } /* end if */
            break;

        case 4: /* For 3-D datasets */
            if(stride1[3] == *elmt_size && stride2[3] == *elmt_size) {
                *elmt_size *= size[3];
                --*np;  /* *np decrements to a value of 3 now */
                stride1[2] += size[3] * stride1[3];
                stride2[2] += size[3] * stride2[3];

                if(stride1[2] == *elmt_size && stride2[2] == *elmt_size) {
                    *elmt_size *= size[2];
                    --*np;  /* *np decrements to a value of 2 now */
                    stride1[1] += size[2] * stride1[2];
                    stride2[1] += size[2] * stride2[2];

                    if(stride1[1] == *elmt_size && stride2[1] == *elmt_size) {
                        *elmt_size *= size[1];
                        --*np;  /* *np decrements to a value of 1 now */
                        stride1[0] += size[1] * stride1[1];
                        stride2[0] += size[1] * stride2[1];

                        if(stride1[0] == *elmt_size && stride2[0] == *elmt_size) {
                            *elmt_size *= size[0];
                            --*np;  /* *np decrements to a value of 0 now */
                        } /* end if */
                    } /* end if */
                } /* end if */
            } /* end if */
            break;

        default:
            while (*np &&
                    stride1[*np-1] == *elmt_size &&
                    stride2[*np-1] == *elmt_size) {
                *elmt_size *= size[*np-1];
                if (--*np) {
                    stride1[*np-1] += size[*np] * stride1[*np];
                    stride2[*np-1] += size[*np] * stride2[*np];
                }
            }
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI_VOID
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_hyper_stride
 *
 * Purpose:	Given a description of a hyperslab, this function returns
 *		(through STRIDE[]) the byte strides appropriate for accessing
 *		all bytes of the hyperslab and the byte offset where the
 *		striding will begin.  The SIZE can be passed to the various
 *		stride functions.
 *
 *		The dimensionality of the whole array, the hyperslab, and the
 *		returned stride array is N.  The whole array dimensions are
 *		TOTAL_SIZE and the hyperslab is at offset OFFSET and has
 *		dimensions SIZE.
 *
 *		The stride and starting point returned will cause the
 *		hyperslab elements to be referenced in C order.
 *
 * Return:	Success:	Byte offset from beginning of array to start
 *				of striding.
 *
 *		Failure:	abort() -- should never fail
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *              Unrolled loops for common cases
 *              Quincey Koziol
 *		?, ? ?, 2001?
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5VM_hyper_stride(unsigned n, const hsize_t *size,
		 const hsize_t *total_size, const hsize_t *offset,
		 hsize_t *stride/*out*/)
{
    hsize_t	    skip;	/*starting point byte offset		*/
    hsize_t	    acc;	/*accumulator				*/
    int		i;		/*counter				*/
    hsize_t	    ret_value;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(size);
    HDassert(total_size);
    HDassert(stride);

    /* init */
    HDassert(n>0);
    stride[n-1] = 1;
    skip = offset ? offset[n-1] : 0;

    switch(n) {
        case 2: /* 1-D dataset */
            HDassert(total_size[1]>=size[1]);
            stride[0] = total_size[1]-size[1]; /*overflow checked*/
            acc = total_size[1];
            skip += acc * (offset ? offset[0] : 0);
            break;

        case 3: /* 2-D dataset */
            HDassert(total_size[2]>=size[2]);
            stride[1] = total_size[2]-size[2]; /*overflow checked*/
            acc = total_size[2];
            skip += acc * (offset ? (hsize_t)offset[1] : 0);

            HDassert(total_size[1]>=size[1]);
            stride[0] = acc * (total_size[1] - size[1]); /*overflow checked*/
            acc *= total_size[1];
            skip += acc * (offset ? (hsize_t)offset[0] : 0);
            break;

        case 4: /* 3-D dataset */
            HDassert(total_size[3]>=size[3]);
            stride[2] = total_size[3]-size[3]; /*overflow checked*/
            acc = total_size[3];
            skip += acc * (offset ? (hsize_t)offset[2] : 0);

            HDassert(total_size[2]>=size[2]);
            stride[1] = acc * (total_size[2] - size[2]); /*overflow checked*/
            acc *= total_size[2];
            skip += acc * (offset ? (hsize_t)offset[1] : 0);

            HDassert(total_size[1]>=size[1]);
            stride[0] = acc * (total_size[1] - size[1]); /*overflow checked*/
            acc *= total_size[1];
            skip += acc * (offset ? (hsize_t)offset[0] : 0);
            break;

        default:
            /* others */
            for (i=(int)(n-2), acc=1; i>=0; --i) {
                HDassert(total_size[i+1]>=size[i+1]);
                stride[i] = acc * (total_size[i+1] - size[i+1]); /*overflow checked*/
                acc *= total_size[i+1];
                skip += acc * (offset ? (hsize_t)offset[i] : 0);
            }
            break;
    } /* end switch */

    /* Set return value */
    ret_value=skip;

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_hyper_eq
 *
 * Purpose:	Determines whether two hyperslabs are equal.  This function
 *		assumes that both hyperslabs are relative to the same array,
 *		for if not, they could not possibly be equal.
 *
 * Return:	Success:	TRUE if the hyperslabs are equal (that is,
 *				both refer to exactly the same elements of an
 *				array)
 *
 *				FALSE otherwise.
 *
 *		Failure:	TRUE the rank is zero or if both hyperslabs
 *				are of zero size.
 *
 * Programmer:	Robb Matzke
 *		Friday, October 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5VM_hyper_eq(unsigned n,
	     const hsize_t *offset1, const hsize_t *size1,
	     const hsize_t *offset2, const hsize_t *size2)
{
    hsize_t	nelmts1 = 1, nelmts2 = 1;
    unsigned	i;
    htri_t      ret_value=TRUE;         /* Return value */

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (n == 0) HGOTO_DONE(TRUE)

    for (i=0; i<n; i++) {
	if ((offset1 ? offset1[i] : 0) != (offset2 ? offset2[i] : 0))
	    HGOTO_DONE(FALSE)
	if ((size1 ? size1[i] : 0) != (size2 ? size2[i] : 0))
	    HGOTO_DONE(FALSE)
	if (0 == (nelmts1 *= (size1 ? size1[i] : 0)))
            HGOTO_DONE(FALSE)
	if (0 == (nelmts2 *= (size2 ? size2[i] : 0)))
            HGOTO_DONE(FALSE)
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_hyper_fill
 *
 * Purpose:	Similar to memset() except it operates on hyperslabs...
 *
 *		Fills a hyperslab of array BUF with some value VAL.  BUF
 *		is treated like a C-order array with N dimensions where the
 *		size of each dimension is TOTAL_SIZE[].	 The hyperslab which
 *		will be filled with VAL begins at byte offset OFFSET[] from
 *		the minimum corner of BUF and continues for SIZE[] bytes in
 *		each dimension.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_hyper_fill(unsigned n, const hsize_t *_size,
	       const hsize_t *total_size, const hsize_t *offset, void *_dst,
	       unsigned fill_value)
{
    uint8_t	*dst = (uint8_t*)_dst;	/*cast for ptr arithmetic	*/
    hsize_t	size[H5VM_HYPER_NDIMS];	/*a modifiable copy of _size	*/
    hsize_t	dst_stride[H5VM_HYPER_NDIMS]; /*destination stride info  */
    hsize_t	dst_start;		/*byte offset to start of stride*/
    hsize_t	elmt_size = 1;		/*bytes per element		*/
    herr_t	ret_value;		/*function return status	*/
#ifndef NDEBUG
    unsigned	u;
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(n > 0 && n <= H5VM_HYPER_NDIMS);
    HDassert(_size);
    HDassert(total_size);
    HDassert(dst);
#ifndef NDEBUG
    for (u = 0; u < n; u++) {
        HDassert(_size[u] > 0);
        HDassert(total_size[u] > 0);
    }
#endif

    /* Copy the size vector so we can modify it */
    H5VM_vector_cpy(n, size, _size);

    /* Compute an optimal destination stride vector */
    dst_start = H5VM_hyper_stride(n, size, total_size, offset, dst_stride);
    H5VM_stride_optimize1(&n, &elmt_size, size, dst_stride);

    /* Copy */
    ret_value = H5VM_stride_fill(n, elmt_size, size, dst_stride, dst+dst_start,
			     fill_value);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_hyper_copy
 *
 * Purpose:	Copies a hyperslab from the source to the destination.
 *
 *		A hyperslab is a logically contiguous region of
 *		multi-dimensional size SIZE of an array whose dimensionality
 *		is N and whose total size is DST_TOTAL_SIZE or SRC_TOTAL_SIZE.
 *		The minimum corner of the hyperslab begins at a
 *		multi-dimensional offset from the minimum corner of the DST
 *		(destination) or SRC (source) array.  The sizes and offsets
 *		are assumed to be in C order, that is, the first size/offset
 *		varies the slowest while the last varies the fastest in the
 *		mapping from N-dimensional space to linear space.  This
 *		function assumes that the array elements are single bytes (if
 *		your array has multi-byte elements then add an additional
 *		dimension whose size is that of your element).
 *
 *		The SRC and DST array may be the same array, but the results
 *		are undefined if the source hyperslab overlaps the
 *		destination hyperslab.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *              Unrolled loops for common cases
 *              Quincey Koziol
 *		?, ? ?, 2001?
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_hyper_copy(unsigned n, const hsize_t *_size,

	       /*destination*/
	       const hsize_t *dst_size, const hsize_t *dst_offset,
	       void *_dst,

	       /*source*/
	       const hsize_t *src_size, const hsize_t *src_offset,
	       const void *_src)
{
    const uint8_t *src = (const uint8_t*)_src;	/*cast for ptr arithmtc */
    uint8_t	*dst = (uint8_t*) _dst;		/*cast for ptr arithmtc */
    hsize_t	size[H5VM_HYPER_NDIMS];		/*a modifiable _size	*/
    hsize_t	src_stride[H5VM_HYPER_NDIMS];	/*source stride info	*/
    hsize_t	dst_stride[H5VM_HYPER_NDIMS];	/*dest stride info	*/
    hsize_t	dst_start, src_start;		/*offset to start at	*/
    hsize_t	elmt_size = 1;			/*element size in bytes */
    herr_t	ret_value;			/*return status		*/
#ifndef NDEBUG
    unsigned	u;
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(n > 0 && n <= H5VM_HYPER_NDIMS);
    HDassert(_size);
    HDassert(dst_size);
    HDassert(src_size);
    HDassert(dst);
    HDassert(src);
#ifndef NDEBUG
    for (u = 0; u < n; u++) {
        HDassert(_size[u] > 0);
        HDassert(dst_size[u] > 0);
        HDassert(src_size[u] > 0);
    }
#endif

    /* Copy the size vector so we can modify it */
    H5VM_vector_cpy(n, size, _size);

    /* Compute stride vectors for source and destination */
#ifdef NO_INLINED_CODE
    dst_start = H5VM_hyper_stride(n, size, dst_size, dst_offset, dst_stride);
    src_start = H5VM_hyper_stride(n, size, src_size, src_offset, src_stride);
#else /* NO_INLINED_CODE */
    /* in-line version of two calls to H5VM_hyper_stride() */
    {
        hsize_t	    dst_acc;	/*accumulator				*/
        hsize_t	    src_acc;	/*accumulator				*/
        int        ii;		    /*counter				*/

        /* init */
        HDassert(n>0);
        dst_stride[n-1] = 1;
        src_stride[n-1] = 1;
        dst_start = dst_offset ? dst_offset[n-1] : 0;
        src_start = src_offset ? src_offset[n-1] : 0;

        /* Unroll loop for common cases */
        switch(n) {
            case 2:
                HDassert(dst_size[1]>=size[1]);
                HDassert(src_size[1]>=size[1]);
                dst_stride[0] = dst_size[1] - size[1]; /*overflow checked*/
                src_stride[0] = src_size[1] - size[1]; /*overflow checked*/
                dst_acc = dst_size[1];
                src_acc = src_size[1];
                dst_start += dst_acc * (dst_offset ? dst_offset[0] : 0);
                src_start += src_acc * (src_offset ? src_offset[0] : 0);
                break;

            case 3:
                HDassert(dst_size[2]>=size[2]);
                HDassert(src_size[2]>=size[2]);
                dst_stride[1] = dst_size[2] - size[2]; /*overflow checked*/
                src_stride[1] = src_size[2] - size[2]; /*overflow checked*/
                dst_acc = dst_size[2];
                src_acc = src_size[2];
                dst_start += dst_acc * (dst_offset ? dst_offset[1] : 0);
                src_start += src_acc * (src_offset ? src_offset[1] : 0);

                HDassert(dst_size[1]>=size[1]);
                HDassert(src_size[1]>=size[1]);
                dst_stride[0] = dst_acc * (dst_size[1] - size[1]); /*overflow checked*/
                src_stride[0] = src_acc * (src_size[1] - size[1]); /*overflow checked*/
                dst_acc *= dst_size[1];
                src_acc *= src_size[1];
                dst_start += dst_acc * (dst_offset ? dst_offset[0] : 0);
                src_start += src_acc * (src_offset ? src_offset[0] : 0);
                break;

            case 4:
                HDassert(dst_size[3]>=size[3]);
                HDassert(src_size[3]>=size[3]);
                dst_stride[2] = dst_size[3] - size[3]; /*overflow checked*/
                src_stride[2] = src_size[3] - size[3]; /*overflow checked*/
                dst_acc = dst_size[3];
                src_acc = src_size[3];
                dst_start += dst_acc * (dst_offset ? dst_offset[2] : 0);
                src_start += src_acc * (src_offset ? src_offset[2] : 0);

                HDassert(dst_size[2]>=size[2]);
                HDassert(src_size[2]>=size[2]);
                dst_stride[1] = dst_acc * (dst_size[2] - size[2]); /*overflow checked*/
                src_stride[1] = src_acc * (src_size[2] - size[2]); /*overflow checked*/
                dst_acc *= dst_size[2];
                src_acc *= src_size[2];
                dst_start += dst_acc * (dst_offset ? dst_offset[1] : 0);
                src_start += src_acc * (src_offset ? src_offset[1] : 0);

                HDassert(dst_size[1]>=size[1]);
                HDassert(src_size[1]>=size[1]);
                dst_stride[0] = dst_acc * (dst_size[1] - size[1]); /*overflow checked*/
                src_stride[0] = src_acc * (src_size[1] - size[1]); /*overflow checked*/
                dst_acc *= dst_size[1];
                src_acc *= src_size[1];
                dst_start += dst_acc * (dst_offset ? dst_offset[0] : 0);
                src_start += src_acc * (src_offset ? src_offset[0] : 0);
                break;

            default:
                /* others */
                for (ii=(int)(n-2), dst_acc=1, src_acc=1; ii>=0; --ii) {
                    HDassert(dst_size[ii+1]>=size[ii+1]);
                    HDassert(src_size[ii+1]>=size[ii+1]);
                    dst_stride[ii] = dst_acc * (dst_size[ii+1] - size[ii+1]); /*overflow checked*/
                    src_stride[ii] = src_acc * (src_size[ii+1] - size[ii+1]); /*overflow checked*/
                    dst_acc *= dst_size[ii+1];
                    src_acc *= src_size[ii+1];
                    dst_start += dst_acc * (dst_offset ? dst_offset[ii] : 0);
                    src_start += src_acc * (src_offset ? src_offset[ii] : 0);
                }
                break;
        } /* end switch */
    }
#endif /* NO_INLINED_CODE */

    /* Optimize the strides as a pair */
    H5VM_stride_optimize2(&n, &elmt_size, size, dst_stride, src_stride);

    /* Perform the copy in terms of stride */
    ret_value = H5VM_stride_copy(n, elmt_size, size,
             dst_stride, dst+dst_start, src_stride, src+src_start);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_fill
 *
 * Purpose:	Fills all bytes of a hyperslab with the same value using
 *		memset().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_stride_fill(unsigned n, hsize_t elmt_size, const hsize_t *size,
		const hsize_t *stride, void *_dst, unsigned fill_value)
{
    uint8_t	*dst = (uint8_t*)_dst; 	/*cast for ptr arithmetic	*/
    hsize_t	idx[H5VM_HYPER_NDIMS]; 	/*1-origin indices		*/
    hsize_t	nelmts;			/*number of elements to fill	*/
    hsize_t	i;			/*counter			*/
    int	j;			/*counter			*/
    hbool_t	carry;			/*subtraction carray value	*/

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(elmt_size < SIZET_MAX);

    H5VM_vector_cpy(n, idx, size);
    nelmts = H5VM_vector_reduce_product(n, size);
    for (i=0; i<nelmts; i++) {
        /* Copy an element */
        H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
        HDmemset(dst, (int)fill_value, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */

        /* Decrement indices and advance pointer */
        for (j=(int)(n-1), carry=TRUE; j>=0 && carry; --j) {
            dst += stride[j];

            if (--idx[j])
                carry = FALSE;
            else {
                HDassert(size);
                idx[j] = size[j];
            } /* end else */
        }
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_copy
 *
 * Purpose:	Uses DST_STRIDE and SRC_STRIDE to advance through the arrays
 *		DST and SRC while copying bytes from SRC to DST.  This
 *		function minimizes the number of calls to memcpy() by
 *		combining various strides, but it will never touch memory
 *		outside the hyperslab defined by the strides.
 *
 * Note:	If the src_stride is all zero and elmt_size is one, then it's
 *		probably more efficient to use H5VM_stride_fill() instead.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_stride_copy(unsigned n, hsize_t elmt_size, const hsize_t *size,
		const hsize_t *dst_stride, void *_dst,
		const hsize_t *src_stride, const void *_src)
{
    uint8_t	*dst = (uint8_t*)_dst;		/*cast for ptr arithmetic*/
    const uint8_t *src = (const uint8_t*) _src;	/*cast for ptr arithmetic*/
    hsize_t	idx[H5VM_HYPER_NDIMS];		/*1-origin indices	*/
    hsize_t	nelmts;				/*num elements to copy	*/
    hsize_t	i;				/*counter		*/
    int	j;				/*counters		*/
    hbool_t	carry;				/*carray for subtraction*/

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(elmt_size<SIZET_MAX);

    if (n) {
        H5VM_vector_cpy(n, idx, size);
        nelmts = H5VM_vector_reduce_product(n, size);
        for (i=0; i<nelmts; i++) {

            /* Copy an element */
            H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
            HDmemcpy(dst, src, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */

            /* Decrement indices and advance pointers */
            for (j=(int)(n-1), carry=TRUE; j>=0 && carry; --j) {
                src += src_stride[j];
                dst += dst_stride[j];

                if (--idx[j])
                    carry = FALSE;
                else {
                    HDassert(size);
                    idx[j] = size[j];
                }
            }
        }
    } else {
        H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
        HDmemcpy (dst, src, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_copy_s
 *
 * Purpose:	Uses DST_STRIDE and SRC_STRIDE to advance through the arrays
 *		DST and SRC while copying bytes from SRC to DST.  This
 *		function minimizes the number of calls to memcpy() by
 *		combining various strides, but it will never touch memory
 *		outside the hyperslab defined by the strides.
 *
 * Note:	If the src_stride is all zero and elmt_size is one, then it's
 *		probably more efficient to use H5VM_stride_fill() instead.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_stride_copy_s(unsigned n, hsize_t elmt_size, const hsize_t *size,
		const hssize_t *dst_stride, void *_dst,
		const hssize_t *src_stride, const void *_src)
{
    uint8_t	*dst = (uint8_t*)_dst;		/*cast for ptr arithmetic*/
    const uint8_t *src = (const uint8_t*) _src;	/*cast for ptr arithmetic*/
    hsize_t	idx[H5VM_HYPER_NDIMS];		/*1-origin indices	*/
    hsize_t	nelmts;				/*num elements to copy	*/
    hsize_t	i;				/*counter		*/
    int	j;				/*counters		*/
    hbool_t	carry;				/*carray for subtraction*/

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(elmt_size<SIZET_MAX);

    if (n) {
        H5VM_vector_cpy(n, idx, size);
        nelmts = H5VM_vector_reduce_product(n, size);
        for (i=0; i<nelmts; i++) {

            /* Copy an element */
            H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
            HDmemcpy(dst, src, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */

            /* Decrement indices and advance pointers */
            for (j=(int)(n-1), carry=TRUE; j>=0 && carry; --j) {
                src += src_stride[j];
                dst += dst_stride[j];

                if (--idx[j])
                    carry = FALSE;
                else {
                    HDassert(size);
                    idx[j] = size[j];
                }
            }
        }
    } else {
        H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
        HDmemcpy (dst, src, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}

#ifdef LATER

/*-------------------------------------------------------------------------
 * Function:	H5VM_stride_copy2
 *
 * Purpose:	Similar to H5VM_stride_copy() except the source and
 *		destination each have their own dimensionality and size and
 *		we copy exactly NELMTS elements each of size ELMT_SIZE.	 The
 *		size counters wrap if NELMTS is more than a size counter.
 *
 * Return:	None
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5VM_stride_copy2(hsize_t nelmts, hsize_t elmt_size,

		 /* destination */
		 unsigned dst_n, const hsize_t *dst_size,
		 const hsize_t *dst_stride,
		 void *_dst,

		 /* source */
		 unsigned src_n, const hsize_t *src_size,
		 const hsize_t *src_stride,
		 const void *_src)
{
    uint8_t	*dst = (uint8_t *) _dst;
    const uint8_t *src = (const uint8_t *) _src;
    hsize_t	dst_idx[H5VM_HYPER_NDIMS];
    hsize_t	src_idx[H5VM_HYPER_NDIMS];
    hsize_t	i;              /* Local index variable */
    int		j;              /* Local index variable */
    hbool_t	carry;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(elmt_size < SIZET_MAX);
    HDassert(dst_n>0);
    HDassert(src_n>0);

    H5VM_vector_cpy(dst_n, dst_idx, dst_size);
    H5VM_vector_cpy(src_n, src_idx, src_size);

    for (i=0; i<nelmts; i++) {

	/* Copy an element */
        H5_CHECK_OVERFLOW(elmt_size,hsize_t,size_t);
	HDmemcpy(dst, src, (size_t)elmt_size); /*lint !e671 The elmt_size will be OK */

	/* Decrement indices and advance pointers */
	for (j=(int)(dst_n-1), carry=TRUE; j>=0 && carry; --j) {
	    dst += dst_stride[j];
	    if (--dst_idx[j])
                carry = FALSE;
	    else {
                HDassert(dst_size);
                dst_idx[j] = dst_size[j];
            } /* end else */
	}
	for (j=(int)(src_n-1), carry=TRUE; j>=0 && carry; --j) {
	    src += src_stride[j];
	    if (--src_idx[j])
                carry = FALSE;
	    else {
                HDassert(src_size);
                src_idx[j] = src_size[j];
            } /* end else */
	}
    }

    FUNC_LEAVE_NOAPI_VOID
}
#endif /* LATER */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_fill
 *
 * Purpose:	Fills all bytes of an array with the same value using
 *		memset(). Increases amount copied by power of two until the
 *		halfway point is crossed, then copies the rest in one swoop.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, June 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_array_fill(void *_dst, const void *src, size_t size, size_t count)
{
    size_t      copy_size;          /* size of the buffer to copy	*/
    size_t      copy_items;         /* number of items currently copying*/
    size_t      items_left;         /* number of items left to copy 	*/
    uint8_t     *dst=(uint8_t*)_dst;/* alias for pointer arithmetic	*/

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(dst);
    HDassert(src);
    HDassert(size < SIZET_MAX && size > 0);
    HDassert(count < SIZET_MAX && count > 0);

    HDmemcpy(dst, src, size);   /* copy first item */

    /* Initialize counters, etc. while compensating for first element copied */
    copy_size = size;
    copy_items = 1;
    items_left = count - 1;
    dst += size;

    /* copy until we've copied at least half of the items */
    while (items_left >= copy_items)
    {
        HDmemcpy(dst, _dst, copy_size);   /* copy the current chunk */
        dst += copy_size;     /* move the offset for the next chunk */
        items_left -= copy_items;   /* decrement the number of items left */

        copy_size *= 2;     /* increase the size of the chunk to copy */
        copy_items *= 2;    /* increase the count of items we are copying */
    }   /* end while */
    if (items_left > 0)   /* if there are any items left to copy */
        HDmemcpy(dst, _dst, items_left * size);

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* H5VM_array_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_down
 *
 * Purpose:	Given a set of dimension sizes, calculate the size of each
 *              "down" slice.  This is the size of the dimensions for all the
 *              dimensions below the current one, which is used for indexing
 *              offsets in this dimension.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, April 28, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_array_down(unsigned n, const hsize_t *total_size, hsize_t *down)
{
    hsize_t	acc;	                /*accumulator			*/
    int	        i;		        /*counter			*/

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(total_size);
    HDassert(down);

    /* Build the sizes of each dimension in the array */
    /* (From fastest to slowest) */
    for(i=(int)(n-1),acc=1; i>=0; i--) {
        down[i]=acc;
        acc *= total_size[i];
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VM_array_down() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_offset_pre
 *
 * Purpose:	Given a coordinate description of a location in an array, this
 *      function returns the byte offset of the coordinate.
 *
 *		The dimensionality of the whole array, and the offset is N.
 *              The whole array dimensions are TOTAL_SIZE and the coordinate
 *              is at offset OFFSET.
 *
 * Return:	Success: Byte offset from beginning of array to element offset
 *		Failure: abort() -- should never fail
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, June 22, 1999
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5VM_array_offset_pre(unsigned n, const hsize_t *acc, const hsize_t *offset)
{
    unsigned        u;		/* Local index variable */
    hsize_t	    ret_value;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(acc);
    HDassert(offset);

    /* Compute offset in array */
    for(u = 0, ret_value = 0; u < n; u++)
        ret_value += acc[u] * offset[u];

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VM_array_offset_pre() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_offset
 *
 * Purpose:	Given a coordinate description of a location in an array, this
 *      function returns the byte offset of the coordinate.
 *
 *		The dimensionality of the whole array, and the offset is N.
 *              The whole array dimensions are TOTAL_SIZE and the coordinate
 *              is at offset OFFSET.
 *
 * Return:	Success: Byte offset from beginning of array to element offset
 *		Failure: abort() -- should never fail
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, June 22, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5VM_array_offset(unsigned n, const hsize_t *total_size, const hsize_t *offset)
{
    hsize_t	acc_arr[H5VM_HYPER_NDIMS];	/* Accumulated size of down dimensions */
    hsize_t	ret_value;  /* Return value */

    FUNC_ENTER_NOAPI((HDabort(), 0)) /*lint !e527 Don't worry about unreachable statement */

    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(total_size);
    HDassert(offset);

    /* Build the sizes of each dimension in the array */
    if(H5VM_array_down(n,total_size,acc_arr)<0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, UFAIL, "can't compute down sizes")

    /* Set return value */
    ret_value=H5VM_array_offset_pre(n,acc_arr,offset);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VM_array_offset() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_calc_pre
 *
 * Purpose:	Given a linear offset in an array, the dimensions of that
 *              array and the pre-computed 'down' (accumulator) sizes, this
 *              function computes the coordinates of that offset in the array.
 *
 *		The dimensionality of the whole array, and the coordinates is N.
 *              The array dimensions are TOTAL_SIZE and the coordinates
 *              are returned in COORD.  The linear offset is in OFFSET.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, July 16, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_array_calc_pre(hsize_t offset, unsigned n, const hsize_t *down,
    hsize_t *coords)
{
    unsigned    u;                      /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(coords);

    /* Compute the coordinates from the offset */
    for(u = 0; u < n; u++) {
        coords[u] = offset / down[u];
        offset %= down[u];
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VM_array_calc_pre() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_array_calc
 *
 * Purpose:	Given a linear offset in an array and the dimensions of that
 *              array, this function computes the coordinates of that offset
 *              in the array.
 *
 *		The dimensionality of the whole array, and the coordinates is N.
 *              The array dimensions are TOTAL_SIZE and the coordinates
 *              are returned in COORD.  The linear offset is in OFFSET.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, April 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VM_array_calc(hsize_t offset, unsigned n, const hsize_t *total_size, hsize_t *coords)
{
    hsize_t	idx[H5VM_HYPER_NDIMS];	/* Size of each dimension in bytes */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(n <= H5VM_HYPER_NDIMS);
    HDassert(total_size);
    HDassert(coords);

    /* Build the sizes of each dimension in the array */
    if(H5VM_array_down(n, total_size, idx) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't compute down sizes")

    /* Compute the coordinates from the offset */
    if(H5VM_array_calc_pre(offset, n, idx, coords) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't compute coordinates")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VM_array_calc() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_chunk_index
 *
 * Purpose:	Given a coordinate offset (COORD), the size of each chunk
 *              (CHUNK), the number of chunks in each dimension (NCHUNKS)
 *              and the number of dimensions of all of these (NDIMS), calculate
 *              a "chunk index" for the chunk that the coordinate offset is
 *              located in.
 *
 *              The chunk index starts at 0 and increases according to the
 *              fastest changing dimension, then the next fastest, etc.
 *
 *              For example, with a 3x5 chunk size and 6 chunks in the fastest
 *              changing dimension and 3 chunks in the slowest changing
 *              dimension, the chunk indices are as follows:
 *
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              |  0  |  1  |  2  |  3  |  4  |  5  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              |  6  |  7  |  8  |  9  | 10  | 11  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              | 12  | 13  | 14  | 15  | 16  | 17  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *
 *              The chunk index is placed in the CHUNK_IDX location for return
 *              from this function
 *
 * Return:	Chunk index on success (can't fail)
 *
 * Programmer:	Quincey Koziol
 *		Monday, April 21, 2003
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5VM_chunk_index(unsigned ndims, const hsize_t *coord, const uint32_t *chunk,
    const hsize_t *down_nchunks)
{
    hsize_t scaled_coord[H5VM_HYPER_NDIMS];	/* Scaled, coordinates, in terms of chunks */
    hsize_t chunk_idx;          /* Chunk index computed */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ndims <= H5VM_HYPER_NDIMS);
    HDassert(coord);
    HDassert(chunk);
    HDassert(down_nchunks);

    /* Defer to H5VM_chunk_index_scaled */
    chunk_idx = H5VM_chunk_index_scaled(ndims, coord, chunk, down_nchunks, scaled_coord);
    
    FUNC_LEAVE_NOAPI(chunk_idx)
} /* end H5VM_chunk_index() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_chunk_scaled
 *
 * Purpose:	Compute the scaled coordinates for a chunk offset
 *
 * Return:	<none>
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, November 19, 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VM_chunk_scaled(unsigned ndims, const hsize_t *coord, const uint32_t *chunk,
    hsize_t *scaled)
{
    unsigned u;                 /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ndims <= H5VM_HYPER_NDIMS);
    HDassert(coord);
    HDassert(chunk);
    HDassert(scaled);

    /* Compute the scaled coordinates for actual coordinates */
    /* (Note that the 'scaled' array is an 'OUT' parameter) */
    for(u = 0; u < ndims; u++)
        scaled[u] = coord[u] / chunk[u];

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VM_chunk_scaled() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_chunk_index_scaled
 *
 * Purpose:	Given a coordinate offset (COORD), the size of each chunk
 *              (CHUNK), the number of chunks in each dimension (NCHUNKS)
 *              and the number of dimensions of all of these (NDIMS), calculate
 *              a "chunk index" for the chunk that the coordinate offset is
 *              located in.
 *
 *              The chunk index starts at 0 and increases according to the
 *              fastest changing dimension, then the next fastest, etc.
 *
 *              For example, with a 3x5 chunk size and 6 chunks in the fastest
 *              changing dimension and 3 chunks in the slowest changing
 *              dimension, the chunk indices are as follows:
 *
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              |  0  |  1  |  2  |  3  |  4  |  5  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              |  6  |  7  |  8  |  9  | 10  | 11  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *              |     |     |     |     |     |     |
 *              | 12  | 13  | 14  | 15  | 16  | 17  |
 *              |     |     |     |     |     |     |
 *              +-----+-----+-----+-----+-----+-----+
 *
 *              The chunk index is placed in the CHUNK_IDX location for return
 *              from this function
 *
 * Note:	This routine is identical to H5VM_chunk_index(), except for
 *		caching the scaled information.  Make changes in both places.
 *
 * Return:	Chunk index on success (can't fail)
 *
 * Programmer:	Vailin Choi
 *		Monday, February 9, 2015
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5VM_chunk_index_scaled(unsigned ndims, const hsize_t *coord, const uint32_t *chunk,
    const hsize_t *down_nchunks, hsize_t *scaled)
{
    hsize_t chunk_idx;          /* Computed chunk index */
    unsigned u;                 /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ndims <= H5VM_HYPER_NDIMS);
    HDassert(coord);
    HDassert(chunk);
    HDassert(down_nchunks);
    HDassert(scaled);

    /* Compute the scaled coordinates for actual coordinates */
    /* (Note that the 'scaled' array is an 'OUT' parameter) */
    for(u = 0; u < ndims; u++)
        scaled[u] = coord[u] / chunk[u];

    /* Compute the chunk index */
    chunk_idx = H5VM_array_offset_pre(ndims, down_nchunks, scaled); /*lint !e772 scaled_coord will always be initialized */

    FUNC_LEAVE_NOAPI(chunk_idx)
} /* end H5VM_chunk_index_scaled() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_opvv
 *
 * Purpose:	Perform an operation on a source & destination sequences
 *		of offset/length pairs.  Each set of sequnces has an array
 *		of lengths, an array of offsets, the maximum number of
 *		sequences and the current sequence to start at in the sequence.
 *
 *              There may be different numbers of bytes in the source and
 *              destination sequences, the operation stops when either the
 *              source or destination sequence runs out of information.
 *
 * Note:	The algorithm in this routine is [basically] the same as for
 *		H5VM_memcpyvv().  Changes should be made to both!
 *
 * Return:	Non-negative # of bytes operated on, on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, September 30, 2010
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VM_opvv(size_t dst_max_nseq, size_t *dst_curr_seq, size_t dst_len_arr[],
    hsize_t dst_off_arr[],
    size_t src_max_nseq, size_t *src_curr_seq, size_t src_len_arr[],
    hsize_t src_off_arr[],
    H5VM_opvv_func_t op, void *op_data)
{
    hsize_t *max_dst_off_ptr, *max_src_off_ptr;  /* Pointers to max. source and destination offset locations */
    hsize_t *dst_off_ptr, *src_off_ptr; /* Pointers to source and destination offset arrays */
    size_t *dst_len_ptr, *src_len_ptr;  /* Pointers to source and destination length arrays */
    hsize_t tmp_dst_off, tmp_src_off;   /* Temporary source and destination offset values */
    size_t tmp_dst_len, tmp_src_len;    /* Temporary source and destination length values */
    size_t acc_len;             /* Accumulated length of sequences */
    ssize_t ret_value = 0;      /* Return value (Total size of sequence in bytes) */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(dst_curr_seq);
    HDassert(*dst_curr_seq < dst_max_nseq);
    HDassert(dst_len_arr);
    HDassert(dst_off_arr);
    HDassert(src_curr_seq);
    HDassert(*src_curr_seq < src_max_nseq);
    HDassert(src_len_arr);
    HDassert(src_off_arr);
    HDassert(op);

    /* Set initial offset & length pointers */
    dst_len_ptr = dst_len_arr + *dst_curr_seq;
    dst_off_ptr = dst_off_arr + *dst_curr_seq;
    src_len_ptr = src_len_arr + *src_curr_seq;
    src_off_ptr = src_off_arr + *src_curr_seq;

    /* Get temporary source & destination sequence offsets & lengths */
    tmp_dst_len = *dst_len_ptr;
    tmp_dst_off = *dst_off_ptr;
    tmp_src_len = *src_len_ptr;
    tmp_src_off = *src_off_ptr;

    /* Compute maximum offset pointer values */
    max_dst_off_ptr = dst_off_arr + dst_max_nseq;
    max_src_off_ptr = src_off_arr + src_max_nseq;

/* Work through the sequences */
/* (Choose smallest sequence available initially) */

    /* Source sequence is less than destination sequence */
    if(tmp_src_len < tmp_dst_len) {
src_smaller:
        acc_len = 0;
        do {
            /* Make operator callback */
            if((*op)(tmp_dst_off, tmp_src_off, tmp_src_len, op_data) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTOPERATE, FAIL, "can't perform operation")

            /* Accumulate number of bytes copied */
            acc_len += tmp_src_len;

            /* Update destination length */
            tmp_dst_off += tmp_src_len;
            tmp_dst_len -= tmp_src_len;

            /* Advance source offset & check for being finished */
            src_off_ptr++;
            if(src_off_ptr >= max_src_off_ptr) {
                /* Roll accumulated changes into appropriate counters */
                *dst_off_ptr = tmp_dst_off;
                *dst_len_ptr = tmp_dst_len;

                /* Done with sequences */
                goto finished;
            } /* end if */
            tmp_src_off = *src_off_ptr;

            /* Update source information */
            src_len_ptr++;
            tmp_src_len = *src_len_ptr;
        } while(tmp_src_len < tmp_dst_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_dst_len < tmp_src_len)
            goto dst_smaller;
        else
            goto equal;
    } /* end if */
    /* Destination sequence is less than source sequence */
    else if(tmp_dst_len < tmp_src_len) {
dst_smaller:
        acc_len = 0;
        do {
            /* Make operator callback */
            if((*op)(tmp_dst_off, tmp_src_off, tmp_dst_len, op_data) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTOPERATE, FAIL, "can't perform operation")

            /* Accumulate number of bytes copied */
            acc_len += tmp_dst_len;

            /* Update source length */
            tmp_src_off += tmp_dst_len;
            tmp_src_len -= tmp_dst_len;

            /* Advance destination offset & check for being finished */
            dst_off_ptr++;
            if(dst_off_ptr >= max_dst_off_ptr) {
                /* Roll accumulated changes into appropriate counters */
                *src_off_ptr = tmp_src_off;
                *src_len_ptr = tmp_src_len;

                /* Done with sequences */
                goto finished;
            } /* end if */
            tmp_dst_off = *dst_off_ptr;

            /* Update destination information */
            dst_len_ptr++;
            tmp_dst_len = *dst_len_ptr;
        } while(tmp_dst_len < tmp_src_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_src_len < tmp_dst_len)
            goto src_smaller;
        else
            goto equal;
    } /* end else-if */
    /* Destination sequence and source sequence are same length */
    else {
equal:
        acc_len = 0;
        do {
            /* Make operator callback */
            if((*op)(tmp_dst_off, tmp_src_off, tmp_dst_len, op_data) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTOPERATE, FAIL, "can't perform operation")

            /* Accumulate number of bytes copied */
            acc_len += tmp_dst_len;

            /* Advance source & destination offset & check for being finished */
            src_off_ptr++;
            dst_off_ptr++;
            if(src_off_ptr >= max_src_off_ptr || dst_off_ptr >= max_dst_off_ptr)
                /* Done with sequences */
                goto finished;
            tmp_src_off = *src_off_ptr;
            tmp_dst_off = *dst_off_ptr;

            /* Update source information */
            src_len_ptr++;
            tmp_src_len = *src_len_ptr;

            /* Update destination information */
            dst_len_ptr++;
            tmp_dst_len = *dst_len_ptr;
        } while(tmp_dst_len == tmp_src_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_dst_len < tmp_src_len)
            goto dst_smaller;
        else
            goto src_smaller;
    } /* end else */

finished:
    /* Roll accumulated sequence lengths into return value */
    ret_value += (ssize_t)acc_len;

    /* Update current sequence vectors */
    *dst_curr_seq = (size_t)(dst_off_ptr - dst_off_arr);
    *src_curr_seq = (size_t)(src_off_ptr - src_off_arr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VM_opvv() */


/*-------------------------------------------------------------------------
 * Function:	H5VM_memcpyvv
 *
 * Purpose:	Given source and destination buffers in memory (SRC & DST)
 *              copy sequences of from the source buffer into the destination
 *              buffer.  Each set of sequences has an array of lengths, an
 *              array of offsets, the maximum number of sequences and the
 *              current sequence to start at in the sequence.
 *
 *              There may be different numbers of bytes in the source and
 *              destination sequences, data copying stops when either the
 *              source or destination buffer runs out of sequence information.
 *
 * Note:	The algorithm in this routine is [basically] the same as for
 *		H5VM_opvv().  Changes should be made to both!
 *
 * Return:	Non-negative # of bytes copied on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, May 2, 2003
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VM_memcpyvv(void *_dst,
    size_t dst_max_nseq, size_t *dst_curr_seq, size_t dst_len_arr[], hsize_t dst_off_arr[],
    const void *_src,
    size_t src_max_nseq, size_t *src_curr_seq, size_t src_len_arr[], hsize_t src_off_arr[])
{
    unsigned char *dst;         /* Destination buffer pointer */
    const unsigned char *src;   /* Source buffer pointer */
    hsize_t *max_dst_off_ptr, *max_src_off_ptr;  /* Pointers to max. source and destination offset locations */
    hsize_t *dst_off_ptr, *src_off_ptr;  /* Pointers to source and destination offset arrays */
    size_t *dst_len_ptr, *src_len_ptr;  /* Pointers to source and destination length arrays */
    size_t tmp_dst_len;         /* Temporary dest. length value */
    size_t tmp_src_len;         /* Temporary source length value */
    size_t acc_len;             /* Accumulated length of sequences */
    ssize_t ret_value = 0;      /* Return value (Total size of sequence in bytes) */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(_dst);
    HDassert(dst_curr_seq);
    HDassert(*dst_curr_seq < dst_max_nseq);
    HDassert(dst_len_arr);
    HDassert(dst_off_arr);
    HDassert(_src);
    HDassert(src_curr_seq);
    HDassert(*src_curr_seq < src_max_nseq);
    HDassert(src_len_arr);
    HDassert(src_off_arr);

    /* Set initial offset & length pointers */
    dst_len_ptr = dst_len_arr + *dst_curr_seq;
    dst_off_ptr = dst_off_arr + *dst_curr_seq;
    src_len_ptr = src_len_arr + *src_curr_seq;
    src_off_ptr = src_off_arr + *src_curr_seq;

    /* Get temporary source & destination sequence lengths */
    tmp_dst_len = *dst_len_ptr;
    tmp_src_len = *src_len_ptr;

    /* Compute maximum offset pointer values */
    max_dst_off_ptr = dst_off_arr + dst_max_nseq;
    max_src_off_ptr = src_off_arr + src_max_nseq;

    /* Compute buffer offsets */
    dst = (unsigned char *)_dst + *dst_off_ptr;
    src = (const unsigned char *)_src + *src_off_ptr;

/* Work through the sequences */
/* (Choose smallest sequence available initially) */

    /* Source sequence is less than destination sequence */
    if(tmp_src_len < tmp_dst_len) {
src_smaller:
        acc_len = 0;
        do {
            /* Copy data */
            HDmemcpy(dst, src, tmp_src_len);

            /* Accumulate number of bytes copied */
            acc_len += tmp_src_len;

            /* Update destination length */
            tmp_dst_len -= tmp_src_len;

            /* Advance source offset & check for being finished */
            src_off_ptr++;
            if(src_off_ptr >= max_src_off_ptr) {
                /* Roll accumulated changes into appropriate counters */
                *dst_off_ptr += acc_len;
                *dst_len_ptr = tmp_dst_len;

                /* Done with sequences */
                goto finished;
            } /* end if */

            /* Update destination pointer */
            dst += tmp_src_len;

            /* Update source information */
            src_len_ptr++;
            tmp_src_len = *src_len_ptr;
            src = (const unsigned char *)_src + *src_off_ptr;
        } while(tmp_src_len < tmp_dst_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_dst_len < tmp_src_len)
            goto dst_smaller;
        else
            goto equal;
    } /* end if */
    /* Destination sequence is less than source sequence */
    else if(tmp_dst_len < tmp_src_len) {
dst_smaller:
        acc_len = 0;
        do {
            /* Copy data */
            HDmemcpy(dst, src, tmp_dst_len);

            /* Accumulate number of bytes copied */
            acc_len += tmp_dst_len;

            /* Update source length */
            tmp_src_len -= tmp_dst_len;

            /* Advance destination offset & check for being finished */
            dst_off_ptr++;
            if(dst_off_ptr >= max_dst_off_ptr) {
                /* Roll accumulated changes into appropriate counters */
                *src_off_ptr += acc_len;
                *src_len_ptr = tmp_src_len;

                /* Done with sequences */
                goto finished;
            } /* end if */

            /* Update source pointer */
            src += tmp_dst_len;

            /* Update destination information */
            dst_len_ptr++;
            tmp_dst_len = *dst_len_ptr;
            dst = (unsigned char *)_dst + *dst_off_ptr;
        } while(tmp_dst_len < tmp_src_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_src_len < tmp_dst_len)
            goto src_smaller;
        else
            goto equal;
    } /* end else-if */
    /* Destination sequence and source sequence are same length */
    else {
equal:
        acc_len = 0;
        do {
            /* Copy data */
            HDmemcpy(dst, src, tmp_dst_len);

            /* Accumulate number of bytes copied */
            acc_len += tmp_dst_len;

            /* Advance source & destination offset & check for being finished */
            src_off_ptr++;
            dst_off_ptr++;
            if(src_off_ptr >= max_src_off_ptr || dst_off_ptr >= max_dst_off_ptr)
                /* Done with sequences */
                goto finished;

            /* Update source information */
            src_len_ptr++;
            tmp_src_len = *src_len_ptr;
            src = (const unsigned char *)_src + *src_off_ptr;

            /* Update destination information */
            dst_len_ptr++;
            tmp_dst_len = *dst_len_ptr;
            dst = (unsigned char *)_dst + *dst_off_ptr;
        } while(tmp_dst_len == tmp_src_len);

        /* Roll accumulated sequence lengths into return value */
        ret_value += (ssize_t)acc_len;

        /* Transition to next state */
        if(tmp_dst_len < tmp_src_len)
            goto dst_smaller;
        else
            goto src_smaller;
    } /* end else */

finished:
    /* Roll accumulated sequence lengths into return value */
    ret_value += (ssize_t)acc_len;

    /* Update current sequence vectors */
    *dst_curr_seq = (size_t)(dst_off_ptr - dst_off_arr);
    *src_curr_seq = (size_t)(src_off_ptr - src_off_arr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VM_memcpyvv() */

