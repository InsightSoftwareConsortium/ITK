// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 



#ifndef CHARLS_LOSSLESSTRAITS
#define CHARLS_LOSSLESSTRAITS

#include "defaulttraits.h"

//
// optimized trait classes for lossless compression of 8 bit color and 8/16 bit monochrome images.
// This class is assumes MAXVAL correspond to a whole number of bits, and no custom RESET value is set when encoding.
// The point of this is to have the most optimized code for the most common and most demanding scenario. 

template<typename sample, int32_t bitsperpixel>
struct LosslessTraitsImplT 
{
    typedef sample SAMPLE;
    enum
    {
        NEAR  = 0,
        bpp   = bitsperpixel,
        qbpp  = bitsperpixel,
        RANGE = (1 << bpp),
        MAXVAL= (1 << bpp) - 1,
        LIMIT = 2 * (bitsperpixel + MAX(8,bitsperpixel)),
        RESET = BASIC_RESET
    };

    static inlinehint int32_t ComputeErrVal(int32_t d)
    {
        return ModuloRange(d);
    }

    static inlinehint bool IsNear(int32_t lhs, int32_t rhs)
    {
        return lhs == rhs;
    }

// The following optimalization is implementation-dependent (works on x86 and ARM, see charlstest). 
#if defined(__clang__) && (__clang_major__ > 7)
     __attribute__((no_sanitize("shift")))
#endif    
    static inlinehint int32_t ModuloRange(int32_t errorValue)
    {
        return int32_t(errorValue << (INT32_BITCOUNT - bpp)) >> (INT32_BITCOUNT - bpp); 
    }

    static inlinehint SAMPLE ComputeReconstructedSample(int32_t Px, int32_t ErrVal)
    {
        return SAMPLE(MAXVAL & (Px + ErrVal)); 
    }

    static inlinehint int32_t CorrectPrediction(int32_t Pxc) 
    {
        if ((Pxc & MAXVAL) == Pxc)
            return Pxc;

        return (~(Pxc >> (INT32_BITCOUNT-1))) & MAXVAL;
    }
};


template<typename SAMPLE, int32_t bpp>
struct LosslessTraitsT : LosslessTraitsImplT<SAMPLE, bpp> 
{
    typedef SAMPLE PIXEL;
};


template<>
struct LosslessTraitsT<uint8_t, 8> : LosslessTraitsImplT<uint8_t, 8>
{
    typedef SAMPLE PIXEL;

    static inlinehint signed char ModRange(int32_t Errval) 
    {
        return static_cast<signed char>(Errval);
    }

    static inlinehint int32_t ComputeErrVal(int32_t d)
    {
        return static_cast<signed char>(d);
    }

    static inlinehint uint8_t ComputeReconstructedSample(int32_t Px, int32_t ErrVal)
    {
        return uint8_t(Px + ErrVal);
    }
};


template<>
struct LosslessTraitsT<uint16_t, 16> : LosslessTraitsImplT<uint16_t, 16>
{
    typedef SAMPLE PIXEL;

    static inlinehint short ModRange(int32_t Errval) 
    {
        return short(Errval);
    }

    static inlinehint int32_t ComputeErrVal(int32_t d)
    {
        return short(d);
    }

    static inlinehint SAMPLE ComputeReconstructedSample(int32_t Px, int32_t ErrVal)
    {
        return SAMPLE(Px + ErrVal);
    }
};


template<typename SAMPLE, int32_t bpp>
struct LosslessTraitsT<Triplet<SAMPLE>, bpp> : LosslessTraitsImplT<SAMPLE, bpp>
{
    typedef Triplet<SAMPLE> PIXEL;

    static inlinehint bool IsNear(int32_t lhs, int32_t rhs)
    {
        return lhs == rhs;
    }

    static inlinehint bool IsNear(PIXEL lhs, PIXEL rhs)
    {
        return lhs == rhs;
    }

    static inlinehint SAMPLE ComputeReconstructedSample(int32_t Px, int32_t ErrVal)
    {
        return SAMPLE(Px + ErrVal);
    }
};

#endif
