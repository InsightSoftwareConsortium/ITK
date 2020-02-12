// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 


#ifndef CHARLS_DEFAULTTRAITS
#define CHARLS_DEFAULTTRAITS


#include "util.h"
#include <cstdlib>


const int BASIC_RESET = 64; // Default value as defined in ITU T.87, table C.2


// Default traits that support all JPEG LS parameters: custom limit, near, maxval (not power of 2)

// This traits class is used to initialize a coder/decoder.
// The coder/decoder also delegates some functions to the traits class.
// This is to allow the traits class to replace the default implementation here with optimized specific implementations.
// This is done for lossless coding/decoding: see losslesstraits.h 

template<typename sample, typename pixel>
struct DefaultTraitsT
{
    typedef sample SAMPLE;
    typedef pixel PIXEL;

    int32_t MAXVAL;
    const int32_t RANGE;
    const int32_t NEAR;
    const int32_t qbpp;
    const int32_t bpp;
    const int32_t LIMIT;
    const int32_t RESET;

    DefaultTraitsT(int32_t max, int32_t near, int32_t reset = BASIC_RESET) :
        MAXVAL(max),
        RANGE((max + 2 * near) / (2 * near + 1) + 1),
        NEAR(near),
        qbpp(log_2(RANGE)),
        bpp(log_2(max)),
        LIMIT(2 * (bpp + MAX(8, bpp))),
        RESET(reset)
    {
    }

    DefaultTraitsT(const DefaultTraitsT& other) :
        MAXVAL(other.MAXVAL),
        RANGE(other.RANGE),
        NEAR(other.NEAR),
        qbpp(other.qbpp),
        bpp(other.bpp),
        LIMIT(other.LIMIT),
        RESET(other.RESET)
    {
    }

    inlinehint int32_t ComputeErrVal(int32_t e) const
    {
        return ModuloRange(Quantize(e));
    }

    inlinehint SAMPLE ComputeReconstructedSample(int32_t Px, int32_t ErrVal) const
    {
        return FixReconstructedValue(Px + DeQuantize(ErrVal)); 
    }

    inlinehint bool IsNear(int32_t lhs, int32_t rhs) const
    {
        return std::abs(lhs - rhs) <= NEAR;
    }

    bool IsNear(Triplet<SAMPLE> lhs, Triplet<SAMPLE> rhs) const
    {
        return std::abs(lhs.v1 - rhs.v1) <= NEAR &&
               std::abs(lhs.v2 - rhs.v2) <= NEAR &&
               std::abs(lhs.v3 - rhs.v3) <= NEAR;
    }

    inlinehint int32_t CorrectPrediction(int32_t Pxc) const
    {
        if ((Pxc & MAXVAL) == Pxc)
            return Pxc;

        return (~(Pxc >> (INT32_BITCOUNT-1))) & MAXVAL;
    }

    /// <summary>
    /// Returns the value of errorValue modulo RANGE. ITU.T.87, A.4.5 (code segment A.9)
    /// </summary>
    inlinehint int32_t ModuloRange(int32_t errorValue) const
    {
        ASSERT(std::abs(errorValue) <= RANGE);

        if (errorValue < 0)
        {
            errorValue += RANGE;
        }
        if (errorValue >= (RANGE + 1) / 2)
        {
            errorValue -= RANGE;
        }

        ASSERT(-RANGE / 2 <= errorValue && errorValue <= (RANGE / 2) - 1);
        return errorValue;
    }

private:
    int32_t Quantize(int32_t Errval) const
    {
        if (Errval > 0)
            return  (Errval + NEAR) / (2 * NEAR + 1);
        else
            return - (NEAR - Errval) / (2 * NEAR + 1);
    }

    inlinehint int32_t DeQuantize(int32_t Errval) const
    {
        return Errval * (2 * NEAR + 1);
    }

    inlinehint SAMPLE FixReconstructedValue(int32_t val) const
    { 
        if (val < -NEAR)
        {
            val = val + RANGE * (2 * NEAR + 1);
        }
        else if (val > MAXVAL + NEAR)
        {
            val = val - RANGE * (2 * NEAR + 1);
        }

        return SAMPLE(CorrectPrediction(val));
    }
};


#endif
