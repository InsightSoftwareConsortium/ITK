// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 


#ifndef CHARLS_CONTEXT
#define CHARLS_CONTEXT


#include <cstdint>


//
// Purpose: a JPEG-LS context with it's current statistics.
//
struct JlsContext
{
    int32_t A;
    int32_t B;
    int16_t C;
    int16_t N;

    JlsContext() :
        A(),
        B(),
        C(),
        N(1)
    {
    }


    JlsContext(int32_t a) :
        A(a),
        B(0),
        C(0),
        N(1)
    {
    }


    inlinehint int32_t GetErrorCorrection(int32_t k) const
    {
        if (k != 0)
            return 0;

        return BitWiseSign(2 * B + N - 1);
    }


    inlinehint void UpdateVariables(int32_t errorValue, int32_t NEAR, int32_t NRESET)
    {
        ASSERT(N != 0);

        // For performance work on copies of A,B,N (compiler will use registers).
        int a = A + std::abs(errorValue);
        int b = B + errorValue * (2 * NEAR + 1); 
        int n = N;

        ASSERT(a < 65536 * 256);
        ASSERT(std::abs(b) < 65536 * 256);

        if (n == NRESET)
        {
            a = a >> 1;
            b = b >> 1;
            n = n >> 1;
        }

        A = a;
        n = n + 1;
        N = static_cast<int16_t>(n);

        if (b + n <= 0) 
        {
            b = b + n;
            if (b <= -n)
            {
                b = -n + 1;
            }
            C = C - (C > -128);
        }
        else  if (b > 0)
        {
            b = b - n;
            if (b > 0)
            {
                b = 0;
            }
            C = C + (C < 127);
        }
        B = b;

        ASSERT(N != 0);
    }


    inlinehint int32_t GetGolomb() const
    {
        int32_t Ntest = N;
        int32_t Atest = A;

        if (Ntest >= Atest) return 0;
        if (Ntest << 1 >= Atest) return 1;
        if (Ntest << 2 >= Atest) return 2;
        if (Ntest << 3 >= Atest) return 3;
        if (Ntest << 4 >= Atest) return 4;

        int32_t k = 5;
        for(; (Ntest << k) < Atest; k++)
        {
            ASSERT(k <= 32);
        }
        return k;
    }
};

#endif
