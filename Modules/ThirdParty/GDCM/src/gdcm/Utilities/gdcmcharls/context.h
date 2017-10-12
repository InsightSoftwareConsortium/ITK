// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 


#ifndef CHARLS_CONTEXT
#define CHARLS_CONTEXT

#include <cstdlib>


//
// JlsContext: a JPEG-LS context with it's current statistics.
//
struct JlsContext
{
public:
	JlsContext() 
	{}

 	JlsContext(LONG a) :
		A(a),
		B(0),
		C(0),
		N(1)
	{
	}

	LONG A;
	LONG B;
	short C;
	short N;

	inlinehint LONG GetErrorCorrection(LONG k) const
	{
		if (k != 0)
			return 0;

		return BitWiseSign(2 * B + N - 1);
	}
	

	inlinehint void UpdateVariables(LONG errorValue, LONG NEAR, LONG NRESET)
	{
		ASSERT(N != 0);

		// For performance work on copies of A,B,N (compiler will use registers).
		int a = A + std::abs(errorValue);
		int b = B + errorValue * (2 * NEAR + 1); 
		int n = N;

		ASSERT(a < 65536 * 256);
		ASSERT(abs(b) < 65536 * 256);
		
		if (n == NRESET) 
		{
			a = a >> 1;
			b = b >> 1;
			n = n >> 1;
		}

		A = a;		
		n = n + 1;
		N = (short)n;

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



	inlinehint LONG GetGolomb() const
	{
		LONG Ntest	= N;
		LONG Atest	= A;

		if (Ntest >= Atest) return 0;
		if (Ntest << 1 >= Atest) return 1;
		if (Ntest << 2 >= Atest) return 2;
		if (Ntest << 3 >= Atest) return 3;
		if (Ntest << 4 >= Atest) return 4;

		LONG k = 5;
		for(; (Ntest << k) < Atest; k++) 
		{ 
			ASSERT(k <= 32); 
		};
		return k;
	}

	
};

#endif
