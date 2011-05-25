set(DOCUMENTATION "This module provides interfaces to FFT implementations. In
particular it provides the direct and inverse computations of Fast Fourier
Transforms based on VXL and FFTW. Note that when using the FFTW implementation
you must comply with the GPL license.")

itk_module(ITK-FFT DEPENDS ITK-Common TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
