#ifndef EIGEN_MISC_BLAS_H
#define EIGEN_MISC_BLAS_H

extern "C" {

#define BLASFUNC(FUNC) FUNC##_

/* Level 1 routines */

int BLASFUNC(saxpy)(const int *, const float  *, const float  *, const int *, float  *, const int *);
int BLASFUNC(daxpy)(const int *, const double *, const double *, const int *, double *, const int *);
int BLASFUNC(caxpy)(const int *, const float  *, const float  *, const int *, float  *, const int *);
int BLASFUNC(zaxpy)(const int *, const double *, const double *, const int *, double *, const int *);

/* Level 2 routines */

int BLASFUNC(sgemv)(const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(dgemv)(const char *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);
int BLASFUNC(cgemv)(const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(zgemv)(const char *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(strmv)(const char *, const char *, const char *, const int *, const float  *, const int *, float  *, const int *);
int BLASFUNC(dtrmv)(const char *, const char *, const char *, const int *, const double *, const int *, double *, const int *);
int BLASFUNC(ctrmv)(const char *, const char *, const char *, const int *, const float  *, const int *, float  *, const int *);
int BLASFUNC(ztrmv)(const char *, const char *, const char *, const int *, const double *, const int *, double *, const int *);

int BLASFUNC(ssymv)(const char *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(dsymv)(const char *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(chemv)(const char *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(zhemv)(const char *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

/* Level 3 routines */

int BLASFUNC(sgemm)(const char *, const char *, const int *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(dgemm)(const char *, const char *, const int *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);
int BLASFUNC(cgemm)(const char *, const char *, const int *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(zgemm)(const char *, const char *, const int *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(strsm)(const char *, const char *, const char *, const char *, const int *, const int *, const float *,  const float *,  const int *, float *,  const int *);
int BLASFUNC(dtrsm)(const char *, const char *, const char *, const char *, const int *, const int *, const double *, const double *, const int *, double *, const int *);
int BLASFUNC(ctrsm)(const char *, const char *, const char *, const char *, const int *, const int *, const float *,  const float *,  const int *, float *,  const int *);
int BLASFUNC(ztrsm)(const char *, const char *, const char *, const char *, const int *, const int *, const double *, const double *, const int *, double *, const int *);

int BLASFUNC(strmm)(const char *, const char *, const char *, const char *, const int *, const int *, const float *,  const float *,  const int *, float *,  const int *);
int BLASFUNC(dtrmm)(const char *, const char *, const char *, const char *, const int *, const int *, const double *, const double *, const int *, double *, const int *);
int BLASFUNC(ctrmm)(const char *, const char *, const char *, const char *, const int *, const int *, const float *,  const float *,  const int *, float *,  const int *);
int BLASFUNC(ztrmm)(const char *, const char *, const char *, const char *, const int *, const int *, const double *, const double *, const int *, double *, const int *);

int BLASFUNC(ssymm)(const char *, const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(dsymm)(const char *, const char *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(ssyrk)(const char *, const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(dsyrk)(const char *, const char *, const int *, const int *, const double *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(chemm)(const char *, const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(zhemm)(const char *, const char *, const int *, const int *, const double *, const double *, const int *, const double *, const int *, const double *, double *, const int *);

int BLASFUNC(cherk)(const char *, const char *, const int *, const int *, const float  *, const float  *, const int *, const float  *, float  *, const int *);
int BLASFUNC(zherk)(const char *, const char *, const int *, const int *, const double *, const double *, const int *, const double *, double *, const int *);

#undef BLASFUNC

}

#endif
