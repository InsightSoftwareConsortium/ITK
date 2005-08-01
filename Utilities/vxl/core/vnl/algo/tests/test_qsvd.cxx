#include <vnl/vnl_math.h> // vnl_math_abs()
#include <testlib/testlib_test.h>
#undef printf // to work around a bug in libintl.h
#include <vcl_cstdio.h>

extern "C"
int sggsvd_(char const *jobu, char const *jobv, char const *jobq, int *m, int *n, int *p,
            int *k, int *l, float *a, int *lda, float *b, int *ldb,
            float *alpha, float *beta, float *u, int *ldu, float *v,
            int *ldv, float *q, int *ldq, float *work, int *iwork,
            int *info);

static
void test_qsvd() {
  float AA[9]={2.f/3, -1.36f/3, .2f/3,   2.8f/3, .4f/3, 1.f/3,   1, .16f, -.2f};
  float BB[9]={.16f, -.224f, -.768f,  .8f, .36f, -.48f,  1.12f, -.168f, -.576f};
  float U[9], V[9], Q[9], Alpha[3], Beta[3], Work[12];
  int m=3, n=3, p=3, k, l, Iwork[3], info;

  vcl_printf("m = 3, n = 3, p = 3\n");
  sggsvd_("U", "V", "Q", &m, &n, &p, &k, &l, AA, &n, BB, &n, Alpha, Beta,
          U, &n, V, &n, Q, &n, Work, Iwork, &info);

  vcl_printf("k = %d, l = %d, return = %d\n", k, l, info);
  testlib_test_assert("(k,l) must be (0,3)", k==0 && l==3);
  testlib_test_assert("sggsvd should return 0", info==0);

  vcl_printf("U = %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n",
             U[0], U[3], U[6], U[1], U[4], U[7], U[2], U[5], U[8]);
  vcl_printf("V = %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n",
             V[0], V[3], V[6], V[1], V[4], V[7], V[2], V[5], V[8]);
  vcl_printf("Q = %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n",
             Q[0], Q[3], Q[6], Q[1], Q[4], Q[7], Q[2], Q[5], Q[8]);
  vcl_printf("D1 = diag(%12g %12g %12g)\n", Alpha[0], Alpha[1], Alpha[2]);
  vcl_printf("D2 = diag(%12g %12g %12g)\n", Beta[0], Beta[1], Beta[2]);
  vcl_printf("R = %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n    %12.7f %12.7f %12.7f\n",
             AA[0], AA[3], AA[6], AA[1], AA[4], AA[7], AA[2], AA[5], AA[8]);

  testlib_test_assert("D1 must be (0.6,0.8,0.6)",
                      vnl_math_abs(Alpha[0]-0.6)<1e-6 &&
                      vnl_math_abs(Alpha[1]-0.8)<1e-6 &&
                      vnl_math_abs(Alpha[2]-0.6)<1e-6);

  testlib_test_assert("D2 must be (0.8,0.6,0.8)",
                      vnl_math_abs(Beta[0]-0.8)<1e-6 &&
                      vnl_math_abs(Beta[1]-0.6)<1e-6 &&
                      vnl_math_abs(Beta[2]-0.8)<1e-6);
}

TESTMAIN(test_qsvd);
