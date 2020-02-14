typedef struct v3p_netlib_rpoly_global_s v3p_netlib_rpoly_global_t;

/*: Finds the zeros of a real polynomial */
extern void v3p_netlib_rpoly_(
  v3p_netlib_doublereal *op,
  v3p_netlib_integer *degree,
  v3p_netlib_doublereal *zeror,
  v3p_netlib_doublereal *zeroi,
  v3p_netlib_logical *fail,
  v3p_netlib_rpoly_global_t* v3p_netlib_rpoly_global_arg
  );
struct v3p_netlib_rpoly_global_s
{
  v3p_netlib_doublereal p[101], qp[101], k[101], qk[101], svk[101];
  v3p_netlib_doublereal sr, si, u, v, a, b, c, d;
  v3p_netlib_doublereal a1, a2, a3, a6, a7, e, f, g;
  v3p_netlib_doublereal h, szr, szi, lzr, lzi;
  v3p_netlib_real eta, are, mre;
  v3p_netlib_integer n, nn;
};
