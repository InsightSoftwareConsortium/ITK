#include <stdio.h>
#include <math.h>

void lbptf_(char* msg)
{
  printf(msg);
}

void lbp1d_(char* msg, int* i)
{
  printf(msg, *i);
}

void lbp1f_(char* msg, double* i)
{
  printf(msg, *i);
}

static void write50(double* v, int n)
{
  int cols = 15;
  double vmax = 0;
  int i;
  double vmaxscale;
  for(i = 0; i < n; ++i)
    if (fabs(v[i]) > vmax)
      vmax = v[i];
  vmaxscale = log(fabs(vmax)) / log(10);
  vmaxscale = pow(10, ceil(vmaxscale) - 1);
  if (vmaxscale != 1.0)
    printf("  %e x\n", vmaxscale);

  for(i = 0; i < n; ++i) {
    if (i > 0 && i%cols == 0)
      printf("\n");
    printf(" %10.5f", v[i] / vmaxscale);
  }
  printf("\n");
}

/*C
//C     -------------------------------------------------------------
//C     THIS ROUTINE PRINTS MONITORING INFORMATION. THE FREQUENCY AND
//C     AMOUNT OF OUTPUT ARE CONTROLLED BY IPRINT.
//C     -------------------------------------------------------------
*/
void lb1_(iprint, iter, nfun, gnorm, n, m, x, f, g, stp, finish)
int *iprint, *iter, *nfun;
double *gnorm;
int *n, *m;
double *x, *f, *g, *stp;
int *finish; /* logical*/
{
  (void)m;
  --iprint;
/* C*/
/*IF (ITER.EQ.0)THEN*/
  if (*iter == 0) {
/*  30   FORMAT(' F= ',1PD10.3,'   GNORM= ',1PD10.3)*/
/*       WRITE(MP,30)F,GNORM*/
    printf(" F = %g, GNORM = %g\n", *f, *gnorm);
/*       IF (IPRINT(2).GE.1)THEN*/
    if (iprint[2] >= 1) {
/*  40   FORMAT(' VECTOR X= ')*/
/*       WRITE(MP,40)*/
      printf(" VECTOR X=\n");
/*       WRITE(MP,50) (X(I),I=1,N)*/
      write50(x, *n);
/*  60   FORMAT(' GRADIENT VECTOR G= ')*/
/*       WRITE(MP,60)*/
      printf(" GRADIENT VECTOR G=\n");
/*       WRITE(MP,50) (G(I),I=1,N)*/
      write50(g, *n);
/*       ENDIF*/
    }
/*  10   FORMAT('*************************************************')*/
    printf("*************************************************\n");
/*  70   FORMAT(/'   I   NFN',4X,'FUNC',8X,'GNORM',7X,'STEPLENGTH'/)*/
/*       WRITE(MP,70)*/
    printf("   I   NFN    FUNC        GNORM       STEPLENGTH\n");
/*ELSE*/
  } else {
/*  IF ((IPRINT(1).EQ.0).AND.(ITER.NE.1.AND..NOT.FINISH))RETURN*/
    if ((iprint[1]==0) && (*iter != 1 && !*finish))
      return;
/*  IF (IPRINT(1).NE.0)THEN*/
    if (iprint[1] != 0) {
/*    IF(MOD(ITER-1,IPRINT(1)).EQ.0.OR.FINISH)THEN*/
      if ((*iter - 1)%iprint[1] == 0 || *finish) {
/*  70  FORMAT(/'   I   NFN',4X,'FUNC',8X,'GNORM',7X,'STEPLENGTH'/)*/
/*      IF(IPRINT(2).GT.1.AND.ITER.GT.1) WRITE(MP,70)*/
        if (iprint[2] > 1 && *iter > 1)
          printf("   I   NFN    FUNC        GNORM       STEPLENGTH\n");
/*  80  FORMAT(2(I4,1X),3X,3(1PD10.3,2X))*/
/*      WRITE(MP,80)ITER,NFUN,F,GNORM,STP*/
        printf("%4d %4d    %10.3f  %10.3f  %10.3f  \n", *iter, *nfun, *f, *gnorm, *stp);
      }
/*    ELSE*/
      else {
/*      RETURN*/
        return;
/*    ENDIF*/
      }
    }
/*  ELSE*/
    else {

/*  70   FORMAT(/'   I   NFN',4X,'FUNC',8X,'GNORM',7X,'STEPLENGTH'/)*/
/*    IF( IPRINT(2).GT.1.AND.FINISH) WRITE(MP,70)*/
      if (iprint[2] > 1 && *finish)
        printf("   I   NFN    FUNC        GNORM       STEPLENGTH\n");

/*  80   FORMAT(2(I4,1X),3X,3(1PD10.3,2X))*/
/*    WRITE(MP,80)ITER,NFUN,F,GNORM,STP*/
      printf("%4d %4d    %10.3f  %10.3f  %10.3f  \n", *iter, *nfun, *f, *gnorm, *stp);
/*  ENDIF*/
    }

/*  IF (IPRINT(2).EQ.2.OR.IPRINT(2).EQ.3)THEN*/
    if (iprint[2] == 2 || iprint[2] == 3) {
/*    IF (FINISH)THEN*/
      if (*finish)
/*  90  FORMAT(' FINAL POINT X= ')*/
/*      WRITE(MP,90)*/
        printf(" FINAL POINT X=\n");
/*    ELSE*/
      else
/*  40  FORMAT(' VECTOR X= ')*/
/*      WRITE(MP,40)*/
        printf(" VECTOR X=\n");
/*    ENDIF*/

/*  50   FORMAT(6(2X,1PD10.3))*/
/*     WRITE(MP,50)(X(I),I=1,N)*/
      write50(x, *n);
/*     IF (IPRINT(2).EQ.3)THEN*/
      if (iprint[2] == 3) {
/*  60   FORMAT(' GRADIENT VECTOR G= ')*/
/*       WRITE(MP,60)*/
        printf(" GRADIENT VECTOR G=\n");
/*  50   FORMAT(6(2X,1PD10.3))*/
/*       WRITE(MP,50)(G(I),I=1,N)*/
        write50(g, *n);
/*     ENDIF*/
      }
/*  ENDIF*/
    }
/*  100  FORMAT(/' THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS.',*/
/* .       /' IFLAG = 0')*/
/*  IF (FINISH) WRITE(MP,100)*/
    if (*finish)
      printf(" THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS.\n");
  }
/*  ENDIF*/
/* C*/
/*  RETURN*/
/*  END*/
}
