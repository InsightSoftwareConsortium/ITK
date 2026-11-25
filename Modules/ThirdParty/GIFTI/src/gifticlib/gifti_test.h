#ifndef GIFTI_TEST_H
#define GIFTI_TEST_H

#define CHECK_NEXT_OPT(n,m,str)                                     \
   do { if ( (n) >= (m) ) {                                          \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: 'prog -help'\n");              \
           return 1;      }                                             \
      } while(0)
#define CHECK_NEXT_OPT2(n,m,s1,s2)                                        \
   do { if ( (n) >= (m) ) {                                                \
           fprintf(stderr,"** option '%s': missing parameter '%s'\n",s1,s2);\
           fprintf(stderr,"   consider: 'prog -help'\n");                    \
           return 1;      }                                                   \
      } while(0)

int ewrite_data_line (void *, int, int, int, int, int, FILE *);
int ewrite_many_lines(void **, int, long long, long long, int, FILE *);
int write_surf_file  (giiDataArray *, giiDataArray *, char *, int);

#endif /* GIFTI_TEST_H */
