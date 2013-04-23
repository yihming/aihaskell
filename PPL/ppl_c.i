# 1 "/usr/local/include/ppl_c.h"
# 1 "<command-line>"
# 1 "/usr/local/include/ppl_c.h"
# 146 "/usr/local/include/ppl_c.h"
# 1 "/usr/include/stdio.h" 1 3 4
# 28 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 324 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4
# 325 "/usr/include/features.h" 2 3 4
# 357 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 1 3 4
# 378 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 379 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 2 3 4
# 358 "/usr/include/features.h" 2 3 4
# 389 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 1 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 5 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4




# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs-64.h" 1 3 4
# 10 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4
# 390 "/usr/include/features.h" 2 3 4
# 29 "/usr/include/stdio.h" 2 3 4





# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 213 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 35 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/types.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;







typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
# 131 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/typesizes.h" 1 3 4
# 132 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef long int __swblk_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;

typedef long int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;
# 37 "/usr/include/stdio.h" 2 3 4
# 45 "/usr/include/stdio.h" 3 4
struct _IO_FILE;



typedef struct _IO_FILE FILE;





# 65 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 75 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 15 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 16 "/usr/include/_G_config.h" 2 3 4




# 1 "/usr/include/wchar.h" 1 3 4
# 83 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {

    unsigned int __wch;



    char __wchb[4];
  } __value;
} __mbstate_t;
# 21 "/usr/include/_G_config.h" 2 3 4

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 53 "/usr/include/_G_config.h" 3 4
typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3 4
# 53 "/usr/include/libio.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3 4
# 172 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 182 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 205 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 273 "/usr/include/libio.h" 3 4
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 321 "/usr/include/libio.h" 3 4
  __off64_t _offset;
# 330 "/usr/include/libio.h" 3 4
  void *__pad1;
  void *__pad2;
  void *__pad3;
  void *__pad4;
  size_t __pad5;

  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 366 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 418 "/usr/include/libio.h" 3 4
extern int __underflow (_IO_FILE *);
extern int __uflow (_IO_FILE *);
extern int __overflow (_IO_FILE *, int);
# 462 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp);
extern int _IO_putc (int __c, _IO_FILE *__fp);
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__ , __leaf__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__ , __leaf__));

extern int _IO_peekc_locked (_IO_FILE *__fp);





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
# 492 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
   __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
    __gnuc_va_list);
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t);

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int);

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
# 76 "/usr/include/stdio.h" 2 3 4




typedef __gnuc_va_list va_list;
# 91 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;
# 103 "/usr/include/stdio.h" 3 4
typedef __ssize_t ssize_t;







typedef _G_fpos_t fpos_t;




# 165 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio_lim.h" 1 3 4
# 166 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;







extern int remove (__const char *__filename) __attribute__ ((__nothrow__ , __leaf__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__ , __leaf__));




extern int renameat (int __oldfd, __const char *__old, int __newfd,
       __const char *__new) __attribute__ ((__nothrow__ , __leaf__));








extern FILE *tmpfile (void) ;
# 210 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;
# 228 "/usr/include/stdio.h" 3 4
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 253 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 267 "/usr/include/stdio.h" 3 4






extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes) ;




extern FILE *freopen (__const char *__restrict __filename,
        __const char *__restrict __modes,
        FILE *__restrict __stream) ;
# 296 "/usr/include/stdio.h" 3 4

# 307 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__ , __leaf__)) ;
# 320 "/usr/include/stdio.h" 3 4
extern FILE *fmemopen (void *__s, size_t __len, __const char *__modes)
  __attribute__ ((__nothrow__ , __leaf__)) ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__ , __leaf__)) ;






extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__ , __leaf__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__ , __leaf__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));








extern int fprintf (FILE *__restrict __stream,
      __const char *__restrict __format, ...);




extern int printf (__const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      __const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));





extern int snprintf (char *__restrict __s, size_t __maxlen,
       __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));

# 418 "/usr/include/stdio.h" 3 4
extern int vdprintf (int __fd, __const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, __const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));








extern int fscanf (FILE *__restrict __stream,
     __const char *__restrict __format, ...) ;




extern int scanf (__const char *__restrict __format, ...) ;

extern int sscanf (__const char *__restrict __s,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__ , __leaf__));
# 449 "/usr/include/stdio.h" 3 4
extern int fscanf (FILE *__restrict __stream, __const char *__restrict __format, ...) __asm__ ("" "__isoc99_fscanf")

                               ;
extern int scanf (__const char *__restrict __format, ...) __asm__ ("" "__isoc99_scanf")
                              ;
extern int sscanf (__const char *__restrict __s, __const char *__restrict __format, ...) __asm__ ("" "__isoc99_sscanf") __attribute__ ((__nothrow__ , __leaf__))

                      ;
# 469 "/usr/include/stdio.h" 3 4








extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;





extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (__const char *__restrict __s,
      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
# 500 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vfscanf")



     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vscanf")

     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (__const char *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vsscanf") __attribute__ ((__nothrow__ , __leaf__))



     __attribute__ ((__format__ (__scanf__, 2, 0)));
# 528 "/usr/include/stdio.h" 3 4









extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 556 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 567 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 600 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;






extern char *gets (char *__s) ;

# 662 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
          size_t *__restrict __n, int __delimiter,
          FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
        size_t *__restrict __n, int __delimiter,
        FILE *__restrict __stream) ;







extern __ssize_t getline (char **__restrict __lineptr,
       size_t *__restrict __n,
       FILE *__restrict __stream) ;








extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);





extern int puts (__const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);

# 734 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);

# 770 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 789 "/usr/include/stdio.h" 3 4






extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, __const fpos_t *__pos);
# 812 "/usr/include/stdio.h" 3 4

# 821 "/usr/include/stdio.h" 3 4


extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;








extern void perror (__const char *__s);






# 1 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern __const char *__const sys_errlist[];
# 851 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
# 870 "/usr/include/stdio.h" 3 4
extern FILE *popen (__const char *__command, __const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__ , __leaf__));
# 910 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
# 940 "/usr/include/stdio.h" 3 4

# 147 "/usr/local/include/ppl_c.h" 2
# 1 "/usr/include/gmp.h" 1 3 4
# 67 "/usr/include/gmp.h" 3 4
# 1 "/usr/include/gmp-x86_64.h" 1 3 4
# 52 "/usr/include/gmp-x86_64.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 53 "/usr/include/gmp-x86_64.h" 2 3 4
# 193 "/usr/include/gmp-x86_64.h" 3 4
typedef unsigned long int mp_limb_t;
typedef long int mp_limb_signed_t;


typedef unsigned long int mp_bitcnt_t;




typedef struct
{
  int _mp_alloc;

  int _mp_size;


  mp_limb_t *_mp_d;
} __mpz_struct;




typedef __mpz_struct MP_INT;
typedef __mpz_struct mpz_t[1];

typedef mp_limb_t * mp_ptr;
typedef const mp_limb_t * mp_srcptr;







typedef long int mp_size_t;
typedef long int mp_exp_t;


typedef struct
{
  __mpz_struct _mp_num;
  __mpz_struct _mp_den;
} __mpq_struct;

typedef __mpq_struct MP_RAT;
typedef __mpq_struct mpq_t[1];

typedef struct
{
  int _mp_prec;



  int _mp_size;


  mp_exp_t _mp_exp;
  mp_limb_t *_mp_d;
} __mpf_struct;


typedef __mpf_struct mpf_t[1];


typedef enum
{
  GMP_RAND_ALG_DEFAULT = 0,
  GMP_RAND_ALG_LC = GMP_RAND_ALG_DEFAULT
} gmp_randalg_t;


typedef struct
{
  mpz_t _mp_seed;
  gmp_randalg_t _mp_alg;
  union {
    void *_mp_lc;
  } _mp_algdata;
} __gmp_randstate_struct;
typedef __gmp_randstate_struct gmp_randstate_t[1];



typedef const __mpz_struct *mpz_srcptr;
typedef __mpz_struct *mpz_ptr;
typedef const __mpf_struct *mpf_srcptr;
typedef __mpf_struct *mpf_ptr;
typedef const __mpq_struct *mpq_srcptr;
typedef __mpq_struct *mpq_ptr;
# 542 "/usr/include/gmp-x86_64.h" 3 4
 void __gmp_set_memory_functions (void *(*) (size_t), void *(*) (void *, size_t, size_t), void (*) (void *, size_t))

                                                   ;


 void __gmp_get_memory_functions (void *(**) (size_t), void *(**) (void *, size_t, size_t), void (**) (void *, size_t))

                                                                                ;


 extern const int __gmp_bits_per_limb;


 extern int __gmp_errno;


 extern const char * const __gmp_version;






 void __gmp_randinit (gmp_randstate_t, gmp_randalg_t, ...);


 void __gmp_randinit_default (gmp_randstate_t);


 void __gmp_randinit_lc_2exp (gmp_randstate_t, mpz_srcptr, unsigned long int, mp_bitcnt_t)

                          ;


 int __gmp_randinit_lc_2exp_size (gmp_randstate_t, mp_bitcnt_t);


 void __gmp_randinit_mt (gmp_randstate_t);


 void __gmp_randinit_set (gmp_randstate_t, const __gmp_randstate_struct *);


 void __gmp_randseed (gmp_randstate_t, mpz_srcptr);


 void __gmp_randseed_ui (gmp_randstate_t, unsigned long int);


 void __gmp_randclear (gmp_randstate_t);


 unsigned long __gmp_urandomb_ui (gmp_randstate_t, unsigned long);


 unsigned long __gmp_urandomm_ui (gmp_randstate_t, unsigned long);





 int __gmp_asprintf (char **, const char *, ...);



 int __gmp_fprintf (FILE *, const char *, ...);
# 621 "/usr/include/gmp-x86_64.h" 3 4
 int __gmp_printf (const char *, ...);


 int __gmp_snprintf (char *, size_t, const char *, ...);


 int __gmp_sprintf (char *, const char *, ...);
# 659 "/usr/include/gmp-x86_64.h" 3 4
 int __gmp_fscanf (FILE *, const char *, ...);



 int __gmp_scanf (const char *, ...);


 int __gmp_sscanf (const char *, const char *, ...);
# 688 "/usr/include/gmp-x86_64.h" 3 4
 void *__gmpz_realloc (mpz_ptr, mp_size_t);



 void __gmpz_abs (mpz_ptr, mpz_srcptr);



 void __gmpz_add (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_add_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_addmul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_addmul_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_and (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_array_init (mpz_ptr, mp_size_t, mp_size_t);


 void __gmpz_bin_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_bin_uiui (mpz_ptr, unsigned long int, unsigned long int);


 void __gmpz_cdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_cdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_cdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_cdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_cdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_cdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_cdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_cdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 unsigned long int __gmpz_cdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_clear (mpz_ptr);


 void __gmpz_clears (mpz_ptr, ...);


 void __gmpz_clrbit (mpz_ptr, mp_bitcnt_t);


 int __gmpz_cmp (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_cmp_d (mpz_srcptr, double) __attribute__ ((__pure__));


 int __gmpz_cmp_si (mpz_srcptr, signed long int) __attribute__ ((__pure__));


 int __gmpz_cmp_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 int __gmpz_cmpabs (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_cmpabs_d (mpz_srcptr, double) __attribute__ ((__pure__));


 int __gmpz_cmpabs_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_com (mpz_ptr, mpz_srcptr);


 void __gmpz_combit (mpz_ptr, mp_bitcnt_t);


 int __gmpz_congruent_p (mpz_srcptr, mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_congruent_2exp_p (mpz_srcptr, mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 int __gmpz_congruent_ui_p (mpz_srcptr, unsigned long, unsigned long) __attribute__ ((__pure__));


 void __gmpz_divexact (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_divexact_ui (mpz_ptr, mpz_srcptr, unsigned long);


 int __gmpz_divisible_p (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_divisible_ui_p (mpz_srcptr, unsigned long) __attribute__ ((__pure__));


 int __gmpz_divisible_2exp_p (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_dump (mpz_srcptr);


 void *__gmpz_export (void *, size_t *, int, size_t, int, size_t, mpz_srcptr);


 void __gmpz_fac_ui (mpz_ptr, unsigned long int);


 void __gmpz_fdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_fdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_fdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_fdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_fdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_fdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_fdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_fdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 unsigned long int __gmpz_fdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_fib_ui (mpz_ptr, unsigned long int);


 void __gmpz_fib2_ui (mpz_ptr, mpz_ptr, unsigned long int);


 int __gmpz_fits_sint_p (mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_fits_slong_p (mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_fits_sshort_p (mpz_srcptr) __attribute__ ((__pure__));



 int __gmpz_fits_uint_p (mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_fits_ulong_p (mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_fits_ushort_p (mpz_srcptr) __attribute__ ((__pure__));



 void __gmpz_gcd (mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_gcd_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_gcdext (mpz_ptr, mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 double __gmpz_get_d (mpz_srcptr) __attribute__ ((__pure__));


 double __gmpz_get_d_2exp (signed long int *, mpz_srcptr);


 long int __gmpz_get_si (mpz_srcptr) __attribute__ ((__pure__));


 char *__gmpz_get_str (char *, int, mpz_srcptr);



 unsigned long int __gmpz_get_ui (mpz_srcptr) __attribute__ ((__pure__));




 mp_limb_t __gmpz_getlimbn (mpz_srcptr, mp_size_t) __attribute__ ((__pure__));



 mp_bitcnt_t __gmpz_hamdist (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 void __gmpz_import (mpz_ptr, size_t, int, size_t, int, size_t, const void *);


 void __gmpz_init (mpz_ptr);


 void __gmpz_init2 (mpz_ptr, mp_bitcnt_t);


 void __gmpz_inits (mpz_ptr, ...);


 void __gmpz_init_set (mpz_ptr, mpz_srcptr);


 void __gmpz_init_set_d (mpz_ptr, double);


 void __gmpz_init_set_si (mpz_ptr, signed long int);


 int __gmpz_init_set_str (mpz_ptr, const char *, int);


 void __gmpz_init_set_ui (mpz_ptr, unsigned long int);



 size_t __gmpz_inp_raw (mpz_ptr, FILE *);




 size_t __gmpz_inp_str (mpz_ptr, FILE *, int);



 int __gmpz_invert (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_ior (mpz_ptr, mpz_srcptr, mpz_srcptr);


 int __gmpz_jacobi (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_kronecker_si (mpz_srcptr, long) __attribute__ ((__pure__));


 int __gmpz_kronecker_ui (mpz_srcptr, unsigned long) __attribute__ ((__pure__));


 int __gmpz_si_kronecker (long, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_ui_kronecker (unsigned long, mpz_srcptr) __attribute__ ((__pure__));


 void __gmpz_lcm (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_lcm_ui (mpz_ptr, mpz_srcptr, unsigned long);




 void __gmpz_lucnum_ui (mpz_ptr, unsigned long int);


 void __gmpz_lucnum2_ui (mpz_ptr, mpz_ptr, unsigned long int);


 int __gmpz_millerrabin (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_mod (mpz_ptr, mpz_srcptr, mpz_srcptr);




 void __gmpz_mul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_mul_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 void __gmpz_mul_si (mpz_ptr, mpz_srcptr, long int);


 void __gmpz_mul_ui (mpz_ptr, mpz_srcptr, unsigned long int);



 void __gmpz_neg (mpz_ptr, mpz_srcptr);



 void __gmpz_nextprime (mpz_ptr, mpz_srcptr);



 size_t __gmpz_out_raw (FILE *, mpz_srcptr);




 size_t __gmpz_out_str (FILE *, int, mpz_srcptr);



 int __gmpz_perfect_power_p (mpz_srcptr) __attribute__ ((__pure__));



 int __gmpz_perfect_square_p (mpz_srcptr) __attribute__ ((__pure__));




 mp_bitcnt_t __gmpz_popcount (mpz_srcptr) __attribute__ ((__pure__));



 void __gmpz_pow_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_powm (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_powm_sec (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_powm_ui (mpz_ptr, mpz_srcptr, unsigned long int, mpz_srcptr);


 int __gmpz_probab_prime_p (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_random (mpz_ptr, mp_size_t);


 void __gmpz_random2 (mpz_ptr, mp_size_t);


 void __gmpz_realloc2 (mpz_ptr, mp_bitcnt_t);


 mp_bitcnt_t __gmpz_remove (mpz_ptr, mpz_srcptr, mpz_srcptr);


 int __gmpz_root (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_rootrem (mpz_ptr,mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_rrandomb (mpz_ptr, gmp_randstate_t, mp_bitcnt_t);


 mp_bitcnt_t __gmpz_scan0 (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpz_scan1 (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_set (mpz_ptr, mpz_srcptr);


 void __gmpz_set_d (mpz_ptr, double);


 void __gmpz_set_f (mpz_ptr, mpf_srcptr);



 void __gmpz_set_q (mpz_ptr, mpq_srcptr);



 void __gmpz_set_si (mpz_ptr, signed long int);


 int __gmpz_set_str (mpz_ptr, const char *, int);


 void __gmpz_set_ui (mpz_ptr, unsigned long int);


 void __gmpz_setbit (mpz_ptr, mp_bitcnt_t);



 size_t __gmpz_size (mpz_srcptr) __attribute__ ((__pure__));



 size_t __gmpz_sizeinbase (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_sqrt (mpz_ptr, mpz_srcptr);


 void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);


 void __gmpz_sub (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_sub_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_ui_sub (mpz_ptr, unsigned long int, mpz_srcptr);


 void __gmpz_submul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_submul_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_swap (mpz_ptr, mpz_ptr) ;


 unsigned long int __gmpz_tdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_tdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_tdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_tdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_tdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_tdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_tdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_tdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_tdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 int __gmpz_tstbit (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_ui_pow_ui (mpz_ptr, unsigned long int, unsigned long int);


 void __gmpz_urandomb (mpz_ptr, gmp_randstate_t, mp_bitcnt_t);


 void __gmpz_urandomm (mpz_ptr, gmp_randstate_t, mpz_srcptr);



 void __gmpz_xor (mpz_ptr, mpz_srcptr, mpz_srcptr);






 void __gmpq_abs (mpq_ptr, mpq_srcptr);



 void __gmpq_add (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_canonicalize (mpq_ptr);


 void __gmpq_clear (mpq_ptr);


 void __gmpq_clears (mpq_ptr, ...);


 int __gmpq_cmp (mpq_srcptr, mpq_srcptr) __attribute__ ((__pure__));


 int __gmpq_cmp_si (mpq_srcptr, long, unsigned long) __attribute__ ((__pure__));


 int __gmpq_cmp_ui (mpq_srcptr, unsigned long int, unsigned long int) __attribute__ ((__pure__));


 void __gmpq_div (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_div_2exp (mpq_ptr, mpq_srcptr, mp_bitcnt_t);


 int __gmpq_equal (mpq_srcptr, mpq_srcptr) __attribute__ ((__pure__));


 void __gmpq_get_num (mpz_ptr, mpq_srcptr);


 void __gmpq_get_den (mpz_ptr, mpq_srcptr);


 double __gmpq_get_d (mpq_srcptr) __attribute__ ((__pure__));


 char *__gmpq_get_str (char *, int, mpq_srcptr);


 void __gmpq_init (mpq_ptr);


 void __gmpq_inits (mpq_ptr, ...);



 size_t __gmpq_inp_str (mpq_ptr, FILE *, int);



 void __gmpq_inv (mpq_ptr, mpq_srcptr);


 void __gmpq_mul (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_mul_2exp (mpq_ptr, mpq_srcptr, mp_bitcnt_t);



 void __gmpq_neg (mpq_ptr, mpq_srcptr);




 size_t __gmpq_out_str (FILE *, int, mpq_srcptr);



 void __gmpq_set (mpq_ptr, mpq_srcptr);


 void __gmpq_set_d (mpq_ptr, double);


 void __gmpq_set_den (mpq_ptr, mpz_srcptr);


 void __gmpq_set_f (mpq_ptr, mpf_srcptr);


 void __gmpq_set_num (mpq_ptr, mpz_srcptr);


 void __gmpq_set_si (mpq_ptr, signed long int, unsigned long int);


 int __gmpq_set_str (mpq_ptr, const char *, int);


 void __gmpq_set_ui (mpq_ptr, unsigned long int, unsigned long int);


 void __gmpq_set_z (mpq_ptr, mpz_srcptr);


 void __gmpq_sub (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_swap (mpq_ptr, mpq_ptr) ;





 void __gmpf_abs (mpf_ptr, mpf_srcptr);


 void __gmpf_add (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_add_ui (mpf_ptr, mpf_srcptr, unsigned long int);

 void __gmpf_ceil (mpf_ptr, mpf_srcptr);


 void __gmpf_clear (mpf_ptr);


 void __gmpf_clears (mpf_ptr, ...);


 int __gmpf_cmp (mpf_srcptr, mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_cmp_d (mpf_srcptr, double) __attribute__ ((__pure__));


 int __gmpf_cmp_si (mpf_srcptr, signed long int) __attribute__ ((__pure__));


 int __gmpf_cmp_ui (mpf_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpf_div (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_div_2exp (mpf_ptr, mpf_srcptr, mp_bitcnt_t);


 void __gmpf_div_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_dump (mpf_srcptr);


 int __gmpf_eq (mpf_srcptr, mpf_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 int __gmpf_fits_sint_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_slong_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_sshort_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_uint_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_ulong_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_ushort_p (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_floor (mpf_ptr, mpf_srcptr);


 double __gmpf_get_d (mpf_srcptr) __attribute__ ((__pure__));


 double __gmpf_get_d_2exp (signed long int *, mpf_srcptr);


 mp_bitcnt_t __gmpf_get_default_prec (void) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpf_get_prec (mpf_srcptr) __attribute__ ((__pure__));


 long __gmpf_get_si (mpf_srcptr) __attribute__ ((__pure__));


 char *__gmpf_get_str (char *, mp_exp_t *, int, size_t, mpf_srcptr);


 unsigned long __gmpf_get_ui (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_init (mpf_ptr);


 void __gmpf_init2 (mpf_ptr, mp_bitcnt_t);


 void __gmpf_inits (mpf_ptr, ...);


 void __gmpf_init_set (mpf_ptr, mpf_srcptr);


 void __gmpf_init_set_d (mpf_ptr, double);


 void __gmpf_init_set_si (mpf_ptr, signed long int);


 int __gmpf_init_set_str (mpf_ptr, const char *, int);


 void __gmpf_init_set_ui (mpf_ptr, unsigned long int);



 size_t __gmpf_inp_str (mpf_ptr, FILE *, int);



 int __gmpf_integer_p (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_mul (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_mul_2exp (mpf_ptr, mpf_srcptr, mp_bitcnt_t);


 void __gmpf_mul_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_neg (mpf_ptr, mpf_srcptr);



 size_t __gmpf_out_str (FILE *, int, size_t, mpf_srcptr);



 void __gmpf_pow_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_random2 (mpf_ptr, mp_size_t, mp_exp_t);


 void __gmpf_reldiff (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_set (mpf_ptr, mpf_srcptr);


 void __gmpf_set_d (mpf_ptr, double);


 void __gmpf_set_default_prec (mp_bitcnt_t) ;


 void __gmpf_set_prec (mpf_ptr, mp_bitcnt_t);


 void __gmpf_set_prec_raw (mpf_ptr, mp_bitcnt_t) ;


 void __gmpf_set_q (mpf_ptr, mpq_srcptr);


 void __gmpf_set_si (mpf_ptr, signed long int);


 int __gmpf_set_str (mpf_ptr, const char *, int);


 void __gmpf_set_ui (mpf_ptr, unsigned long int);


 void __gmpf_set_z (mpf_ptr, mpz_srcptr);


 size_t __gmpf_size (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_sqrt (mpf_ptr, mpf_srcptr);


 void __gmpf_sqrt_ui (mpf_ptr, unsigned long int);


 void __gmpf_sub (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_sub_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_swap (mpf_ptr, mpf_ptr) ;


 void __gmpf_trunc (mpf_ptr, mpf_srcptr);


 void __gmpf_ui_div (mpf_ptr, unsigned long int, mpf_srcptr);


 void __gmpf_ui_sub (mpf_ptr, unsigned long int, mpf_srcptr);


 void __gmpf_urandomb (mpf_t, gmp_randstate_t, mp_bitcnt_t);
# 1501 "/usr/include/gmp-x86_64.h" 3 4
 mp_limb_t __gmpn_add (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t);




 mp_limb_t __gmpn_add_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t) ;



 mp_limb_t __gmpn_add_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_addmul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);



 int __gmpn_cmp (mp_srcptr, mp_srcptr, mp_size_t) __attribute__ ((__pure__));






 mp_limb_t __gmpn_divexact_by3c (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);





 mp_limb_t __gmpn_divrem (mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_divrem_1 (mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t);


 mp_limb_t __gmpn_divrem_2 (mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr);


 mp_size_t __gmpn_gcd (mp_ptr, mp_ptr, mp_size_t, mp_ptr, mp_size_t);


 mp_limb_t __gmpn_gcd_1 (mp_srcptr, mp_size_t, mp_limb_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_gcdext_1 (mp_limb_signed_t *, mp_limb_signed_t *, mp_limb_t, mp_limb_t);


 mp_size_t __gmpn_gcdext (mp_ptr, mp_ptr, mp_size_t *, mp_ptr, mp_size_t, mp_ptr, mp_size_t);


 size_t __gmpn_get_str (unsigned char *, int, mp_ptr, mp_size_t);


 mp_bitcnt_t __gmpn_hamdist (mp_srcptr, mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_lshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);


 mp_limb_t __gmpn_mod_1 (mp_srcptr, mp_size_t, mp_limb_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_mul (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_mul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);


 void __gmpn_mul_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 void __gmpn_sqr (mp_ptr, mp_srcptr, mp_size_t);



 mp_limb_t __gmpn_neg (mp_ptr, mp_srcptr, mp_size_t);




 void __gmpn_com (mp_ptr, mp_srcptr, mp_size_t);



 int __gmpn_perfect_square_p (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 int __gmpn_perfect_power_p (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpn_popcount (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_size_t __gmpn_pow_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);



 mp_limb_t __gmpn_preinv_mod_1 (mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t) __attribute__ ((__pure__));


 void __gmpn_random (mp_ptr, mp_size_t);


 void __gmpn_random2 (mp_ptr, mp_size_t);


 mp_limb_t __gmpn_rshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);


 mp_bitcnt_t __gmpn_scan0 (mp_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpn_scan1 (mp_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_size_t __gmpn_set_str (mp_ptr, const unsigned char *, size_t, int);


 mp_size_t __gmpn_sqrtrem (mp_ptr, mp_ptr, mp_srcptr, mp_size_t);



 mp_limb_t __gmpn_sub (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t);




 mp_limb_t __gmpn_sub_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t) ;



 mp_limb_t __gmpn_sub_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_submul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);


 void __gmpn_tdiv_qr (mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);


 void __gmpn_and_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_andn_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_nand_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_ior_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_iorn_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_nior_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_xor_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_xnor_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 void __gmpn_copyi (mp_ptr, mp_srcptr, mp_size_t);

 void __gmpn_copyd (mp_ptr, mp_srcptr, mp_size_t);

 void __gmpn_zero (mp_ptr, mp_size_t);
# 1681 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__ void
__gmpz_abs (mpz_ptr __gmp_w, mpz_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpz_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_size = ((__gmp_w->_mp_size) >= 0 ? (__gmp_w->_mp_size) : -(__gmp_w->_mp_size));
}
# 1705 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__

int
__gmpz_fits_uint_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= (~ (unsigned) 0)));;
}




extern __inline__

int
__gmpz_fits_ulong_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= (~ (unsigned long) 0)));;
}




extern __inline__

int
__gmpz_fits_ushort_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= ((unsigned short) ~0)));;
}




extern __inline__

unsigned long
__gmpz_get_ui (mpz_srcptr __gmp_z)
{
  mp_ptr __gmp_p = __gmp_z->_mp_d;
  mp_size_t __gmp_n = __gmp_z->_mp_size;
  mp_limb_t __gmp_l = __gmp_p[0];






  return (__gmp_n != 0 ? __gmp_l : 0);
# 1761 "/usr/include/gmp-x86_64.h" 3 4
}




extern __inline__

mp_limb_t
__gmpz_getlimbn (mpz_srcptr __gmp_z, mp_size_t __gmp_n)
{
  mp_limb_t __gmp_result = 0;
  if (__builtin_expect ((__gmp_n >= 0 && __gmp_n < ((__gmp_z->_mp_size) >= 0 ? (__gmp_z->_mp_size) : -(__gmp_z->_mp_size))) != 0, 1))
    __gmp_result = __gmp_z->_mp_d[__gmp_n];
  return __gmp_result;
}



extern __inline__ void
__gmpz_neg (mpz_ptr __gmp_w, mpz_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpz_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_size = - __gmp_w->_mp_size;
}




extern __inline__

int
__gmpz_perfect_square_p (mpz_srcptr __gmp_a)
{
  mp_size_t __gmp_asize;
  int __gmp_result;

  __gmp_asize = __gmp_a->_mp_size;
  __gmp_result = (__gmp_asize >= 0);
  if (__builtin_expect ((__gmp_asize > 0) != 0, 1))
    __gmp_result = __gmpn_perfect_square_p (__gmp_a->_mp_d, __gmp_asize);
  return __gmp_result;
}




extern __inline__

mp_bitcnt_t
__gmpz_popcount (mpz_srcptr __gmp_u)
{
  mp_size_t __gmp_usize;
  mp_bitcnt_t __gmp_result;

  __gmp_usize = __gmp_u->_mp_size;
  __gmp_result = (__gmp_usize < 0 ? (~ (unsigned long) 0) : 0);
  if (__builtin_expect ((__gmp_usize > 0) != 0, 1))
    __gmp_result = __gmpn_popcount (__gmp_u->_mp_d, __gmp_usize);
  return __gmp_result;
}




extern __inline__

void
__gmpz_set_q (mpz_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  __gmpz_tdiv_q (__gmp_w, (&((__gmp_u)->_mp_num)), (&((__gmp_u)->_mp_den)));
}




extern __inline__

size_t
__gmpz_size (mpz_srcptr __gmp_z)
{
  return ((__gmp_z->_mp_size) >= 0 ? (__gmp_z->_mp_size) : -(__gmp_z->_mp_size));
}






extern __inline__ void
__gmpq_abs (mpq_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpq_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_num._mp_size = ((__gmp_w->_mp_num._mp_size) >= 0 ? (__gmp_w->_mp_num._mp_size) : -(__gmp_w->_mp_num._mp_size));
}



extern __inline__ void
__gmpq_neg (mpq_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpq_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_num._mp_size = - __gmp_w->_mp_num._mp_size;
}
# 2103 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__

mp_limb_t
__gmpn_add (mp_ptr __gmp_wp, mp_srcptr __gmp_xp, mp_size_t __gmp_xsize, mp_srcptr __gmp_yp, mp_size_t __gmp_ysize)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x; __gmp_i = (__gmp_ysize); if (__gmp_i != 0) { if (__gmpn_add_n (__gmp_wp, __gmp_xp, __gmp_yp, __gmp_i)) { do { if (__gmp_i >= (__gmp_xsize)) { (__gmp_c) = 1; goto __gmp_done; } __gmp_x = (__gmp_xp)[__gmp_i]; } while ((((__gmp_wp)[__gmp_i++] = (__gmp_x + 1) & ((~ ((mp_limb_t) (0))) >> 0)) == 0)); } } if ((__gmp_wp) != (__gmp_xp)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_xsize); __gmp_j++) (__gmp_wp)[__gmp_j] = (__gmp_xp)[__gmp_j]; } while (0); (__gmp_c) = 0; __gmp_done: ; } while (0);
  return __gmp_c;
}




extern __inline__

mp_limb_t
__gmpn_add_1 (mp_ptr __gmp_dst, mp_srcptr __gmp_src, mp_size_t __gmp_size, mp_limb_t __gmp_n)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_r; __gmp_x = (__gmp_src)[0]; __gmp_r = __gmp_x + (__gmp_n); (__gmp_dst)[0] = __gmp_r; if (((__gmp_r) < ((__gmp_n)))) { (__gmp_c) = 1; for (__gmp_i = 1; __gmp_i < (__gmp_size);) { __gmp_x = (__gmp_src)[__gmp_i]; __gmp_r = __gmp_x + 1; (__gmp_dst)[__gmp_i] = __gmp_r; ++__gmp_i; if (!((__gmp_r) < (1))) { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; break; } } } else { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (1); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; } } while (0);
  return __gmp_c;
}




extern __inline__

int
__gmpn_cmp (mp_srcptr __gmp_xp, mp_srcptr __gmp_yp, mp_size_t __gmp_size)
{
  int __gmp_result;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_y; (__gmp_result) = 0; __gmp_i = (__gmp_size); while (--__gmp_i >= 0) { __gmp_x = (__gmp_xp)[__gmp_i]; __gmp_y = (__gmp_yp)[__gmp_i]; if (__gmp_x != __gmp_y) { (__gmp_result) = (__gmp_x > __gmp_y ? 1 : -1); break; } } } while (0);
  return __gmp_result;
}




extern __inline__

mp_limb_t
__gmpn_sub (mp_ptr __gmp_wp, mp_srcptr __gmp_xp, mp_size_t __gmp_xsize, mp_srcptr __gmp_yp, mp_size_t __gmp_ysize)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x; __gmp_i = (__gmp_ysize); if (__gmp_i != 0) { if (__gmpn_sub_n (__gmp_wp, __gmp_xp, __gmp_yp, __gmp_i)) { do { if (__gmp_i >= (__gmp_xsize)) { (__gmp_c) = 1; goto __gmp_done; } __gmp_x = (__gmp_xp)[__gmp_i]; } while ((((__gmp_wp)[__gmp_i++] = (__gmp_x - 1) & ((~ ((mp_limb_t) (0))) >> 0)), __gmp_x == 0)); } } if ((__gmp_wp) != (__gmp_xp)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_xsize); __gmp_j++) (__gmp_wp)[__gmp_j] = (__gmp_xp)[__gmp_j]; } while (0); (__gmp_c) = 0; __gmp_done: ; } while (0);
  return __gmp_c;
}




extern __inline__

mp_limb_t
__gmpn_sub_1 (mp_ptr __gmp_dst, mp_srcptr __gmp_src, mp_size_t __gmp_size, mp_limb_t __gmp_n)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_r; __gmp_x = (__gmp_src)[0]; __gmp_r = __gmp_x - (__gmp_n); (__gmp_dst)[0] = __gmp_r; if (((__gmp_x) < ((__gmp_n)))) { (__gmp_c) = 1; for (__gmp_i = 1; __gmp_i < (__gmp_size);) { __gmp_x = (__gmp_src)[__gmp_i]; __gmp_r = __gmp_x - 1; (__gmp_dst)[__gmp_i] = __gmp_r; ++__gmp_i; if (!((__gmp_x) < (1))) { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; break; } } } else { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (1); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; } } while (0);
  return __gmp_c;
}




extern __inline__

mp_limb_t
__gmpn_neg (mp_ptr __gmp_rp, mp_srcptr __gmp_up, mp_size_t __gmp_n)
{
  mp_limb_t __gmp_ul, __gmp_cy;
  __gmp_cy = 0;
  do {
      __gmp_ul = *__gmp_up++;
      *__gmp_rp++ = -__gmp_ul - __gmp_cy;
      __gmp_cy |= __gmp_ul != 0;
  } while (--__gmp_n != 0);
  return __gmp_cy;
}
# 2260 "/usr/include/gmp-x86_64.h" 3 4
enum
{
  GMP_ERROR_NONE = 0,
  GMP_ERROR_UNSUPPORTED_ARGUMENT = 1,
  GMP_ERROR_DIVISION_BY_ZERO = 2,
  GMP_ERROR_SQRT_OF_NEGATIVE = 4,
  GMP_ERROR_INVALID_ARGUMENT = 8
};
# 68 "/usr/include/gmp.h" 2 3 4
# 148 "/usr/local/include/ppl_c.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 150 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 325 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef int wchar_t;
# 149 "/usr/local/include/ppl_c.h" 2
# 183 "/usr/local/include/ppl_c.h"
int
ppl_initialize (void);
# 194 "/usr/local/include/ppl_c.h"
int
ppl_finalize (void);
# 205 "/usr/local/include/ppl_c.h"
int
ppl_set_rounding_for_PPL (void);
# 216 "/usr/local/include/ppl_c.h"
int
ppl_restore_pre_PPL_rounding (void);




int
ppl_irrational_precision (unsigned* p);
# 234 "/usr/local/include/ppl_c.h"
int
ppl_set_irrational_precision (unsigned p);
# 292 "/usr/local/include/ppl_c.h"
int
ppl_version_major (void);




int
ppl_version_minor (void);




int
ppl_version_revision (void);




int
ppl_version_beta (void);





int
ppl_version (const char** p);
# 328 "/usr/local/include/ppl_c.h"
int
ppl_banner (const char** p);
# 341 "/usr/local/include/ppl_c.h"
enum ppl_enum_error_code {


  PPL_ERROR_OUT_OF_MEMORY = -2,


  PPL_ERROR_INVALID_ARGUMENT = -3,


  PPL_ERROR_DOMAIN_ERROR = -4,



  PPL_ERROR_LENGTH_ERROR = -5,




  PPL_ARITHMETIC_OVERFLOW = -6,




  PPL_STDIO_ERROR = -7,



  PPL_ERROR_INTERNAL_ERROR = -8,



  PPL_ERROR_UNKNOWN_STANDARD_EXCEPTION = -9,



  PPL_ERROR_UNEXPECTED_ERROR = -10,




  PPL_TIMEOUT_EXCEPTION = -11,





  PPL_ERROR_LOGIC_ERROR = -12
};
# 398 "/usr/local/include/ppl_c.h"
int
ppl_set_error_handler (void (*h)(enum ppl_enum_error_code code, const char* description))
                                  ;
# 426 "/usr/local/include/ppl_c.h"
int
ppl_set_timeout (unsigned csecs);




int
ppl_reset_timeout (void);
# 473 "/usr/local/include/ppl_c.h"
int
ppl_set_deterministic_timeout (unsigned long unscaled_weight, unsigned scale)
                                                         ;




int
ppl_reset_deterministic_timeout (void);
# 506 "/usr/local/include/ppl_c.h"
typedef size_t ppl_dimension_type;




int
ppl_max_space_dimension (ppl_dimension_type* m);




int
ppl_not_a_dimension (ppl_dimension_type* m);




int
ppl_io_print_variable (ppl_dimension_type var);




int
ppl_io_fprint_variable (FILE* stream, ppl_dimension_type var);





int
ppl_io_asprint_variable (char** strp, ppl_dimension_type var);
# 549 "/usr/local/include/ppl_c.h"
typedef const char*
ppl_io_variable_output_function_type(ppl_dimension_type var);




int
ppl_io_set_variable_output_function(ppl_io_variable_output_function_type* p);




int
ppl_io_get_variable_output_function(ppl_io_variable_output_function_type** pp);
# 581 "/usr/local/include/ppl_c.h"
char*
ppl_io_wrap_string(const char* src,
     unsigned indent_depth,
     unsigned preferred_first_line_length,
     unsigned preferred_line_length);
# 607 "/usr/local/include/ppl_c.h"
typedef struct ppl_Coefficient_tag* ppl_Coefficient_t; typedef struct ppl_Coefficient_tag const* ppl_const_Coefficient_t;
# 616 "/usr/local/include/ppl_c.h"
typedef struct ppl_Linear_Expression_tag* ppl_Linear_Expression_t; typedef struct ppl_Linear_Expression_tag const* ppl_const_Linear_Expression_t;
# 625 "/usr/local/include/ppl_c.h"
typedef struct ppl_Constraint_tag* ppl_Constraint_t; typedef struct ppl_Constraint_tag const* ppl_const_Constraint_t;
# 635 "/usr/local/include/ppl_c.h"
typedef struct ppl_Constraint_System_tag* ppl_Constraint_System_t; typedef struct ppl_Constraint_System_tag const* ppl_const_Constraint_System_t;
# 646 "/usr/local/include/ppl_c.h"
typedef struct ppl_Constraint_System_const_iterator_tag* ppl_Constraint_System_const_iterator_t; typedef struct ppl_Constraint_System_const_iterator_tag const* ppl_const_Constraint_System_const_iterator_t;
# 655 "/usr/local/include/ppl_c.h"
typedef struct ppl_Generator_tag* ppl_Generator_t; typedef struct ppl_Generator_tag const* ppl_const_Generator_t;
# 665 "/usr/local/include/ppl_c.h"
typedef struct ppl_Generator_System_tag* ppl_Generator_System_t; typedef struct ppl_Generator_System_tag const* ppl_const_Generator_System_t;
# 676 "/usr/local/include/ppl_c.h"
typedef struct ppl_Generator_System_const_iterator_tag* ppl_Generator_System_const_iterator_t; typedef struct ppl_Generator_System_const_iterator_tag const* ppl_const_Generator_System_const_iterator_t;
# 685 "/usr/local/include/ppl_c.h"
typedef struct ppl_Congruence_tag* ppl_Congruence_t; typedef struct ppl_Congruence_tag const* ppl_const_Congruence_t;
# 695 "/usr/local/include/ppl_c.h"
typedef struct ppl_Congruence_System_tag* ppl_Congruence_System_t; typedef struct ppl_Congruence_System_tag const* ppl_const_Congruence_System_t;
# 706 "/usr/local/include/ppl_c.h"
typedef struct ppl_Congruence_System_const_iterator_tag* ppl_Congruence_System_const_iterator_t; typedef struct ppl_Congruence_System_const_iterator_tag const* ppl_const_Congruence_System_const_iterator_t;
# 715 "/usr/local/include/ppl_c.h"
typedef struct ppl_Grid_Generator_tag* ppl_Grid_Generator_t; typedef struct ppl_Grid_Generator_tag const* ppl_const_Grid_Generator_t;
# 726 "/usr/local/include/ppl_c.h"
typedef struct ppl_Grid_Generator_System_tag* ppl_Grid_Generator_System_t; typedef struct ppl_Grid_Generator_System_tag const* ppl_const_Grid_Generator_System_t;
# 737 "/usr/local/include/ppl_c.h"
typedef struct ppl_Grid_Generator_System_const_iterator_tag* ppl_Grid_Generator_System_const_iterator_t; typedef struct ppl_Grid_Generator_System_const_iterator_tag const* ppl_const_Grid_Generator_System_const_iterator_t;
# 746 "/usr/local/include/ppl_c.h"
typedef struct ppl_MIP_Problem_tag* ppl_MIP_Problem_t; typedef struct ppl_MIP_Problem_tag const* ppl_const_MIP_Problem_t;
# 755 "/usr/local/include/ppl_c.h"
typedef struct ppl_PIP_Problem_tag* ppl_PIP_Problem_t; typedef struct ppl_PIP_Problem_tag const* ppl_const_PIP_Problem_t;
# 764 "/usr/local/include/ppl_c.h"
typedef struct ppl_PIP_Tree_Node_tag* ppl_PIP_Tree_Node_t; typedef struct ppl_PIP_Tree_Node_tag const* ppl_const_PIP_Tree_Node_t;
# 773 "/usr/local/include/ppl_c.h"
typedef struct ppl_PIP_Decision_Node_tag* ppl_PIP_Decision_Node_t; typedef struct ppl_PIP_Decision_Node_tag const* ppl_const_PIP_Decision_Node_t;
# 782 "/usr/local/include/ppl_c.h"
typedef struct ppl_PIP_Solution_Node_tag* ppl_PIP_Solution_Node_t; typedef struct ppl_PIP_Solution_Node_tag const* ppl_const_PIP_Solution_Node_t;
# 792 "/usr/local/include/ppl_c.h"
typedef struct ppl_Artificial_Parameter_tag* ppl_Artificial_Parameter_t; typedef struct ppl_Artificial_Parameter_tag const* ppl_const_Artificial_Parameter_t;
typedef struct ppl_Artificial_Parameter_Sequence_tag* ppl_Artificial_Parameter_Sequence_t; typedef struct ppl_Artificial_Parameter_Sequence_tag const* ppl_const_Artificial_Parameter_Sequence_t;





typedef struct ppl_Artificial_Parameter_Sequence_const_iterator_tag* ppl_Artificial_Parameter_Sequence_const_iterator_t; typedef struct ppl_Artificial_Parameter_Sequence_const_iterator_tag const* ppl_const_Artificial_Parameter_Sequence_const_iterator_t;
# 871 "/usr/local/include/ppl_c.h"
int
ppl_new_Coefficient (ppl_Coefficient_t* pc);






int
ppl_new_Coefficient_from_mpz_t (ppl_Coefficient_t* pc, mpz_t z);





int
ppl_new_Coefficient_from_Coefficient (ppl_Coefficient_t* pc, ppl_const_Coefficient_t c)
                                 ;




int
ppl_assign_Coefficient_from_mpz_t (ppl_Coefficient_t dst, mpz_t z);




int
ppl_assign_Coefficient_from_Coefficient
(ppl_Coefficient_t dst, ppl_const_Coefficient_t src);





int
ppl_delete_Coefficient (ppl_const_Coefficient_t c);
# 918 "/usr/local/include/ppl_c.h"
int
ppl_Coefficient_to_mpz_t (ppl_const_Coefficient_t c, mpz_t z);






int
ppl_Coefficient_OK (ppl_const_Coefficient_t c);





int
ppl_Coefficient_is_bounded (void);





int
ppl_Coefficient_min (mpz_t min);





int
ppl_Coefficient_max (mpz_t max);






int ppl_io_print_Coefficient (ppl_const_Coefficient_t x); int ppl_io_fprint_Coefficient (FILE* stream, ppl_const_Coefficient_t x); int ppl_io_asprint_Coefficient (char** strp, ppl_const_Coefficient_t x);
# 967 "/usr/local/include/ppl_c.h"
int
ppl_new_Linear_Expression (ppl_Linear_Expression_t* ple);






int
ppl_new_Linear_Expression_with_dimension
(ppl_Linear_Expression_t* ple, ppl_dimension_type d);





int
ppl_new_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t* ple, ppl_const_Linear_Expression_t le);





int
ppl_new_Linear_Expression_from_Constraint
(ppl_Linear_Expression_t* ple, ppl_const_Constraint_t c);





int
ppl_new_Linear_Expression_from_Generator
(ppl_Linear_Expression_t* ple, ppl_const_Generator_t g);





int
ppl_new_Linear_Expression_from_Congruence
(ppl_Linear_Expression_t* ple, ppl_const_Congruence_t c);





int
ppl_new_Linear_Expression_from_Grid_Generator
(ppl_Linear_Expression_t* ple, ppl_const_Grid_Generator_t g);




int
ppl_assign_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src);





int
ppl_delete_Linear_Expression (ppl_const_Linear_Expression_t le);
# 1041 "/usr/local/include/ppl_c.h"
int
ppl_Linear_Expression_space_dimension
(ppl_const_Linear_Expression_t le, ppl_dimension_type* m);





int
ppl_Linear_Expression_coefficient (ppl_const_Linear_Expression_t le, ppl_dimension_type var, ppl_Coefficient_t n)

                               ;




int
ppl_Linear_Expression_inhomogeneous_term
(ppl_const_Linear_Expression_t le, ppl_Coefficient_t n);






int
ppl_Linear_Expression_OK (ppl_const_Linear_Expression_t le);




int
ppl_Linear_Expression_is_zero (ppl_const_Linear_Expression_t le);





int
ppl_Linear_Expression_all_homogeneous_terms_are_zero
(ppl_const_Linear_Expression_t le);
# 1093 "/usr/local/include/ppl_c.h"
int
ppl_Linear_Expression_add_to_coefficient
(ppl_Linear_Expression_t le, ppl_dimension_type var, ppl_const_Coefficient_t n)

                               ;




int
ppl_Linear_Expression_add_to_inhomogeneous
(ppl_Linear_Expression_t le, ppl_const_Coefficient_t n);




int
ppl_add_Linear_Expression_to_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src);




int
ppl_subtract_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src);




int
ppl_multiply_Linear_Expression_by_Coefficient
(ppl_Linear_Expression_t le, ppl_const_Coefficient_t n);



int ppl_io_print_Linear_Expression (ppl_const_Linear_Expression_t x); int ppl_io_fprint_Linear_Expression (FILE* stream, ppl_const_Linear_Expression_t x); int ppl_io_asprint_Linear_Expression (char** strp, ppl_const_Linear_Expression_t x); int ppl_Linear_Expression_ascii_dump (ppl_const_Linear_Expression_t x, FILE* stream); int ppl_Linear_Expression_ascii_load (ppl_Linear_Expression_t x, FILE* stream);




enum ppl_enum_Constraint_Type {

  PPL_CONSTRAINT_TYPE_LESS_THAN,

  PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL,

  PPL_CONSTRAINT_TYPE_EQUAL,

  PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL,

  PPL_CONSTRAINT_TYPE_GREATER_THAN
};
# 1155 "/usr/local/include/ppl_c.h"
int
ppl_new_Constraint (ppl_Constraint_t* pc, ppl_const_Linear_Expression_t le, enum ppl_enum_Constraint_Type rel)

                                            ;





int
ppl_new_Constraint_zero_dim_false (ppl_Constraint_t* pc);






int
ppl_new_Constraint_zero_dim_positivity (ppl_Constraint_t* pc);





int
ppl_new_Constraint_from_Constraint (ppl_Constraint_t* pc, ppl_const_Constraint_t c)
                                     ;




int
ppl_assign_Constraint_from_Constraint (ppl_Constraint_t dst, ppl_const_Constraint_t src)
                                   ;





int
ppl_delete_Constraint (ppl_const_Constraint_t c);
# 1205 "/usr/local/include/ppl_c.h"
int
ppl_Constraint_space_dimension (ppl_const_Constraint_t c, ppl_dimension_type* m)
                              ;




int
ppl_Constraint_type (ppl_const_Constraint_t c);





int
ppl_Constraint_coefficient (ppl_const_Constraint_t c, ppl_dimension_type var, ppl_Coefficient_t n)

                               ;




int
ppl_Constraint_inhomogeneous_term (ppl_const_Constraint_t c, ppl_Coefficient_t n)
                               ;






int
ppl_Constraint_OK (ppl_const_Constraint_t c);



int ppl_io_print_Constraint (ppl_const_Constraint_t x); int ppl_io_fprint_Constraint (FILE* stream, ppl_const_Constraint_t x); int ppl_io_asprint_Constraint (char** strp, ppl_const_Constraint_t x); int ppl_Constraint_ascii_dump (ppl_const_Constraint_t x, FILE* stream); int ppl_Constraint_ascii_load (ppl_Constraint_t x, FILE* stream);
# 1251 "/usr/local/include/ppl_c.h"
int
ppl_new_Constraint_System (ppl_Constraint_System_t* pcs);





int
ppl_new_Constraint_System_zero_dim_empty
(ppl_Constraint_System_t* pcs);






int
ppl_new_Constraint_System_from_Constraint
(ppl_Constraint_System_t* pcs, ppl_const_Constraint_t c);





int
ppl_new_Constraint_System_from_Constraint_System
(ppl_Constraint_System_t* pcs, ppl_const_Constraint_System_t cs);




int
ppl_assign_Constraint_System_from_Constraint_System
(ppl_Constraint_System_t dst, ppl_const_Constraint_System_t src);





int
ppl_delete_Constraint_System (ppl_const_Constraint_System_t cs);
# 1301 "/usr/local/include/ppl_c.h"
int
ppl_Constraint_System_space_dimension
(ppl_const_Constraint_System_t cs, ppl_dimension_type* m);





int
ppl_Constraint_System_empty
(ppl_const_Constraint_System_t cs);





int
ppl_Constraint_System_has_strict_inequalities
(ppl_const_Constraint_System_t cs);





int
ppl_Constraint_System_begin
(ppl_const_Constraint_System_t cs, ppl_Constraint_System_const_iterator_t cit)
                                                ;





int
ppl_Constraint_System_end
(ppl_const_Constraint_System_t cs, ppl_Constraint_System_const_iterator_t cit)
                                                ;






int
ppl_Constraint_System_OK (ppl_const_Constraint_System_t cs);
# 1356 "/usr/local/include/ppl_c.h"
int
ppl_Constraint_System_clear (ppl_Constraint_System_t cs);





int
ppl_Constraint_System_insert_Constraint (ppl_Constraint_System_t cs, ppl_const_Constraint_t c)
                                   ;



int ppl_io_print_Constraint_System (ppl_const_Constraint_System_t x); int ppl_io_fprint_Constraint_System (FILE* stream, ppl_const_Constraint_System_t x); int ppl_io_asprint_Constraint_System (char** strp, ppl_const_Constraint_System_t x); int ppl_Constraint_System_ascii_dump (ppl_const_Constraint_System_t x, FILE* stream); int ppl_Constraint_System_ascii_load (ppl_Constraint_System_t x, FILE* stream);
# 1379 "/usr/local/include/ppl_c.h"
int
ppl_new_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t* pcit);





int
ppl_new_Constraint_System_const_iterator_from_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t* pcit, ppl_const_Constraint_System_const_iterator_t cit)
                                                      ;




int
ppl_assign_Constraint_System_const_iterator_from_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t dst, ppl_const_Constraint_System_const_iterator_t src)
                                                      ;





int
ppl_delete_Constraint_System_const_iterator
(ppl_const_Constraint_System_const_iterator_t cit);
# 1417 "/usr/local/include/ppl_c.h"
int
ppl_Constraint_System_const_iterator_dereference
(ppl_const_Constraint_System_const_iterator_t cit, ppl_const_Constraint_t* pc)
                                ;




int
ppl_Constraint_System_const_iterator_increment
(ppl_Constraint_System_const_iterator_t cit);





int
ppl_Constraint_System_const_iterator_equal_test
(ppl_const_Constraint_System_const_iterator_t x, ppl_const_Constraint_System_const_iterator_t y)
                                                    ;







enum ppl_enum_Generator_Type {

  PPL_GENERATOR_TYPE_LINE,

  PPL_GENERATOR_TYPE_RAY,

  PPL_GENERATOR_TYPE_POINT,

  PPL_GENERATOR_TYPE_CLOSURE_POINT
};
# 1466 "/usr/local/include/ppl_c.h"
int
ppl_new_Generator (ppl_Generator_t* pg, ppl_const_Linear_Expression_t le, enum ppl_enum_Generator_Type t, ppl_const_Coefficient_t d)


                                   ;






int
ppl_new_Generator_zero_dim_point (ppl_Generator_t* pg);






int
ppl_new_Generator_zero_dim_closure_point (ppl_Generator_t* pg);





int
ppl_new_Generator_from_Generator (ppl_Generator_t* pg, ppl_const_Generator_t g)
                                  ;




int
ppl_assign_Generator_from_Generator (ppl_Generator_t dst, ppl_const_Generator_t src)
                                       ;





int
ppl_delete_Generator (ppl_const_Generator_t g);
# 1518 "/usr/local/include/ppl_c.h"
int
ppl_Generator_space_dimension (ppl_const_Generator_t g, ppl_dimension_type* m)
                             ;




int
ppl_Generator_type (ppl_const_Generator_t g);




int
ppl_Generator_coefficient (ppl_const_Generator_t g, ppl_dimension_type var, ppl_Coefficient_t n)

                              ;




int
ppl_Generator_divisor (ppl_const_Generator_t g, ppl_Coefficient_t n)
                          ;






int
ppl_Generator_OK (ppl_const_Generator_t g);



int ppl_io_print_Generator (ppl_const_Generator_t x); int ppl_io_fprint_Generator (FILE* stream, ppl_const_Generator_t x); int ppl_io_asprint_Generator (char** strp, ppl_const_Generator_t x); int ppl_Generator_ascii_dump (ppl_const_Generator_t x, FILE* stream); int ppl_Generator_ascii_load (ppl_Generator_t x, FILE* stream);
# 1563 "/usr/local/include/ppl_c.h"
int
ppl_new_Generator_System (ppl_Generator_System_t* pgs);






int
ppl_new_Generator_System_zero_dim_univ
(ppl_Generator_System_t* pgs);






int
ppl_new_Generator_System_from_Generator (ppl_Generator_System_t* pgs, ppl_const_Generator_t g)
                                  ;





int
ppl_new_Generator_System_from_Generator_System
(ppl_Generator_System_t* pgs, ppl_const_Generator_System_t gs);




int
ppl_assign_Generator_System_from_Generator_System
(ppl_Generator_System_t dst, ppl_const_Generator_System_t src);





int
ppl_delete_Generator_System (ppl_const_Generator_System_t gs);
# 1614 "/usr/local/include/ppl_c.h"
int
ppl_Generator_System_space_dimension
(ppl_const_Generator_System_t gs, ppl_dimension_type* m);





int
ppl_Generator_System_empty
(ppl_const_Generator_System_t gs);





int
ppl_Generator_System_begin
(ppl_const_Generator_System_t gs, ppl_Generator_System_const_iterator_t git)
                                               ;





int
ppl_Generator_System_end
(ppl_const_Generator_System_t gs, ppl_Generator_System_const_iterator_t git)
                                               ;






int
ppl_Generator_System_OK (ppl_const_Generator_System_t gs);
# 1661 "/usr/local/include/ppl_c.h"
int
ppl_Generator_System_clear (ppl_Generator_System_t gs);





int
ppl_Generator_System_insert_Generator (ppl_Generator_System_t gs, ppl_const_Generator_t g)
                                ;



int ppl_io_print_Generator_System (ppl_const_Generator_System_t x); int ppl_io_fprint_Generator_System (FILE* stream, ppl_const_Generator_System_t x); int ppl_io_asprint_Generator_System (char** strp, ppl_const_Generator_System_t x); int ppl_Generator_System_ascii_dump (ppl_const_Generator_System_t x, FILE* stream); int ppl_Generator_System_ascii_load (ppl_Generator_System_t x, FILE* stream);
# 1684 "/usr/local/include/ppl_c.h"
int
ppl_new_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t* pgit);





int
ppl_new_Generator_System_const_iterator_from_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t* pgit, ppl_const_Generator_System_const_iterator_t git)
                                                     ;




int
ppl_assign_Generator_System_const_iterator_from_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t dst, ppl_const_Generator_System_const_iterator_t src)
                                                     ;





int
ppl_delete_Generator_System_const_iterator
(ppl_const_Generator_System_const_iterator_t git);
# 1722 "/usr/local/include/ppl_c.h"
int
ppl_Generator_System_const_iterator_dereference
(ppl_const_Generator_System_const_iterator_t git, ppl_const_Generator_t* pg)
                               ;




int
ppl_Generator_System_const_iterator_increment
(ppl_Generator_System_const_iterator_t git);





int
ppl_Generator_System_const_iterator_equal_test
(ppl_const_Generator_System_const_iterator_t x, ppl_const_Generator_System_const_iterator_t y)
                                                   ;
# 1754 "/usr/local/include/ppl_c.h"
int
ppl_new_Congruence (ppl_Congruence_t* pc, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t m)

                                    ;





int
ppl_new_Congruence_zero_dim_false (ppl_Congruence_t* pc);






int
ppl_new_Congruence_zero_dim_integrality (ppl_Congruence_t* pc);





int
ppl_new_Congruence_from_Congruence (ppl_Congruence_t* pc, ppl_const_Congruence_t c)
                                     ;




int
ppl_assign_Congruence_from_Congruence (ppl_Congruence_t dst, ppl_const_Congruence_t src)
                                   ;





int
ppl_delete_Congruence (ppl_const_Congruence_t c);
# 1804 "/usr/local/include/ppl_c.h"
int
ppl_Congruence_space_dimension (ppl_const_Congruence_t c, ppl_dimension_type* m)
                              ;





int
ppl_Congruence_coefficient (ppl_const_Congruence_t c, ppl_dimension_type var, ppl_Coefficient_t n)

                               ;




int
ppl_Congruence_inhomogeneous_term (ppl_const_Congruence_t c, ppl_Coefficient_t n)
                               ;




int
ppl_Congruence_modulus (ppl_const_Congruence_t c, ppl_Coefficient_t m)
                           ;






int
ppl_Congruence_OK (ppl_const_Congruence_t c);



int ppl_io_print_Congruence (ppl_const_Congruence_t x); int ppl_io_fprint_Congruence (FILE* stream, ppl_const_Congruence_t x); int ppl_io_asprint_Congruence (char** strp, ppl_const_Congruence_t x); int ppl_Congruence_ascii_dump (ppl_const_Congruence_t x, FILE* stream); int ppl_Congruence_ascii_load (ppl_Congruence_t x, FILE* stream);
# 1851 "/usr/local/include/ppl_c.h"
int
ppl_new_Congruence_System (ppl_Congruence_System_t* pcs);





int
ppl_new_Congruence_System_zero_dim_empty
(ppl_Congruence_System_t* pcs);






int
ppl_new_Congruence_System_from_Congruence
(ppl_Congruence_System_t* pcs, ppl_const_Congruence_t c);





int
ppl_new_Congruence_System_from_Congruence_System
(ppl_Congruence_System_t* pcs, ppl_const_Congruence_System_t cs);




int
ppl_assign_Congruence_System_from_Congruence_System
(ppl_Congruence_System_t dst, ppl_const_Congruence_System_t src);





int
ppl_delete_Congruence_System (ppl_const_Congruence_System_t cs);
# 1901 "/usr/local/include/ppl_c.h"
int
ppl_Congruence_System_space_dimension
(ppl_const_Congruence_System_t cs, ppl_dimension_type* m);





int
ppl_Congruence_System_empty
(ppl_const_Congruence_System_t cs);





int
ppl_Congruence_System_begin
(ppl_const_Congruence_System_t cs, ppl_Congruence_System_const_iterator_t cit)
                                                ;





int
ppl_Congruence_System_end
(ppl_const_Congruence_System_t cs, ppl_Congruence_System_const_iterator_t cit)
                                                ;






int
ppl_Congruence_System_OK (ppl_const_Congruence_System_t cs);
# 1948 "/usr/local/include/ppl_c.h"
int
ppl_Congruence_System_clear (ppl_Congruence_System_t cs);





int
ppl_Congruence_System_insert_Congruence (ppl_Congruence_System_t cs, ppl_const_Congruence_t c)
                                   ;



int ppl_io_print_Congruence_System (ppl_const_Congruence_System_t x); int ppl_io_fprint_Congruence_System (FILE* stream, ppl_const_Congruence_System_t x); int ppl_io_asprint_Congruence_System (char** strp, ppl_const_Congruence_System_t x); int ppl_Congruence_System_ascii_dump (ppl_const_Congruence_System_t x, FILE* stream); int ppl_Congruence_System_ascii_load (ppl_Congruence_System_t x, FILE* stream);
# 1971 "/usr/local/include/ppl_c.h"
int
ppl_new_Congruence_System_const_iterator
(ppl_Congruence_System_const_iterator_t* pcit);





int
ppl_new_Congruence_System_const_iterator_from_Congruence_System_const_iterator
(ppl_Congruence_System_const_iterator_t* pcit, ppl_const_Congruence_System_const_iterator_t cit)
                                                      ;




int
ppl_assign_Congruence_System_const_iterator_from_Congruence_System_const_iterator
(ppl_Congruence_System_const_iterator_t dst, ppl_const_Congruence_System_const_iterator_t src)
                                                      ;





int
ppl_delete_Congruence_System_const_iterator
(ppl_const_Congruence_System_const_iterator_t cit);
# 2009 "/usr/local/include/ppl_c.h"
int
ppl_Congruence_System_const_iterator_dereference
(ppl_const_Congruence_System_const_iterator_t cit, ppl_const_Congruence_t* pc)
                                ;




int
ppl_Congruence_System_const_iterator_increment
(ppl_Congruence_System_const_iterator_t cit);





int
ppl_Congruence_System_const_iterator_equal_test
(ppl_const_Congruence_System_const_iterator_t x, ppl_const_Congruence_System_const_iterator_t y)
                                                    ;







enum ppl_enum_Grid_Generator_Type {

  PPL_GRID_GENERATOR_TYPE_LINE,

  PPL_GRID_GENERATOR_TYPE_PARAMETER,

  PPL_GRID_GENERATOR_TYPE_POINT
};
# 2056 "/usr/local/include/ppl_c.h"
int
ppl_new_Grid_Generator (ppl_Grid_Generator_t* pg, ppl_const_Linear_Expression_t le, enum ppl_enum_Grid_Generator_Type t, ppl_const_Coefficient_t d)


                                 ;






int
ppl_new_Grid_Generator_zero_dim_point (ppl_Grid_Generator_t* pg);





int
ppl_new_Grid_Generator_from_Grid_Generator
(ppl_Grid_Generator_t* pg, ppl_const_Grid_Generator_t g);




int
ppl_assign_Grid_Generator_from_Grid_Generator
(ppl_Grid_Generator_t dst, ppl_const_Grid_Generator_t src)
                                    ;





int
ppl_delete_Grid_Generator (ppl_const_Grid_Generator_t g);
# 2101 "/usr/local/include/ppl_c.h"
int
ppl_Grid_Generator_space_dimension (ppl_const_Grid_Generator_t g, ppl_dimension_type* m)
                                  ;




int
ppl_Grid_Generator_type (ppl_const_Grid_Generator_t g);





int
ppl_Grid_Generator_coefficient (ppl_const_Grid_Generator_t g, ppl_dimension_type var, ppl_Coefficient_t n)

                            ;




int
ppl_Grid_Generator_divisor (ppl_const_Grid_Generator_t g, ppl_Coefficient_t n)
                               ;






int
ppl_Grid_Generator_OK (ppl_const_Grid_Generator_t g);



int ppl_io_print_Grid_Generator (ppl_const_Grid_Generator_t x); int ppl_io_fprint_Grid_Generator (FILE* stream, ppl_const_Grid_Generator_t x); int ppl_io_asprint_Grid_Generator (char** strp, ppl_const_Grid_Generator_t x); int ppl_Grid_Generator_ascii_dump (ppl_const_Grid_Generator_t x, FILE* stream); int ppl_Grid_Generator_ascii_load (ppl_Grid_Generator_t x, FILE* stream);
# 2147 "/usr/local/include/ppl_c.h"
int
ppl_new_Grid_Generator_System (ppl_Grid_Generator_System_t* pgs);






int
ppl_new_Grid_Generator_System_zero_dim_univ
(ppl_Grid_Generator_System_t* pgs);






int
ppl_new_Grid_Generator_System_from_Grid_Generator
(ppl_Grid_Generator_System_t* pgs, ppl_const_Grid_Generator_t g)
                                  ;





int
ppl_new_Grid_Generator_System_from_Grid_Generator_System
(ppl_Grid_Generator_System_t* pgs, ppl_const_Grid_Generator_System_t gs)
                                          ;




int
ppl_assign_Grid_Generator_System_from_Grid_Generator_System
(ppl_Grid_Generator_System_t dst, ppl_const_Grid_Generator_System_t src)
                                           ;





int
ppl_delete_Grid_Generator_System
(ppl_const_Grid_Generator_System_t gs);
# 2202 "/usr/local/include/ppl_c.h"
int
ppl_Grid_Generator_System_space_dimension
(ppl_const_Grid_Generator_System_t gs, ppl_dimension_type* m);





int
ppl_Grid_Generator_System_empty
(ppl_const_Grid_Generator_System_t gs);





int
ppl_Grid_Generator_System_begin
(ppl_const_Grid_Generator_System_t gs, ppl_Grid_Generator_System_const_iterator_t git)
                                                    ;





int
ppl_Grid_Generator_System_end
(ppl_const_Grid_Generator_System_t gs, ppl_Grid_Generator_System_const_iterator_t git)
                                                    ;






int
ppl_Grid_Generator_System_OK (ppl_const_Grid_Generator_System_t gs);
# 2249 "/usr/local/include/ppl_c.h"
int
ppl_Grid_Generator_System_clear (ppl_Grid_Generator_System_t gs);





int
ppl_Grid_Generator_System_insert_Grid_Generator
(ppl_Grid_Generator_System_t gs, ppl_const_Grid_Generator_t g)
                                  ;



int ppl_io_print_Grid_Generator_System (ppl_const_Grid_Generator_System_t x); int ppl_io_fprint_Grid_Generator_System (FILE* stream, ppl_const_Grid_Generator_System_t x); int ppl_io_asprint_Grid_Generator_System (char** strp, ppl_const_Grid_Generator_System_t x); int ppl_Grid_Generator_System_ascii_dump (ppl_const_Grid_Generator_System_t x, FILE* stream); int ppl_Grid_Generator_System_ascii_load (ppl_Grid_Generator_System_t x, FILE* stream);
# 2273 "/usr/local/include/ppl_c.h"
int
ppl_new_Grid_Generator_System_const_iterator
(ppl_Grid_Generator_System_const_iterator_t* pgit);





int
ppl_new_Grid_Generator_System_const_iterator_from_Grid_Generator_System_const_iterator
(ppl_Grid_Generator_System_const_iterator_t* pgit, ppl_const_Grid_Generator_System_const_iterator_t git)
                                                          ;




int
ppl_assign_Grid_Generator_System_const_iterator_from_Grid_Generator_System_const_iterator
(ppl_Grid_Generator_System_const_iterator_t dst, ppl_const_Grid_Generator_System_const_iterator_t src)
                                                          ;





int
ppl_delete_Grid_Generator_System_const_iterator
(ppl_const_Grid_Generator_System_const_iterator_t git);
# 2311 "/usr/local/include/ppl_c.h"
int
ppl_Grid_Generator_System_const_iterator_dereference
(ppl_const_Grid_Generator_System_const_iterator_t git, ppl_const_Grid_Generator_t* pg)
                                    ;




int
ppl_Grid_Generator_System_const_iterator_increment
(ppl_Grid_Generator_System_const_iterator_t git);





int
ppl_Grid_Generator_System_const_iterator_equal_test
(ppl_const_Grid_Generator_System_const_iterator_t x, ppl_const_Grid_Generator_System_const_iterator_t y)
                                                        ;







extern unsigned int PPL_COMPLEXITY_CLASS_POLYNOMIAL;





extern unsigned int PPL_COMPLEXITY_CLASS_SIMPLEX;




extern unsigned int PPL_COMPLEXITY_CLASS_ANY;





extern unsigned int PPL_POLY_CON_RELATION_IS_DISJOINT;





extern unsigned int PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS;





extern unsigned int PPL_POLY_CON_RELATION_IS_INCLUDED;





extern unsigned int PPL_POLY_CON_RELATION_SATURATES;





extern unsigned int PPL_POLY_GEN_RELATION_SUBSUMES;





enum ppl_enum_Bounded_Integer_Type_Width {

  PPL_BITS_8 = 8,

  PPL_BITS_16 = 16,

  PPL_BITS_32 = 32,

  PPL_BITS_64 = 64,

  PPL_BITS_128 = 128
};




enum ppl_enum_Bounded_Integer_Type_Representation {

  PPL_UNSIGNED,




  PPL_SIGNED_2_COMPLEMENT
};




enum ppl_enum_Bounded_Integer_Type_Overflow {






  PPL_OVERFLOW_WRAPS,
# 2436 "/usr/local/include/ppl_c.h"
  PPL_OVERFLOW_UNDEFINED,
# 2446 "/usr/local/include/ppl_c.h"
  PPL_OVERFLOW_IMPOSSIBLE
};







extern int PPL_OPTIMIZATION_MODE_MAXIMIZATION;




extern int PPL_OPTIMIZATION_MODE_MINIMIZATION;




extern int PPL_MIP_PROBLEM_STATUS_UNFEASIBLE;




extern int PPL_MIP_PROBLEM_STATUS_UNBOUNDED;




extern int PPL_MIP_PROBLEM_STATUS_OPTIMIZED;




extern int PPL_MIP_PROBLEM_CONTROL_PARAMETER_NAME_PRICING;




extern int PPL_MIP_PROBLEM_CONTROL_PARAMETER_PRICING_TEXTBOOK;




extern int PPL_MIP_PROBLEM_CONTROL_PARAMETER_PRICING_STEEPEST_EDGE_EXACT;




extern int PPL_MIP_PROBLEM_CONTROL_PARAMETER_PRICING_STEEPEST_EDGE_FLOAT;






extern int PPL_PIP_PROBLEM_STATUS_UNFEASIBLE;




extern int PPL_PIP_PROBLEM_STATUS_OPTIMIZED;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_NAME_CUTTING_STRATEGY;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_NAME_PIVOT_ROW_STRATEGY;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_CUTTING_STRATEGY_FIRST;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_CUTTING_STRATEGY_DEEPEST;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_CUTTING_STRATEGY_ALL;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_PIVOT_ROW_STRATEGY_FIRST;




extern int PPL_PIP_PROBLEM_CONTROL_PARAMETER_PIVOT_ROW_STRATEGY_MAX_COLUMN;
# 2553 "/usr/local/include/ppl_c.h"
int
ppl_new_MIP_Problem_from_space_dimension (ppl_MIP_Problem_t* pmip, ppl_dimension_type d)
                                ;






int
ppl_new_MIP_Problem (ppl_MIP_Problem_t* pmip, ppl_dimension_type d, ppl_const_Constraint_System_t cs, ppl_const_Linear_Expression_t le, int m)



                 ;





int
ppl_new_MIP_Problem_from_MIP_Problem
(ppl_MIP_Problem_t* pmip, ppl_const_MIP_Problem_t mip);




int
ppl_assign_MIP_Problem_from_MIP_Problem
(ppl_MIP_Problem_t dst, ppl_const_MIP_Problem_t src);





int
ppl_delete_MIP_Problem (ppl_const_MIP_Problem_t mip);
# 2599 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_space_dimension
(ppl_const_MIP_Problem_t mip, ppl_dimension_type* m);




int
ppl_MIP_Problem_number_of_integer_space_dimensions
(ppl_const_MIP_Problem_t mip, ppl_dimension_type* m);






int
ppl_MIP_Problem_integer_space_dimensions
(ppl_const_MIP_Problem_t mip, ppl_dimension_type ds[]);





int
ppl_MIP_Problem_number_of_constraints (ppl_const_MIP_Problem_t mip, ppl_dimension_type* m)
                              ;





int
ppl_MIP_Problem_constraint_at_index (ppl_const_MIP_Problem_t mip, ppl_dimension_type i, ppl_const_Constraint_t* pc)

                                        ;





int
ppl_MIP_Problem_objective_function
(ppl_const_MIP_Problem_t mip, ppl_const_Linear_Expression_t* ple);




int
ppl_MIP_Problem_optimization_mode (ppl_const_MIP_Problem_t mip);






int
ppl_MIP_Problem_OK (ppl_const_MIP_Problem_t mip);
# 2666 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_clear (ppl_MIP_Problem_t mip);





int
ppl_MIP_Problem_add_space_dimensions_and_embed
(ppl_MIP_Problem_t mip, ppl_dimension_type d);






int
ppl_MIP_Problem_add_to_integer_space_dimensions
(ppl_MIP_Problem_t mip, ppl_dimension_type ds[], size_t n);





int
ppl_MIP_Problem_add_constraint (ppl_MIP_Problem_t mip, ppl_const_Constraint_t c)
                                 ;





int
ppl_MIP_Problem_add_constraints (ppl_MIP_Problem_t mip, ppl_const_Constraint_System_t cs)
                                          ;




int
ppl_MIP_Problem_set_objective_function
(ppl_MIP_Problem_t mip, ppl_const_Linear_Expression_t le);




int
ppl_MIP_Problem_set_optimization_mode (ppl_MIP_Problem_t mip, int mode)
                 ;
# 2724 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_is_satisfiable (ppl_const_MIP_Problem_t mip);
# 2739 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_solve (ppl_const_MIP_Problem_t mip);
# 2757 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_evaluate_objective_function
(ppl_const_MIP_Problem_t mip, ppl_const_Generator_t g, ppl_Coefficient_t num, ppl_Coefficient_t den)
                                                  ;





int
ppl_MIP_Problem_feasible_point (ppl_const_MIP_Problem_t mip, ppl_const_Generator_t* pg)
                                  ;





int
ppl_MIP_Problem_optimizing_point (ppl_const_MIP_Problem_t mip, ppl_const_Generator_t* pg)
                                    ;
# 2790 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_optimal_value
(ppl_const_MIP_Problem_t mip, ppl_Coefficient_t num, ppl_Coefficient_t den)
                                                  ;
# 2803 "/usr/local/include/ppl_c.h"
int
ppl_MIP_Problem_get_control_parameter
(ppl_const_MIP_Problem_t mip, int name);




int
ppl_MIP_Problem_set_control_parameter
(ppl_MIP_Problem_t mip, int value);




int
ppl_MIP_Problem_total_memory_in_bytes
(ppl_const_MIP_Problem_t mip, size_t* sz);




int
ppl_MIP_Problem_external_memory_in_bytes
(ppl_const_MIP_Problem_t mip, size_t* sz);
# 2838 "/usr/local/include/ppl_c.h"
int
ppl_new_PIP_Problem_from_space_dimension (ppl_PIP_Problem_t* ppip, ppl_dimension_type d)
                                ;





int
ppl_new_PIP_Problem_from_PIP_Problem
(ppl_PIP_Problem_t* ppip, ppl_const_PIP_Problem_t pip);




int
ppl_assign_PIP_Problem_from_PIP_Problem
(ppl_PIP_Problem_t dst, ppl_const_PIP_Problem_t src);







int
ppl_new_PIP_Problem_from_constraints
(ppl_PIP_Problem_t* ppip, ppl_dimension_type d, ppl_Constraint_System_const_iterator_t first, ppl_Constraint_System_const_iterator_t last, size_t n, ppl_dimension_type ds[])




                                    ;





int
ppl_delete_PIP_Problem (ppl_const_PIP_Problem_t pip);
# 2891 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_space_dimension
(ppl_const_PIP_Problem_t pip, ppl_dimension_type* m);




int
ppl_PIP_Problem_number_of_parameter_space_dimensions
(ppl_const_PIP_Problem_t pip, ppl_dimension_type* m);






int
ppl_PIP_Problem_parameter_space_dimensions
(ppl_const_PIP_Problem_t pip, ppl_dimension_type ds[]);




int
ppl_PIP_Problem_get_big_parameter_dimension
(ppl_const_PIP_Problem_t pip, ppl_dimension_type* pd);





int
ppl_PIP_Problem_number_of_constraints (ppl_const_PIP_Problem_t pip, ppl_dimension_type* m)
                              ;





int
ppl_PIP_Problem_constraint_at_index (ppl_const_PIP_Problem_t pip, ppl_dimension_type i, ppl_const_Constraint_t* pc)

                                        ;




int
ppl_PIP_Problem_total_memory_in_bytes
(ppl_const_PIP_Problem_t pip, size_t* sz);




int
ppl_PIP_Problem_external_memory_in_bytes
(ppl_const_PIP_Problem_t pip, size_t* sz);






int
ppl_PIP_Problem_OK (ppl_const_PIP_Problem_t pip);
# 2965 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_clear (ppl_PIP_Problem_t pip);
# 2988 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_add_space_dimensions_and_embed
(ppl_PIP_Problem_t pip, ppl_dimension_type pip_vars, ppl_dimension_type pip_params)

                                          ;






int
ppl_PIP_Problem_add_to_parameter_space_dimensions
(ppl_PIP_Problem_t pip, ppl_dimension_type ds[], size_t n);




int
ppl_PIP_Problem_set_big_parameter_dimension
(ppl_PIP_Problem_t pip, ppl_dimension_type d);





int
ppl_PIP_Problem_add_constraint (ppl_PIP_Problem_t pip, ppl_const_Constraint_t c)
                                 ;





int
ppl_PIP_Problem_add_constraints (ppl_PIP_Problem_t pip, ppl_const_Constraint_System_t cs)
                                          ;
# 3035 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_is_satisfiable (ppl_const_PIP_Problem_t pip);
# 3047 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_solve (ppl_const_PIP_Problem_t pip);




int
ppl_PIP_Problem_solution (ppl_const_PIP_Problem_t pip, ppl_const_PIP_Tree_Node_t* pip_tree)
                                                                         ;




int
ppl_PIP_Problem_optimizing_solution
(ppl_const_PIP_Problem_t pip, ppl_const_PIP_Tree_Node_t* pip_tree)
                                                ;
# 3073 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Problem_get_control_parameter
(ppl_const_PIP_Problem_t pip, int name);




int
ppl_PIP_Problem_set_control_parameter
(ppl_PIP_Problem_t pip, int value);
# 3091 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Tree_Node_as_solution
(ppl_const_PIP_Tree_Node_t spip_tree, ppl_const_PIP_Solution_Node_t* dpip_tree)
                                                     ;





int
ppl_PIP_Tree_Node_as_decision
(ppl_const_PIP_Tree_Node_t spip_tree, ppl_const_PIP_Decision_Node_t* dpip_tree)
                                                     ;





int
ppl_PIP_Tree_Node_get_constraints
(ppl_const_PIP_Tree_Node_t pip_tree, ppl_const_Constraint_System_t* pcs)
                                               ;






int
ppl_PIP_Tree_Node_OK (ppl_const_PIP_Tree_Node_t pip);





int
ppl_PIP_Tree_Node_number_of_artificials
(ppl_const_PIP_Tree_Node_t pip_tree, ppl_dimension_type* m)
                                  ;





int
ppl_PIP_Tree_Node_begin
(ppl_const_PIP_Tree_Node_t pip_tree, ppl_Artificial_Parameter_Sequence_const_iterator_t pit)
                                                                   ;





int
ppl_PIP_Tree_Node_end
(ppl_const_PIP_Tree_Node_t pip_tree, ppl_Artificial_Parameter_Sequence_const_iterator_t pit)
                                                                   ;
# 3169 "/usr/local/include/ppl_c.h"
int
ppl_PIP_Solution_Node_get_parametric_values
(ppl_const_PIP_Solution_Node_t pip_sol, ppl_dimension_type var, ppl_const_Linear_Expression_t* le)

                                              ;





int
ppl_PIP_Decision_Node_get_child_node
(ppl_const_PIP_Decision_Node_t pip_dec, int b, ppl_const_PIP_Tree_Node_t* pip_tree)

                                                ;




int
ppl_Artificial_Parameter_get_Linear_Expression
(ppl_const_Artificial_Parameter_t ap, ppl_Linear_Expression_t le)
                                       ;





int
ppl_Artificial_Parameter_coefficient
(ppl_const_Artificial_Parameter_t ap, ppl_dimension_type var, ppl_Coefficient_t n)

                                ;





int
ppl_Artificial_Parameter_get_inhomogeneous_term
(ppl_const_Artificial_Parameter_t ap, ppl_Coefficient_t n)
                                ;




int
ppl_Artificial_Parameter_denominator
(ppl_const_Artificial_Parameter_t ap, ppl_Coefficient_t n)
                                ;
# 3227 "/usr/local/include/ppl_c.h"
int
ppl_new_Artificial_Parameter_Sequence_const_iterator
(ppl_Artificial_Parameter_Sequence_const_iterator_t* papit);





int
ppl_new_Artificial_Parameter_Sequence_const_iterator_from_Artificial_Parameter_Sequence_const_iterator
(ppl_Artificial_Parameter_Sequence_const_iterator_t* papit, ppl_const_Artificial_Parameter_Sequence_const_iterator_t apit)
                                                                   ;




int
ppl_assign_Artificial_Parameter_Sequence_const_iterator_from_Artificial_Parameter_Sequence_const_iterator
(ppl_Artificial_Parameter_Sequence_const_iterator_t dst, ppl_const_Artificial_Parameter_Sequence_const_iterator_t src)
                                                                  ;





int
ppl_delete_Artificial_Parameter_Sequence_const_iterator
(ppl_const_Artificial_Parameter_Sequence_const_iterator_t apit);
# 3265 "/usr/local/include/ppl_c.h"
int
ppl_Artificial_Parameter_Sequence_const_iterator_dereference
(ppl_const_Artificial_Parameter_Sequence_const_iterator_t apit, ppl_const_Artificial_Parameter_t* pap)
                                           ;




int
ppl_Artificial_Parameter_Sequence_const_iterator_increment
(ppl_Artificial_Parameter_Sequence_const_iterator_t apit);





int
ppl_Artificial_Parameter_Sequence_const_iterator_equal_test
(ppl_const_Artificial_Parameter_Sequence_const_iterator_t x, ppl_const_Artificial_Parameter_Sequence_const_iterator_t y)
                                                                ;



int ppl_io_print_MIP_Problem (ppl_const_MIP_Problem_t x); int ppl_io_fprint_MIP_Problem (FILE* stream, ppl_const_MIP_Problem_t x); int ppl_io_asprint_MIP_Problem (char** strp, ppl_const_MIP_Problem_t x); int ppl_MIP_Problem_ascii_dump (ppl_const_MIP_Problem_t x, FILE* stream); int ppl_MIP_Problem_ascii_load (ppl_MIP_Problem_t x, FILE* stream);

int ppl_io_print_PIP_Problem (ppl_const_PIP_Problem_t x); int ppl_io_fprint_PIP_Problem (FILE* stream, ppl_const_PIP_Problem_t x); int ppl_io_asprint_PIP_Problem (char** strp, ppl_const_PIP_Problem_t x); int ppl_PIP_Problem_ascii_dump (ppl_const_PIP_Problem_t x, FILE* stream); int ppl_PIP_Problem_ascii_load (ppl_PIP_Problem_t x, FILE* stream);

int ppl_io_print_PIP_Tree_Node (ppl_const_PIP_Tree_Node_t x); int ppl_io_fprint_PIP_Tree_Node (FILE* stream, ppl_const_PIP_Tree_Node_t x); int ppl_io_asprint_PIP_Tree_Node (char** strp, ppl_const_PIP_Tree_Node_t x); int ppl_PIP_Tree_Node_ascii_dump (ppl_const_PIP_Tree_Node_t x, FILE* stream); int ppl_PIP_Tree_Node_ascii_load (ppl_PIP_Tree_Node_t x, FILE* stream);

int ppl_io_print_PIP_Solution_Node (ppl_const_PIP_Solution_Node_t x); int ppl_io_fprint_PIP_Solution_Node (FILE* stream, ppl_const_PIP_Solution_Node_t x); int ppl_io_asprint_PIP_Solution_Node (char** strp, ppl_const_PIP_Solution_Node_t x); int ppl_PIP_Solution_Node_ascii_dump (ppl_const_PIP_Solution_Node_t x, FILE* stream); int ppl_PIP_Solution_Node_ascii_load (ppl_PIP_Solution_Node_t x, FILE* stream);

int ppl_io_print_PIP_Decision_Node (ppl_const_PIP_Decision_Node_t x); int ppl_io_fprint_PIP_Decision_Node (FILE* stream, ppl_const_PIP_Decision_Node_t x); int ppl_io_asprint_PIP_Decision_Node (char** strp, ppl_const_PIP_Decision_Node_t x); int ppl_PIP_Decision_Node_ascii_dump (ppl_const_PIP_Decision_Node_t x, FILE* stream); int ppl_PIP_Decision_Node_ascii_load (ppl_PIP_Decision_Node_t x, FILE* stream);

int ppl_io_print_Artificial_Parameter (ppl_const_Artificial_Parameter_t x); int ppl_io_fprint_Artificial_Parameter (FILE* stream, ppl_const_Artificial_Parameter_t x); int ppl_io_asprint_Artificial_Parameter (char** strp, ppl_const_Artificial_Parameter_t x); int ppl_Artificial_Parameter_ascii_dump (ppl_const_Artificial_Parameter_t x, FILE* stream); int ppl_Artificial_Parameter_ascii_load (ppl_Artificial_Parameter_t x, FILE* stream);





typedef struct ppl_Polyhedron_tag* ppl_Polyhedron_t; typedef struct ppl_Polyhedron_tag const* ppl_const_Polyhedron_t;
typedef struct ppl_Grid_tag* ppl_Grid_t; typedef struct ppl_Grid_tag const* ppl_const_Grid_t;
typedef struct ppl_Rational_Box_tag* ppl_Rational_Box_t; typedef struct ppl_Rational_Box_tag const* ppl_const_Rational_Box_t;
typedef struct ppl_BD_Shape_mpz_class_tag* ppl_BD_Shape_mpz_class_t; typedef struct ppl_BD_Shape_mpz_class_tag const* ppl_const_BD_Shape_mpz_class_t;
typedef struct ppl_BD_Shape_mpq_class_tag* ppl_BD_Shape_mpq_class_t; typedef struct ppl_BD_Shape_mpq_class_tag const* ppl_const_BD_Shape_mpq_class_t;
typedef struct ppl_Octagonal_Shape_mpz_class_tag* ppl_Octagonal_Shape_mpz_class_t; typedef struct ppl_Octagonal_Shape_mpz_class_tag const* ppl_const_Octagonal_Shape_mpz_class_t;
typedef struct ppl_Octagonal_Shape_mpq_class_tag* ppl_Octagonal_Shape_mpq_class_t; typedef struct ppl_Octagonal_Shape_mpq_class_tag const* ppl_const_Octagonal_Shape_mpq_class_t;
typedef struct ppl_Constraints_Product_C_Polyhedron_Grid_tag* ppl_Constraints_Product_C_Polyhedron_Grid_t; typedef struct ppl_Constraints_Product_C_Polyhedron_Grid_tag const* ppl_const_Constraints_Product_C_Polyhedron_Grid_t;
typedef struct ppl_Pointset_Powerset_C_Polyhedron_tag* ppl_Pointset_Powerset_C_Polyhedron_t; typedef struct ppl_Pointset_Powerset_C_Polyhedron_tag const* ppl_const_Pointset_Powerset_C_Polyhedron_t;
typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_tag* ppl_Pointset_Powerset_NNC_Polyhedron_t; typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_tag const* ppl_const_Pointset_Powerset_NNC_Polyhedron_t;
typedef struct ppl_Double_Box_tag* ppl_Double_Box_t; typedef struct ppl_Double_Box_tag const* ppl_const_Double_Box_t;
typedef struct ppl_BD_Shape_double_tag* ppl_BD_Shape_double_t; typedef struct ppl_BD_Shape_double_tag const* ppl_const_BD_Shape_double_t;
typedef struct ppl_Octagonal_Shape_double_tag* ppl_Octagonal_Shape_double_t; typedef struct ppl_Octagonal_Shape_double_tag const* ppl_const_Octagonal_Shape_double_t;






int
ppl_delete_Polyhedron
(ppl_const_Polyhedron_t ph);





int
ppl_new_C_Polyhedron_from_space_dimension
(ppl_Polyhedron_t* pph, ppl_dimension_type d, int empty);


int
ppl_new_NNC_Polyhedron_from_space_dimension
(ppl_Polyhedron_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_C_Polyhedron_from_C_Polyhedron
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_NNC_Polyhedron_from_C_Polyhedron
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_C_Polyhedron_from_NNC_Polyhedron
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_C_Polyhedron_from_Grid
(ppl_Polyhedron_t* pph, ppl_const_Grid_t ph);


int
ppl_new_NNC_Polyhedron_from_Grid
(ppl_Polyhedron_t* pph, ppl_const_Grid_t ph);


int
ppl_new_C_Polyhedron_from_Rational_Box
(ppl_Polyhedron_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_NNC_Polyhedron_from_Rational_Box
(ppl_Polyhedron_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_C_Polyhedron_from_BD_Shape_mpz_class
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_C_Polyhedron_from_BD_Shape_mpq_class
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_C_Polyhedron_from_Double_Box
(ppl_Polyhedron_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_NNC_Polyhedron_from_Double_Box
(ppl_Polyhedron_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_C_Polyhedron_from_BD_Shape_double
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_double
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_double
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_double_t ph);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Grid_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Grid_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Rational_Box_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Double_Box_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);


int
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity
(ppl_Polyhedron_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_C_Polyhedron_from_Constraint_System
(ppl_Polyhedron_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_NNC_Polyhedron_from_Constraint_System
(ppl_Polyhedron_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_C_Polyhedron_from_Congruence_System
(ppl_Polyhedron_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_NNC_Polyhedron_from_Congruence_System
(ppl_Polyhedron_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_C_Polyhedron_from_Generator_System
(ppl_Polyhedron_t* pph, ppl_const_Generator_System_t cs);


int
ppl_new_NNC_Polyhedron_from_Generator_System
(ppl_Polyhedron_t* pph, ppl_const_Generator_System_t cs);
# 3612 "/usr/local/include/ppl_c.h"
int
ppl_Polyhedron_space_dimension
(ppl_const_Polyhedron_t ph, ppl_dimension_type* m);


int
ppl_Polyhedron_affine_dimension
(ppl_const_Polyhedron_t ph, ppl_dimension_type* m);






int
ppl_Polyhedron_relation_with_Constraint
(ppl_const_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Polyhedron_relation_with_Generator
(ppl_const_Polyhedron_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Polyhedron_relation_with_Congruence
(ppl_const_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Polyhedron_get_constraints
(ppl_const_Polyhedron_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Polyhedron_get_congruences
(ppl_const_Polyhedron_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;


int
ppl_Polyhedron_get_generators
(ppl_const_Polyhedron_t ph, ppl_const_Generator_System_t* pcs)
                                              ;






int
ppl_Polyhedron_get_minimized_constraints
(ppl_const_Polyhedron_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Polyhedron_get_minimized_congruences
(ppl_const_Polyhedron_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;


int
ppl_Polyhedron_get_minimized_generators
(ppl_const_Polyhedron_t ph, ppl_const_Generator_System_t* pcs)
                                              ;






int
ppl_Polyhedron_is_empty
(ppl_const_Polyhedron_t ph);


int
ppl_Polyhedron_is_universe
(ppl_const_Polyhedron_t ph);


int
ppl_Polyhedron_is_bounded
(ppl_const_Polyhedron_t ph);


int
ppl_Polyhedron_contains_integer_point
(ppl_const_Polyhedron_t ph);


int
ppl_Polyhedron_is_topologically_closed
(ppl_const_Polyhedron_t ph);


int
ppl_Polyhedron_is_discrete
(ppl_const_Polyhedron_t ph);






int
ppl_Polyhedron_topological_closure_assign
(ppl_Polyhedron_t ph);






int
ppl_Polyhedron_bounds_from_above
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Polyhedron_bounds_from_below
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Polyhedron_maximize
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Polyhedron_minimize
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Polyhedron_maximize_with_point
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Polyhedron_minimize_with_point
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Polyhedron_frequency
(ppl_const_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Polyhedron_contains_Polyhedron
(ppl_const_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_strictly_contains_Polyhedron
(ppl_const_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_is_disjoint_from_Polyhedron
(ppl_const_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;






int
ppl_Polyhedron_equals_Polyhedron
(ppl_const_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;





int
ppl_Polyhedron_OK
(ppl_const_Polyhedron_t ph);





int
ppl_Polyhedron_add_constraint
(ppl_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Polyhedron_add_congruence
(ppl_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;


int
ppl_Polyhedron_add_generator
(ppl_Polyhedron_t ph, ppl_const_Generator_t c)
                                    ;






int
ppl_Polyhedron_add_constraints
(ppl_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Polyhedron_add_congruences
(ppl_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;


int
ppl_Polyhedron_add_generators
(ppl_Polyhedron_t ph, ppl_const_Generator_System_t cs)
                                            ;






int
ppl_Polyhedron_refine_with_constraint
(ppl_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Polyhedron_refine_with_congruence
(ppl_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Polyhedron_refine_with_constraints
(ppl_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Polyhedron_refine_with_congruences
(ppl_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Polyhedron_intersection_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_upper_bound_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_difference_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_concatenate_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_time_elapse_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_poly_hull_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_poly_difference_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;






int
ppl_Polyhedron_upper_bound_assign_if_exact
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_poly_hull_assign_if_exact
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;






int
ppl_Polyhedron_simplify_using_context_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;





int
ppl_Polyhedron_constrains
(ppl_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Polyhedron_unconstrain_space_dimension
(ppl_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Polyhedron_unconstrain_space_dimensions
(ppl_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Polyhedron_affine_image
(ppl_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Polyhedron_affine_preimage
(ppl_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Polyhedron_bounded_affine_image
(ppl_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Polyhedron_bounded_affine_preimage
(ppl_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Polyhedron_generalized_affine_image
(ppl_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Polyhedron_generalized_affine_preimage
(ppl_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Polyhedron_generalized_affine_image_lhs_rhs
(ppl_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Polyhedron_generalized_affine_preimage_lhs_rhs
(ppl_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Polyhedron_add_space_dimensions_and_embed
(ppl_Polyhedron_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Polyhedron_add_space_dimensions_and_project
(ppl_Polyhedron_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Polyhedron_remove_space_dimensions
(ppl_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Polyhedron_remove_higher_space_dimensions
(ppl_Polyhedron_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Polyhedron_expand_space_dimension
(ppl_Polyhedron_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Polyhedron_fold_space_dimensions
(ppl_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Polyhedron_map_space_dimensions
(ppl_Polyhedron_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Polyhedron_drop_some_non_integer_points
(ppl_Polyhedron_t ph, int complexity)
                           ;





int
ppl_Polyhedron_drop_some_non_integer_points_2
(ppl_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Polyhedron_external_memory_in_bytes
(ppl_const_Polyhedron_t ps, size_t* sz)
                       ;


int
ppl_Polyhedron_total_memory_in_bytes
(ppl_const_Polyhedron_t ps, size_t* sz)
                       ;






int
ppl_Polyhedron_BHRZ03_widening_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, unsigned* tp)

                         ;


int
ppl_Polyhedron_H79_widening_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, unsigned* tp)

                         ;






int
ppl_Polyhedron_BHRZ03_widening_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;


int
ppl_Polyhedron_H79_widening_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;






int
ppl_Polyhedron_widening_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, unsigned* tp)

                         ;





int
ppl_Polyhedron_widening_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y)
                                     ;





int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 4303 "/usr/local/include/ppl_c.h"
int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Polyhedron_limited_H79_extrapolation_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Polyhedron_bounded_H79_extrapolation_assign
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 4341 "/usr/local/include/ppl_c.h"
int
ppl_Polyhedron_linear_partition
(ppl_const_Polyhedron_t x, ppl_const_Polyhedron_t y, ppl_Polyhedron_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Polyhedron_wrap_assign
(ppl_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_C_Polyhedron_recycle_Constraint_System
(ppl_Polyhedron_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_NNC_Polyhedron_recycle_Constraint_System
(ppl_Polyhedron_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_C_Polyhedron_recycle_Congruence_System
(ppl_Polyhedron_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_NNC_Polyhedron_recycle_Congruence_System
(ppl_Polyhedron_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_C_Polyhedron_recycle_Generator_System
(ppl_Polyhedron_t* pph, ppl_Generator_System_t cs);


int
ppl_new_NNC_Polyhedron_recycle_Generator_System
(ppl_Polyhedron_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_C_Polyhedron_from_C_Polyhedron
(ppl_Polyhedron_t dst, ppl_const_Polyhedron_t src);


int
ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron
(ppl_Polyhedron_t dst, ppl_const_Polyhedron_t src);






int
ppl_Polyhedron_add_recycled_constraints
(ppl_Polyhedron_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Polyhedron_add_recycled_congruences
(ppl_Polyhedron_t ph, ppl_Congruence_System_t cs)
                                       ;


int
ppl_Polyhedron_add_recycled_generators
(ppl_Polyhedron_t ph, ppl_Generator_System_t cs)
                                      ;






int
ppl_termination_test_MS_C_Polyhedron
(ppl_const_Polyhedron_t pset);


int
ppl_termination_test_PR_C_Polyhedron
(ppl_const_Polyhedron_t pset);


int
ppl_termination_test_MS_NNC_Polyhedron
(ppl_const_Polyhedron_t pset);


int
ppl_termination_test_PR_NNC_Polyhedron
(ppl_const_Polyhedron_t pset);







int
ppl_one_affine_ranking_function_MS_C_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_C_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_MS_NNC_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_NNC_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_C_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_C_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_MS_NNC_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_NNC_Polyhedron
(ppl_const_Polyhedron_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after);


int
ppl_termination_test_PR_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after);


int
ppl_termination_test_MS_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after);


int
ppl_termination_test_PR_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after);







int
ppl_one_affine_ranking_function_MS_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_MS_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_C_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_MS_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_NNC_Polyhedron_2
(ppl_const_Polyhedron_t pset_before, ppl_const_Polyhedron_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Polyhedron (ppl_const_Polyhedron_t x); int ppl_io_fprint_Polyhedron (FILE* stream, ppl_const_Polyhedron_t x); int ppl_io_asprint_Polyhedron (char** strp, ppl_const_Polyhedron_t x); int ppl_Polyhedron_ascii_dump (ppl_const_Polyhedron_t x, FILE* stream); int ppl_Polyhedron_ascii_load (ppl_Polyhedron_t x, FILE* stream);
# 4622 "/usr/local/include/ppl_c.h"
int
ppl_delete_Grid
(ppl_const_Grid_t ph);





int
ppl_new_Grid_from_space_dimension
(ppl_Grid_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Grid_from_C_Polyhedron
(ppl_Grid_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Grid_from_NNC_Polyhedron
(ppl_Grid_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Grid_from_Grid
(ppl_Grid_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Grid_from_Rational_Box
(ppl_Grid_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Grid_from_BD_Shape_mpz_class
(ppl_Grid_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Grid_from_BD_Shape_mpq_class
(ppl_Grid_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Grid_from_Octagonal_Shape_mpz_class
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Grid_from_Octagonal_Shape_mpq_class
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Grid_from_Double_Box
(ppl_Grid_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Grid_from_BD_Shape_double
(ppl_Grid_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Grid_from_Octagonal_Shape_double
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Grid_from_C_Polyhedron_with_complexity
(ppl_Grid_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Grid_from_NNC_Polyhedron_with_complexity
(ppl_Grid_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Grid_from_Grid_with_complexity
(ppl_Grid_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Grid_from_Rational_Box_with_complexity
(ppl_Grid_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity
(ppl_Grid_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity
(ppl_Grid_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Grid_from_Double_Box_with_complexity
(ppl_Grid_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Grid_from_BD_Shape_double_with_complexity
(ppl_Grid_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Grid_from_Octagonal_Shape_double_with_complexity
(ppl_Grid_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Grid_from_Constraint_System
(ppl_Grid_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Grid_from_Congruence_System
(ppl_Grid_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Grid_from_Grid_Generator_System
(ppl_Grid_t* pph, ppl_const_Grid_Generator_System_t cs);
# 4781 "/usr/local/include/ppl_c.h"
int
ppl_Grid_space_dimension
(ppl_const_Grid_t ph, ppl_dimension_type* m);


int
ppl_Grid_affine_dimension
(ppl_const_Grid_t ph, ppl_dimension_type* m);






int
ppl_Grid_relation_with_Constraint
(ppl_const_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Grid_relation_with_Generator
(ppl_const_Grid_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Grid_relation_with_Congruence
(ppl_const_Grid_t ph, ppl_const_Congruence_t c)
                                     ;


int
ppl_Grid_relation_with_Grid_Generator
(ppl_const_Grid_t ph, ppl_const_Grid_Generator_t c)
                                         ;






int
ppl_Grid_get_constraints
(ppl_const_Grid_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Grid_get_congruences
(ppl_const_Grid_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;


int
ppl_Grid_get_grid_generators
(ppl_const_Grid_t ph, ppl_const_Grid_Generator_System_t* pcs)
                                                   ;






int
ppl_Grid_get_minimized_constraints
(ppl_const_Grid_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Grid_get_minimized_congruences
(ppl_const_Grid_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;


int
ppl_Grid_get_minimized_grid_generators
(ppl_const_Grid_t ph, ppl_const_Grid_Generator_System_t* pcs)
                                                   ;






int
ppl_Grid_is_empty
(ppl_const_Grid_t ph);


int
ppl_Grid_is_universe
(ppl_const_Grid_t ph);


int
ppl_Grid_is_bounded
(ppl_const_Grid_t ph);


int
ppl_Grid_contains_integer_point
(ppl_const_Grid_t ph);


int
ppl_Grid_is_topologically_closed
(ppl_const_Grid_t ph);


int
ppl_Grid_is_discrete
(ppl_const_Grid_t ph);






int
ppl_Grid_topological_closure_assign
(ppl_Grid_t ph);






int
ppl_Grid_bounds_from_above
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Grid_bounds_from_below
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Grid_maximize
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Grid_minimize
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Grid_maximize_with_point
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Grid_minimize_with_point
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Grid_frequency
(ppl_const_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Grid_contains_Grid
(ppl_const_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_strictly_contains_Grid
(ppl_const_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_is_disjoint_from_Grid
(ppl_const_Grid_t x, ppl_const_Grid_t y)
                               ;






int
ppl_Grid_equals_Grid
(ppl_const_Grid_t x, ppl_const_Grid_t y)
                               ;





int
ppl_Grid_OK
(ppl_const_Grid_t ph);





int
ppl_Grid_add_constraint
(ppl_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Grid_add_congruence
(ppl_Grid_t ph, ppl_const_Congruence_t c)
                                     ;


int
ppl_Grid_add_grid_generator
(ppl_Grid_t ph, ppl_const_Grid_Generator_t c)
                                         ;






int
ppl_Grid_add_constraints
(ppl_Grid_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Grid_add_congruences
(ppl_Grid_t ph, ppl_const_Congruence_System_t cs)
                                             ;


int
ppl_Grid_add_grid_generators
(ppl_Grid_t ph, ppl_const_Grid_Generator_System_t cs)
                                                 ;






int
ppl_Grid_refine_with_constraint
(ppl_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Grid_refine_with_congruence
(ppl_Grid_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Grid_refine_with_constraints
(ppl_Grid_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Grid_refine_with_congruences
(ppl_Grid_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Grid_intersection_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_upper_bound_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_difference_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_concatenate_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_time_elapse_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;






int
ppl_Grid_upper_bound_assign_if_exact
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;






int
ppl_Grid_simplify_using_context_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;





int
ppl_Grid_constrains
(ppl_Grid_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Grid_unconstrain_space_dimension
(ppl_Grid_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Grid_unconstrain_space_dimensions
(ppl_Grid_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Grid_affine_image
(ppl_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Grid_affine_preimage
(ppl_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Grid_bounded_affine_image
(ppl_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Grid_bounded_affine_preimage
(ppl_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Grid_generalized_affine_image
(ppl_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Grid_generalized_affine_preimage
(ppl_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Grid_generalized_affine_image_lhs_rhs
(ppl_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Grid_generalized_affine_preimage_lhs_rhs
(ppl_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Grid_generalized_affine_image_with_congruence
(ppl_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d, ppl_const_Coefficient_t m)




                                      ;


int
ppl_Grid_generalized_affine_preimage_with_congruence
(ppl_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d, ppl_const_Coefficient_t m)




                                      ;






int
ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence
(ppl_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs, ppl_const_Coefficient_t m)



                                      ;


int
ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence
(ppl_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs, ppl_const_Coefficient_t m)



                                      ;






int
ppl_Grid_add_space_dimensions_and_embed
(ppl_Grid_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Grid_add_space_dimensions_and_project
(ppl_Grid_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Grid_remove_space_dimensions
(ppl_Grid_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Grid_remove_higher_space_dimensions
(ppl_Grid_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Grid_expand_space_dimension
(ppl_Grid_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Grid_fold_space_dimensions
(ppl_Grid_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Grid_map_space_dimensions
(ppl_Grid_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Grid_drop_some_non_integer_points
(ppl_Grid_t ph, int complexity)
                           ;





int
ppl_Grid_drop_some_non_integer_points_2
(ppl_Grid_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Grid_external_memory_in_bytes
(ppl_const_Grid_t ps, size_t* sz)
                       ;


int
ppl_Grid_total_memory_in_bytes
(ppl_const_Grid_t ps, size_t* sz)
                       ;






int
ppl_Grid_congruence_widening_assign_with_tokens
(ppl_Grid_t x, ppl_const_Grid_t y, unsigned* tp)

                         ;


int
ppl_Grid_generator_widening_assign_with_tokens
(ppl_Grid_t x, ppl_const_Grid_t y, unsigned* tp)

                         ;






int
ppl_Grid_congruence_widening_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;


int
ppl_Grid_generator_widening_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;






int
ppl_Grid_widening_assign_with_tokens
(ppl_Grid_t x, ppl_const_Grid_t y, unsigned* tp)

                         ;





int
ppl_Grid_widening_assign
(ppl_Grid_t x, ppl_const_Grid_t y)
                               ;





int
ppl_Grid_limited_congruence_extrapolation_assign_with_tokens
(ppl_Grid_t x, ppl_const_Grid_t y, ppl_const_Congruence_System_t cs, unsigned* tp)


                         ;


int
ppl_Grid_limited_generator_extrapolation_assign_with_tokens
(ppl_Grid_t x, ppl_const_Grid_t y, ppl_const_Congruence_System_t cs, unsigned* tp)


                         ;
# 5490 "/usr/local/include/ppl_c.h"
int
ppl_Grid_limited_congruence_extrapolation_assign
(ppl_Grid_t x, ppl_const_Grid_t y, ppl_const_Congruence_System_t cs)

                                             ;


int
ppl_Grid_limited_generator_extrapolation_assign
(ppl_Grid_t x, ppl_const_Grid_t y, ppl_const_Congruence_System_t cs)

                                             ;
# 5514 "/usr/local/include/ppl_c.h"
int
ppl_Grid_wrap_assign
(ppl_Grid_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Grid_recycle_Constraint_System
(ppl_Grid_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Grid_recycle_Congruence_System
(ppl_Grid_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Grid_recycle_Grid_Generator_System
(ppl_Grid_t* pph, ppl_Grid_Generator_System_t cs);







int
ppl_assign_Grid_from_Grid
(ppl_Grid_t dst, ppl_const_Grid_t src);






int
ppl_Grid_add_recycled_constraints
(ppl_Grid_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Grid_add_recycled_congruences
(ppl_Grid_t ph, ppl_Congruence_System_t cs)
                                       ;


int
ppl_Grid_add_recycled_grid_generators
(ppl_Grid_t ph, ppl_Grid_Generator_System_t cs)
                                           ;






int
ppl_termination_test_MS_Grid
(ppl_const_Grid_t pset);


int
ppl_termination_test_PR_Grid
(ppl_const_Grid_t pset);







int
ppl_one_affine_ranking_function_MS_Grid
(ppl_const_Grid_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Grid
(ppl_const_Grid_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Grid
(ppl_const_Grid_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Grid
(ppl_const_Grid_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after);


int
ppl_termination_test_PR_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after);







int
ppl_one_affine_ranking_function_MS_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Grid_2
(ppl_const_Grid_t pset_before, ppl_const_Grid_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Grid (ppl_const_Grid_t x); int ppl_io_fprint_Grid (FILE* stream, ppl_const_Grid_t x); int ppl_io_asprint_Grid (char** strp, ppl_const_Grid_t x); int ppl_Grid_ascii_dump (ppl_const_Grid_t x, FILE* stream); int ppl_Grid_ascii_load (ppl_Grid_t x, FILE* stream);
# 5691 "/usr/local/include/ppl_c.h"
int
ppl_delete_Rational_Box
(ppl_const_Rational_Box_t ph);





int
ppl_new_Rational_Box_from_space_dimension
(ppl_Rational_Box_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Rational_Box_from_C_Polyhedron
(ppl_Rational_Box_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Rational_Box_from_NNC_Polyhedron
(ppl_Rational_Box_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Rational_Box_from_Grid
(ppl_Rational_Box_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Rational_Box_from_Rational_Box
(ppl_Rational_Box_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Rational_Box_from_BD_Shape_mpz_class
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Rational_Box_from_BD_Shape_mpq_class
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Rational_Box_from_Double_Box
(ppl_Rational_Box_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Rational_Box_from_BD_Shape_double
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Rational_Box_from_Octagonal_Shape_double
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Rational_Box_from_C_Polyhedron_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Rational_Box_from_Grid_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Rational_Box_from_Rational_Box_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Rational_Box_from_Double_Box_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Rational_Box_from_BD_Shape_double_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity
(ppl_Rational_Box_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Rational_Box_from_Constraint_System
(ppl_Rational_Box_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Rational_Box_from_Congruence_System
(ppl_Rational_Box_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Rational_Box_from_Generator_System
(ppl_Rational_Box_t* pph, ppl_const_Generator_System_t cs);
# 5850 "/usr/local/include/ppl_c.h"
int
ppl_Rational_Box_space_dimension
(ppl_const_Rational_Box_t ph, ppl_dimension_type* m);


int
ppl_Rational_Box_affine_dimension
(ppl_const_Rational_Box_t ph, ppl_dimension_type* m);






int
ppl_Rational_Box_relation_with_Constraint
(ppl_const_Rational_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Rational_Box_relation_with_Generator
(ppl_const_Rational_Box_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Rational_Box_relation_with_Congruence
(ppl_const_Rational_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Rational_Box_get_constraints
(ppl_const_Rational_Box_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Rational_Box_get_congruences
(ppl_const_Rational_Box_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Rational_Box_get_minimized_constraints
(ppl_const_Rational_Box_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Rational_Box_get_minimized_congruences
(ppl_const_Rational_Box_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Rational_Box_is_empty
(ppl_const_Rational_Box_t ph);


int
ppl_Rational_Box_is_universe
(ppl_const_Rational_Box_t ph);


int
ppl_Rational_Box_is_bounded
(ppl_const_Rational_Box_t ph);


int
ppl_Rational_Box_contains_integer_point
(ppl_const_Rational_Box_t ph);


int
ppl_Rational_Box_is_topologically_closed
(ppl_const_Rational_Box_t ph);


int
ppl_Rational_Box_is_discrete
(ppl_const_Rational_Box_t ph);






int
ppl_Rational_Box_topological_closure_assign
(ppl_Rational_Box_t ph);






int
ppl_Rational_Box_bounds_from_above
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Rational_Box_bounds_from_below
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Rational_Box_get_upper_bound
(ppl_Rational_Box_t ps, ppl_dimension_type var, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* pclosed)



                         ;


int
ppl_Rational_Box_get_lower_bound
(ppl_Rational_Box_t ps, ppl_dimension_type var, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* pclosed)



                         ;






int
ppl_Rational_Box_maximize
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Rational_Box_minimize
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Rational_Box_maximize_with_point
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Rational_Box_minimize_with_point
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Rational_Box_frequency
(ppl_const_Rational_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Rational_Box_contains_Rational_Box
(ppl_const_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_strictly_contains_Rational_Box
(ppl_const_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_is_disjoint_from_Rational_Box
(ppl_const_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;






int
ppl_Rational_Box_equals_Rational_Box
(ppl_const_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;





int
ppl_Rational_Box_OK
(ppl_const_Rational_Box_t ph);





int
ppl_Rational_Box_add_constraint
(ppl_Rational_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Rational_Box_add_congruence
(ppl_Rational_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Rational_Box_add_constraints
(ppl_Rational_Box_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Rational_Box_add_congruences
(ppl_Rational_Box_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Rational_Box_refine_with_constraint
(ppl_Rational_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Rational_Box_refine_with_congruence
(ppl_Rational_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Rational_Box_refine_with_constraints
(ppl_Rational_Box_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Rational_Box_refine_with_congruences
(ppl_Rational_Box_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Rational_Box_intersection_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_upper_bound_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_difference_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_concatenate_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;


int
ppl_Rational_Box_time_elapse_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;






int
ppl_Rational_Box_upper_bound_assign_if_exact
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;






int
ppl_Rational_Box_simplify_using_context_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;





int
ppl_Rational_Box_constrains
(ppl_Rational_Box_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Rational_Box_unconstrain_space_dimension
(ppl_Rational_Box_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Rational_Box_unconstrain_space_dimensions
(ppl_Rational_Box_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Rational_Box_affine_image
(ppl_Rational_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Rational_Box_affine_preimage
(ppl_Rational_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Rational_Box_bounded_affine_image
(ppl_Rational_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Rational_Box_bounded_affine_preimage
(ppl_Rational_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Rational_Box_generalized_affine_image
(ppl_Rational_Box_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Rational_Box_generalized_affine_preimage
(ppl_Rational_Box_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Rational_Box_generalized_affine_image_lhs_rhs
(ppl_Rational_Box_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Rational_Box_generalized_affine_preimage_lhs_rhs
(ppl_Rational_Box_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Rational_Box_add_space_dimensions_and_embed
(ppl_Rational_Box_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Rational_Box_add_space_dimensions_and_project
(ppl_Rational_Box_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Rational_Box_remove_space_dimensions
(ppl_Rational_Box_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Rational_Box_remove_higher_space_dimensions
(ppl_Rational_Box_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Rational_Box_expand_space_dimension
(ppl_Rational_Box_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Rational_Box_fold_space_dimensions
(ppl_Rational_Box_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Rational_Box_map_space_dimensions
(ppl_Rational_Box_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Rational_Box_drop_some_non_integer_points
(ppl_Rational_Box_t ph, int complexity)
                           ;





int
ppl_Rational_Box_drop_some_non_integer_points_2
(ppl_Rational_Box_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Rational_Box_external_memory_in_bytes
(ppl_const_Rational_Box_t ps, size_t* sz)
                       ;


int
ppl_Rational_Box_total_memory_in_bytes
(ppl_const_Rational_Box_t ps, size_t* sz)
                       ;






int
ppl_Rational_Box_CC76_widening_assign_with_tokens
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y, unsigned* tp)

                         ;






int
ppl_Rational_Box_CC76_widening_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;






int
ppl_Rational_Box_widening_assign_with_tokens
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y, unsigned* tp)

                         ;





int
ppl_Rational_Box_widening_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;





int
ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 6484 "/usr/local/include/ppl_c.h"
int
ppl_Rational_Box_limited_CC76_extrapolation_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 6497 "/usr/local/include/ppl_c.h"
int
ppl_Rational_Box_CC76_narrowing_assign
(ppl_Rational_Box_t x, ppl_const_Rational_Box_t y)
                                       ;






int
ppl_Rational_Box_linear_partition
(ppl_const_Rational_Box_t x, ppl_const_Rational_Box_t y, ppl_Rational_Box_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Rational_Box_wrap_assign
(ppl_Rational_Box_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Rational_Box_recycle_Constraint_System
(ppl_Rational_Box_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Rational_Box_recycle_Congruence_System
(ppl_Rational_Box_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Rational_Box_recycle_Generator_System
(ppl_Rational_Box_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_Rational_Box_from_Rational_Box
(ppl_Rational_Box_t dst, ppl_const_Rational_Box_t src);






int
ppl_Rational_Box_add_recycled_constraints
(ppl_Rational_Box_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Rational_Box_add_recycled_congruences
(ppl_Rational_Box_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_Rational_Box
(ppl_const_Rational_Box_t pset);


int
ppl_termination_test_PR_Rational_Box
(ppl_const_Rational_Box_t pset);







int
ppl_one_affine_ranking_function_MS_Rational_Box
(ppl_const_Rational_Box_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Rational_Box
(ppl_const_Rational_Box_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Rational_Box
(ppl_const_Rational_Box_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Rational_Box
(ppl_const_Rational_Box_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after);


int
ppl_termination_test_PR_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after);







int
ppl_one_affine_ranking_function_MS_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Rational_Box_2
(ppl_const_Rational_Box_t pset_before, ppl_const_Rational_Box_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Rational_Box (ppl_const_Rational_Box_t x); int ppl_io_fprint_Rational_Box (FILE* stream, ppl_const_Rational_Box_t x); int ppl_io_asprint_Rational_Box (char** strp, ppl_const_Rational_Box_t x); int ppl_Rational_Box_ascii_dump (ppl_const_Rational_Box_t x, FILE* stream); int ppl_Rational_Box_ascii_load (ppl_Rational_Box_t x, FILE* stream);
# 6690 "/usr/local/include/ppl_c.h"
int
ppl_delete_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t ph);





int
ppl_new_BD_Shape_mpz_class_from_space_dimension
(ppl_BD_Shape_mpz_class_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_BD_Shape_mpz_class_from_C_Polyhedron
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Grid
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Grid_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Rational_Box
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Double_Box
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_double
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_BD_Shape_mpz_class_from_Constraint_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_BD_Shape_mpz_class_from_Congruence_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_BD_Shape_mpz_class_from_Generator_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_const_Generator_System_t cs);
# 6849 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpz_class_space_dimension
(ppl_const_BD_Shape_mpz_class_t ph, ppl_dimension_type* m);


int
ppl_BD_Shape_mpz_class_affine_dimension
(ppl_const_BD_Shape_mpz_class_t ph, ppl_dimension_type* m);






int
ppl_BD_Shape_mpz_class_relation_with_Constraint
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpz_class_relation_with_Generator
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_BD_Shape_mpz_class_relation_with_Congruence
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpz_class_get_constraints
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_mpz_class_get_congruences
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_mpz_class_get_minimized_constraints
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_mpz_class_get_minimized_congruences
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_mpz_class_is_empty
(ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_BD_Shape_mpz_class_is_universe
(ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_BD_Shape_mpz_class_is_bounded
(ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_BD_Shape_mpz_class_contains_integer_point
(ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_BD_Shape_mpz_class_is_topologically_closed
(ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_BD_Shape_mpz_class_is_discrete
(ppl_const_BD_Shape_mpz_class_t ph);






int
ppl_BD_Shape_mpz_class_topological_closure_assign
(ppl_BD_Shape_mpz_class_t ph);






int
ppl_BD_Shape_mpz_class_bounds_from_above
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_BD_Shape_mpz_class_bounds_from_below
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_BD_Shape_mpz_class_maximize
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_BD_Shape_mpz_class_minimize
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_BD_Shape_mpz_class_maximize_with_point
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_BD_Shape_mpz_class_minimize_with_point
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_BD_Shape_mpz_class_frequency
(ppl_const_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;





int
ppl_BD_Shape_mpz_class_OK
(ppl_const_BD_Shape_mpz_class_t ph);





int
ppl_BD_Shape_mpz_class_add_constraint
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpz_class_add_congruence
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpz_class_add_constraints
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_mpz_class_add_congruences
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_mpz_class_refine_with_constraint
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpz_class_refine_with_congruence
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpz_class_refine_with_constraints
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_mpz_class_refine_with_congruences
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_mpz_class_intersection_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_upper_bound_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_difference_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_concatenate_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_time_elapse_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_simplify_using_context_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;





int
ppl_BD_Shape_mpz_class_constrains
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_mpz_class_unconstrain_space_dimension
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_mpz_class_unconstrain_space_dimensions
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_mpz_class_affine_image
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_BD_Shape_mpz_class_affine_preimage
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_BD_Shape_mpz_class_bounded_affine_image
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_mpz_class_bounded_affine_preimage
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_mpz_class_generalized_affine_image
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_mpz_class_generalized_affine_preimage
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs
(ppl_BD_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_BD_Shape_mpz_class_add_space_dimensions_and_embed
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;


int
ppl_BD_Shape_mpz_class_add_space_dimensions_and_project
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;






int
ppl_BD_Shape_mpz_class_remove_space_dimensions
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_mpz_class_remove_higher_space_dimensions
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;





int
ppl_BD_Shape_mpz_class_expand_space_dimension
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_BD_Shape_mpz_class_fold_space_dimensions
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_BD_Shape_mpz_class_map_space_dimensions
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_BD_Shape_mpz_class_drop_some_non_integer_points
(ppl_BD_Shape_mpz_class_t ph, int complexity)
                           ;





int
ppl_BD_Shape_mpz_class_drop_some_non_integer_points_2
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_BD_Shape_mpz_class_external_memory_in_bytes
(ppl_const_BD_Shape_mpz_class_t ps, size_t* sz)
                       ;


int
ppl_BD_Shape_mpz_class_total_memory_in_bytes
(ppl_const_BD_Shape_mpz_class_t ps, size_t* sz)
                       ;






int
ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, unsigned* tp)

                         ;


int
ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_mpz_class_BHMZ05_widening_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;


int
ppl_BD_Shape_mpz_class_H79_widening_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_widening_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, unsigned* tp)

                         ;





int
ppl_BD_Shape_mpz_class_widening_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;





int
ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 7490 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 7517 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_mpz_class_CC76_extrapolation_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_CC76_narrowing_assign
(ppl_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y)
                                             ;






int
ppl_BD_Shape_mpz_class_linear_partition
(ppl_const_BD_Shape_mpz_class_t x, ppl_const_BD_Shape_mpz_class_t y, ppl_BD_Shape_mpz_class_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_BD_Shape_mpz_class_wrap_assign
(ppl_BD_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_BD_Shape_mpz_class_recycle_Constraint_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_BD_Shape_mpz_class_recycle_Congruence_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_BD_Shape_mpz_class_recycle_Generator_System
(ppl_BD_Shape_mpz_class_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_BD_Shape_mpz_class_from_BD_Shape_mpz_class
(ppl_BD_Shape_mpz_class_t dst, ppl_const_BD_Shape_mpz_class_t src);






int
ppl_BD_Shape_mpz_class_add_recycled_constraints
(ppl_BD_Shape_mpz_class_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_BD_Shape_mpz_class_add_recycled_congruences
(ppl_BD_Shape_mpz_class_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset);


int
ppl_termination_test_PR_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset);







int
ppl_one_affine_ranking_function_MS_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_mpz_class
(ppl_const_BD_Shape_mpz_class_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after);


int
ppl_termination_test_PR_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after);







int
ppl_one_affine_ranking_function_MS_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_mpz_class_2
(ppl_const_BD_Shape_mpz_class_t pset_before, ppl_const_BD_Shape_mpz_class_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_BD_Shape_mpz_class (ppl_const_BD_Shape_mpz_class_t x); int ppl_io_fprint_BD_Shape_mpz_class (FILE* stream, ppl_const_BD_Shape_mpz_class_t x); int ppl_io_asprint_BD_Shape_mpz_class (char** strp, ppl_const_BD_Shape_mpz_class_t x); int ppl_BD_Shape_mpz_class_ascii_dump (ppl_const_BD_Shape_mpz_class_t x, FILE* stream); int ppl_BD_Shape_mpz_class_ascii_load (ppl_BD_Shape_mpz_class_t x, FILE* stream);
# 7731 "/usr/local/include/ppl_c.h"
int
ppl_delete_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t ph);





int
ppl_new_BD_Shape_mpq_class_from_space_dimension
(ppl_BD_Shape_mpq_class_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_BD_Shape_mpq_class_from_C_Polyhedron
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Grid
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Grid_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Rational_Box
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Double_Box
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_double
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_BD_Shape_mpq_class_from_Constraint_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_BD_Shape_mpq_class_from_Congruence_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_BD_Shape_mpq_class_from_Generator_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_const_Generator_System_t cs);
# 7890 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpq_class_space_dimension
(ppl_const_BD_Shape_mpq_class_t ph, ppl_dimension_type* m);


int
ppl_BD_Shape_mpq_class_affine_dimension
(ppl_const_BD_Shape_mpq_class_t ph, ppl_dimension_type* m);






int
ppl_BD_Shape_mpq_class_relation_with_Constraint
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpq_class_relation_with_Generator
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_BD_Shape_mpq_class_relation_with_Congruence
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpq_class_get_constraints
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_mpq_class_get_congruences
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_mpq_class_get_minimized_constraints
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_mpq_class_get_minimized_congruences
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_mpq_class_is_empty
(ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_BD_Shape_mpq_class_is_universe
(ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_BD_Shape_mpq_class_is_bounded
(ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_BD_Shape_mpq_class_contains_integer_point
(ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_BD_Shape_mpq_class_is_topologically_closed
(ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_BD_Shape_mpq_class_is_discrete
(ppl_const_BD_Shape_mpq_class_t ph);






int
ppl_BD_Shape_mpq_class_topological_closure_assign
(ppl_BD_Shape_mpq_class_t ph);






int
ppl_BD_Shape_mpq_class_bounds_from_above
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_BD_Shape_mpq_class_bounds_from_below
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_BD_Shape_mpq_class_maximize
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_BD_Shape_mpq_class_minimize
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_BD_Shape_mpq_class_maximize_with_point
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_BD_Shape_mpq_class_minimize_with_point
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_BD_Shape_mpq_class_frequency
(ppl_const_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;





int
ppl_BD_Shape_mpq_class_OK
(ppl_const_BD_Shape_mpq_class_t ph);





int
ppl_BD_Shape_mpq_class_add_constraint
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpq_class_add_congruence
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpq_class_add_constraints
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_mpq_class_add_congruences
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_mpq_class_refine_with_constraint
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_mpq_class_refine_with_congruence
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_mpq_class_refine_with_constraints
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_mpq_class_refine_with_congruences
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_mpq_class_intersection_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_upper_bound_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_difference_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_concatenate_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_time_elapse_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_simplify_using_context_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;





int
ppl_BD_Shape_mpq_class_constrains
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_mpq_class_unconstrain_space_dimension
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_mpq_class_unconstrain_space_dimensions
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_mpq_class_affine_image
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_BD_Shape_mpq_class_affine_preimage
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_BD_Shape_mpq_class_bounded_affine_image
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_mpq_class_bounded_affine_preimage
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_mpq_class_generalized_affine_image
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_mpq_class_generalized_affine_preimage
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
(ppl_BD_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_BD_Shape_mpq_class_add_space_dimensions_and_embed
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;


int
ppl_BD_Shape_mpq_class_add_space_dimensions_and_project
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;






int
ppl_BD_Shape_mpq_class_remove_space_dimensions
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_mpq_class_remove_higher_space_dimensions
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;





int
ppl_BD_Shape_mpq_class_expand_space_dimension
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_BD_Shape_mpq_class_fold_space_dimensions
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_BD_Shape_mpq_class_map_space_dimensions
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_BD_Shape_mpq_class_drop_some_non_integer_points
(ppl_BD_Shape_mpq_class_t ph, int complexity)
                           ;





int
ppl_BD_Shape_mpq_class_drop_some_non_integer_points_2
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_BD_Shape_mpq_class_external_memory_in_bytes
(ppl_const_BD_Shape_mpq_class_t ps, size_t* sz)
                       ;


int
ppl_BD_Shape_mpq_class_total_memory_in_bytes
(ppl_const_BD_Shape_mpq_class_t ps, size_t* sz)
                       ;






int
ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, unsigned* tp)

                         ;


int
ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_mpq_class_BHMZ05_widening_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;


int
ppl_BD_Shape_mpq_class_H79_widening_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_widening_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, unsigned* tp)

                         ;





int
ppl_BD_Shape_mpq_class_widening_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;





int
ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 8531 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 8558 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_mpq_class_CC76_extrapolation_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_CC76_narrowing_assign
(ppl_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y)
                                             ;






int
ppl_BD_Shape_mpq_class_linear_partition
(ppl_const_BD_Shape_mpq_class_t x, ppl_const_BD_Shape_mpq_class_t y, ppl_BD_Shape_mpq_class_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_BD_Shape_mpq_class_wrap_assign
(ppl_BD_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_BD_Shape_mpq_class_recycle_Constraint_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_BD_Shape_mpq_class_recycle_Congruence_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_BD_Shape_mpq_class_recycle_Generator_System
(ppl_BD_Shape_mpq_class_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_BD_Shape_mpq_class_from_BD_Shape_mpq_class
(ppl_BD_Shape_mpq_class_t dst, ppl_const_BD_Shape_mpq_class_t src);






int
ppl_BD_Shape_mpq_class_add_recycled_constraints
(ppl_BD_Shape_mpq_class_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_BD_Shape_mpq_class_add_recycled_congruences
(ppl_BD_Shape_mpq_class_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset);


int
ppl_termination_test_PR_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset);







int
ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class
(ppl_const_BD_Shape_mpq_class_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after);


int
ppl_termination_test_PR_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after);







int
ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class_2
(ppl_const_BD_Shape_mpq_class_t pset_before, ppl_const_BD_Shape_mpq_class_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_BD_Shape_mpq_class (ppl_const_BD_Shape_mpq_class_t x); int ppl_io_fprint_BD_Shape_mpq_class (FILE* stream, ppl_const_BD_Shape_mpq_class_t x); int ppl_io_asprint_BD_Shape_mpq_class (char** strp, ppl_const_BD_Shape_mpq_class_t x); int ppl_BD_Shape_mpq_class_ascii_dump (ppl_const_BD_Shape_mpq_class_t x, FILE* stream); int ppl_BD_Shape_mpq_class_ascii_load (ppl_BD_Shape_mpq_class_t x, FILE* stream);
# 8772 "/usr/local/include/ppl_c.h"
int
ppl_delete_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t ph);





int
ppl_new_Octagonal_Shape_mpz_class_from_space_dimension
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Grid
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Double_Box
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Octagonal_Shape_mpz_class_from_Constraint_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_mpz_class_from_Congruence_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_mpz_class_from_Generator_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_const_Generator_System_t cs);
# 8931 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpz_class_space_dimension
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type* m);


int
ppl_Octagonal_Shape_mpz_class_affine_dimension
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type* m);






int
ppl_Octagonal_Shape_mpz_class_relation_with_Constraint
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpz_class_relation_with_Generator
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Octagonal_Shape_mpz_class_relation_with_Congruence
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpz_class_get_constraints
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_mpz_class_get_congruences
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_mpz_class_get_minimized_constraints
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_mpz_class_get_minimized_congruences
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_mpz_class_is_empty
(ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_Octagonal_Shape_mpz_class_is_universe
(ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_Octagonal_Shape_mpz_class_is_bounded
(ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_Octagonal_Shape_mpz_class_contains_integer_point
(ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_Octagonal_Shape_mpz_class_is_topologically_closed
(ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_Octagonal_Shape_mpz_class_is_discrete
(ppl_const_Octagonal_Shape_mpz_class_t ph);






int
ppl_Octagonal_Shape_mpz_class_topological_closure_assign
(ppl_Octagonal_Shape_mpz_class_t ph);






int
ppl_Octagonal_Shape_mpz_class_bounds_from_above
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Octagonal_Shape_mpz_class_bounds_from_below
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Octagonal_Shape_mpz_class_maximize
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Octagonal_Shape_mpz_class_minimize
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Octagonal_Shape_mpz_class_maximize_with_point
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Octagonal_Shape_mpz_class_minimize_with_point
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Octagonal_Shape_mpz_class_frequency
(ppl_const_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpz_class_OK
(ppl_const_Octagonal_Shape_mpz_class_t ph);





int
ppl_Octagonal_Shape_mpz_class_add_constraint
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpz_class_add_congruence
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpz_class_add_constraints
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_mpz_class_add_congruences
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_mpz_class_refine_with_constraint
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpz_class_refine_with_congruence
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpz_class_refine_with_constraints
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_mpz_class_refine_with_congruences
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_mpz_class_intersection_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_upper_bound_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_difference_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_concatenate_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpz_class_time_elapse_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpz_class_constrains
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpz_class_affine_image
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Octagonal_Shape_mpz_class_affine_preimage
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Octagonal_Shape_mpz_class_bounded_affine_image
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_mpz_class_bounded_affine_preimage
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_mpz_class_generalized_affine_image
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_embed
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_project
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Octagonal_Shape_mpz_class_remove_space_dimensions
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpz_class_remove_higher_space_dimensions
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Octagonal_Shape_mpz_class_expand_space_dimension
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Octagonal_Shape_mpz_class_fold_space_dimensions
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Octagonal_Shape_mpz_class_map_space_dimensions
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpz_class_drop_some_non_integer_points
(ppl_Octagonal_Shape_mpz_class_t ph, int complexity)
                           ;





int
ppl_Octagonal_Shape_mpz_class_drop_some_non_integer_points_2
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes
(ppl_const_Octagonal_Shape_mpz_class_t ps, size_t* sz)
                       ;


int
ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes
(ppl_const_Octagonal_Shape_mpz_class_t ps, size_t* sz)
                       ;






int
ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, unsigned* tp)

                         ;





int
ppl_Octagonal_Shape_mpz_class_widening_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 9551 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 9571 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign
(ppl_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpz_class_linear_partition
(ppl_const_Octagonal_Shape_mpz_class_t x, ppl_const_Octagonal_Shape_mpz_class_t y, ppl_Octagonal_Shape_mpz_class_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Octagonal_Shape_mpz_class_wrap_assign
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Octagonal_Shape_mpz_class_recycle_Constraint_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_mpz_class_recycle_Congruence_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_mpz_class_recycle_Generator_System
(ppl_Octagonal_Shape_mpz_class_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class
(ppl_Octagonal_Shape_mpz_class_t dst, ppl_const_Octagonal_Shape_mpz_class_t src);






int
ppl_Octagonal_Shape_mpz_class_add_recycled_constraints
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Octagonal_Shape_mpz_class_add_recycled_congruences
(ppl_Octagonal_Shape_mpz_class_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset);


int
ppl_termination_test_PR_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpz_class
(ppl_const_Octagonal_Shape_mpz_class_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after);


int
ppl_termination_test_PR_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpz_class_2
(ppl_const_Octagonal_Shape_mpz_class_t pset_before, ppl_const_Octagonal_Shape_mpz_class_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Octagonal_Shape_mpz_class (ppl_const_Octagonal_Shape_mpz_class_t x); int ppl_io_fprint_Octagonal_Shape_mpz_class (FILE* stream, ppl_const_Octagonal_Shape_mpz_class_t x); int ppl_io_asprint_Octagonal_Shape_mpz_class (char** strp, ppl_const_Octagonal_Shape_mpz_class_t x); int ppl_Octagonal_Shape_mpz_class_ascii_dump (ppl_const_Octagonal_Shape_mpz_class_t x, FILE* stream); int ppl_Octagonal_Shape_mpz_class_ascii_load (ppl_Octagonal_Shape_mpz_class_t x, FILE* stream);
# 9785 "/usr/local/include/ppl_c.h"
int
ppl_delete_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t ph);





int
ppl_new_Octagonal_Shape_mpq_class_from_space_dimension
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Grid
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Double_Box
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Octagonal_Shape_mpq_class_from_Constraint_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_mpq_class_from_Congruence_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_mpq_class_from_Generator_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_const_Generator_System_t cs);
# 9944 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpq_class_space_dimension
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type* m);


int
ppl_Octagonal_Shape_mpq_class_affine_dimension
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type* m);






int
ppl_Octagonal_Shape_mpq_class_relation_with_Constraint
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpq_class_relation_with_Generator
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Octagonal_Shape_mpq_class_relation_with_Congruence
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpq_class_get_constraints
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_mpq_class_get_congruences
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_mpq_class_get_minimized_constraints
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_mpq_class_get_minimized_congruences
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_mpq_class_is_empty
(ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_Octagonal_Shape_mpq_class_is_universe
(ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_Octagonal_Shape_mpq_class_is_bounded
(ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_Octagonal_Shape_mpq_class_contains_integer_point
(ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_Octagonal_Shape_mpq_class_is_topologically_closed
(ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_Octagonal_Shape_mpq_class_is_discrete
(ppl_const_Octagonal_Shape_mpq_class_t ph);






int
ppl_Octagonal_Shape_mpq_class_topological_closure_assign
(ppl_Octagonal_Shape_mpq_class_t ph);






int
ppl_Octagonal_Shape_mpq_class_bounds_from_above
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Octagonal_Shape_mpq_class_bounds_from_below
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Octagonal_Shape_mpq_class_maximize
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Octagonal_Shape_mpq_class_minimize
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Octagonal_Shape_mpq_class_maximize_with_point
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Octagonal_Shape_mpq_class_minimize_with_point
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Octagonal_Shape_mpq_class_frequency
(ppl_const_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpq_class_OK
(ppl_const_Octagonal_Shape_mpq_class_t ph);





int
ppl_Octagonal_Shape_mpq_class_add_constraint
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpq_class_add_congruence
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpq_class_add_constraints
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_mpq_class_add_congruences
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_mpq_class_refine_with_constraint
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_mpq_class_refine_with_congruence
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_mpq_class_refine_with_constraints
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_mpq_class_refine_with_congruences
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_mpq_class_intersection_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_upper_bound_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_difference_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_concatenate_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;


int
ppl_Octagonal_Shape_mpq_class_time_elapse_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpq_class_constrains
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpq_class_affine_image
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Octagonal_Shape_mpq_class_affine_preimage
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Octagonal_Shape_mpq_class_bounded_affine_image
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_mpq_class_bounded_affine_preimage
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_mpq_class_generalized_affine_image
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_embed
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_project
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Octagonal_Shape_mpq_class_remove_space_dimensions
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpq_class_remove_higher_space_dimensions
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Octagonal_Shape_mpq_class_expand_space_dimension
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Octagonal_Shape_mpq_class_fold_space_dimensions
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Octagonal_Shape_mpq_class_map_space_dimensions
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Octagonal_Shape_mpq_class_drop_some_non_integer_points
(ppl_Octagonal_Shape_mpq_class_t ph, int complexity)
                           ;





int
ppl_Octagonal_Shape_mpq_class_drop_some_non_integer_points_2
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes
(ppl_const_Octagonal_Shape_mpq_class_t ps, size_t* sz)
                       ;


int
ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes
(ppl_const_Octagonal_Shape_mpq_class_t ps, size_t* sz)
                       ;






int
ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, unsigned* tp)

                         ;





int
ppl_Octagonal_Shape_mpq_class_widening_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;





int
ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 10564 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 10584 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign
(ppl_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y)
                                                    ;






int
ppl_Octagonal_Shape_mpq_class_linear_partition
(ppl_const_Octagonal_Shape_mpq_class_t x, ppl_const_Octagonal_Shape_mpq_class_t y, ppl_Octagonal_Shape_mpq_class_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Octagonal_Shape_mpq_class_wrap_assign
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Octagonal_Shape_mpq_class_recycle_Constraint_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_mpq_class_recycle_Congruence_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_mpq_class_recycle_Generator_System
(ppl_Octagonal_Shape_mpq_class_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class
(ppl_Octagonal_Shape_mpq_class_t dst, ppl_const_Octagonal_Shape_mpq_class_t src);






int
ppl_Octagonal_Shape_mpq_class_add_recycled_constraints
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Octagonal_Shape_mpq_class_add_recycled_congruences
(ppl_Octagonal_Shape_mpq_class_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset);


int
ppl_termination_test_PR_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class
(ppl_const_Octagonal_Shape_mpq_class_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after);


int
ppl_termination_test_PR_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class_2
(ppl_const_Octagonal_Shape_mpq_class_t pset_before, ppl_const_Octagonal_Shape_mpq_class_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Octagonal_Shape_mpq_class (ppl_const_Octagonal_Shape_mpq_class_t x); int ppl_io_fprint_Octagonal_Shape_mpq_class (FILE* stream, ppl_const_Octagonal_Shape_mpq_class_t x); int ppl_io_asprint_Octagonal_Shape_mpq_class (char** strp, ppl_const_Octagonal_Shape_mpq_class_t x); int ppl_Octagonal_Shape_mpq_class_ascii_dump (ppl_const_Octagonal_Shape_mpq_class_t x, FILE* stream); int ppl_Octagonal_Shape_mpq_class_ascii_load (ppl_Octagonal_Shape_mpq_class_t x, FILE* stream);
# 10798 "/usr/local/include/ppl_c.h"
int
ppl_delete_Constraints_Product_C_Polyhedron_Grid
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);





int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_double_t ph);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);







int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, int complexity);







int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraint_System
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Congruence_System
(ppl_Constraints_Product_C_Polyhedron_Grid_t* pph, ppl_const_Congruence_System_t cs);
# 10962 "/usr/local/include/ppl_c.h"
int
ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type* m);


int
ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type* m);






int
ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_Constraint
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_Generator
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_Congruence
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_is_empty
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);


int
ppl_Constraints_Product_C_Polyhedron_Grid_is_universe
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);


int
ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);


int
ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);


int
ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);






int
ppl_Constraints_Product_C_Polyhedron_Grid_topological_closure_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph);






int
ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_maximize
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_minimize
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_OK
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ph);





int
ppl_Constraints_Product_C_Polyhedron_Grid_add_constraint
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_add_congruence
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_add_constraints
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_add_congruences
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraint
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruence
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraints
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruences
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_intersection_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_difference_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_concatenate_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_time_elapse_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_constrains
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_affine_image
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_affine_preimage
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_image
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_preimage
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_embed
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_project
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_remove_higher_space_dimensions
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_expand_space_dimension
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_drop_some_non_integer_points
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, int complexity)
                           ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_drop_some_non_integer_points_2
(ppl_Constraints_Product_C_Polyhedron_Grid_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ps, size_t* sz)
                       ;


int
ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes
(ppl_const_Constraints_Product_C_Polyhedron_Grid_t ps, size_t* sz)
                       ;






int
ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y, unsigned* tp)

                         ;





int
ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign
(ppl_Constraints_Product_C_Polyhedron_Grid_t x, ppl_const_Constraints_Product_C_Polyhedron_Grid_t y)
                                                                ;





int ppl_io_print_Constraints_Product_C_Polyhedron_Grid (ppl_const_Constraints_Product_C_Polyhedron_Grid_t x); int ppl_io_fprint_Constraints_Product_C_Polyhedron_Grid (FILE* stream, ppl_const_Constraints_Product_C_Polyhedron_Grid_t x); int ppl_io_asprint_Constraints_Product_C_Polyhedron_Grid (char** strp, ppl_const_Constraints_Product_C_Polyhedron_Grid_t x); int ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump (ppl_const_Constraints_Product_C_Polyhedron_Grid_t x, FILE* stream); int ppl_Constraints_Product_C_Polyhedron_Grid_ascii_load (ppl_Constraints_Product_C_Polyhedron_Grid_t x, FILE* stream);
# 11497 "/usr/local/include/ppl_c.h"
typedef struct ppl_Pointset_Powerset_C_Polyhedron_iterator_tag* ppl_Pointset_Powerset_C_Polyhedron_iterator_t; typedef struct ppl_Pointset_Powerset_C_Polyhedron_iterator_tag const* ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t;
typedef struct ppl_Pointset_Powerset_C_Polyhedron_const_iterator_tag* ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t; typedef struct ppl_Pointset_Powerset_C_Polyhedron_const_iterator_tag const* ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t;


int
ppl_new_Pointset_Powerset_C_Polyhedron_iterator
(ppl_Pointset_Powerset_C_Polyhedron_iterator_t* pit);


int
ppl_new_Pointset_Powerset_C_Polyhedron_const_iterator
(ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t* pit);





int
ppl_delete_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);





int
ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);







int
ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Pointset_Powerset_C_Polyhedron_t ph, int complexity);


int
ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);







int
ppl_new_Pointset_Powerset_C_Polyhedron_from_Constraint_System
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Pointset_Powerset_C_Polyhedron_from_Congruence_System
(ppl_Pointset_Powerset_C_Polyhedron_t* pph, ppl_const_Congruence_System_t cs);
# 11578 "/usr/local/include/ppl_c.h"
int
ppl_Pointset_Powerset_C_Polyhedron_space_dimension
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type* m);


int
ppl_Pointset_Powerset_C_Polyhedron_affine_dimension
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type* m);






int
ppl_Pointset_Powerset_C_Polyhedron_relation_with_Constraint
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_C_Polyhedron_relation_with_Generator
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Pointset_Powerset_C_Polyhedron_relation_with_Congruence
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_C_Polyhedron_is_empty
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_is_universe
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_is_bounded
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_is_discrete
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);






int
ppl_Pointset_Powerset_C_Polyhedron_topological_closure_assign
(ppl_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_pairwise_reduce
(ppl_Pointset_Powerset_C_Polyhedron_t ph);


int
ppl_Pointset_Powerset_C_Polyhedron_omega_reduce
(ppl_Pointset_Powerset_C_Polyhedron_t ph);






int
ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Pointset_Powerset_C_Polyhedron_maximize
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Pointset_Powerset_C_Polyhedron_minimize
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;






int
ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron
(ppl_const_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;





int
ppl_Pointset_Powerset_C_Polyhedron_OK
(ppl_const_Pointset_Powerset_C_Polyhedron_t ph);





int
ppl_Pointset_Powerset_C_Polyhedron_add_constraint
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_C_Polyhedron_add_congruence
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_C_Polyhedron_add_constraints
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Pointset_Powerset_C_Polyhedron_add_congruences
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraint
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruence
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraints
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruences
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Pointset_Powerset_C_Polyhedron_intersection_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_difference_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_concatenate_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_time_elapse_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;






int
ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;






int
ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;





int
ppl_Pointset_Powerset_C_Polyhedron_constrains
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Pointset_Powerset_C_Polyhedron_affine_image
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Pointset_Powerset_C_Polyhedron_affine_preimage
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_image
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_preimage
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_embed
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_project
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Pointset_Powerset_C_Polyhedron_remove_higher_space_dimensions
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Pointset_Powerset_C_Polyhedron_expand_space_dimension
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Pointset_Powerset_C_Polyhedron_drop_some_non_integer_points
(ppl_Pointset_Powerset_C_Polyhedron_t ph, int complexity)
                           ;





int
ppl_Pointset_Powerset_C_Polyhedron_drop_some_non_integer_points_2
(ppl_Pointset_Powerset_C_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes
(ppl_const_Pointset_Powerset_C_Polyhedron_t ps, size_t* sz)
                       ;


int
ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes
(ppl_const_Pointset_Powerset_C_Polyhedron_t ps, size_t* sz)
                       ;


int
ppl_Pointset_Powerset_C_Polyhedron_size
(ppl_const_Pointset_Powerset_C_Polyhedron_t ps, size_t* sz)
                       ;






int
ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator
(ppl_Pointset_Powerset_C_Polyhedron_iterator_t* px, ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t y)
                                                                  ;


int
ppl_new_Pointset_Powerset_C_Polyhedron_const_iterator_from_const_iterator
(ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t* px, ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t y)
                                                                        ;





int
ppl_Pointset_Powerset_C_Polyhedron_iterator_begin
(ppl_Pointset_Powerset_C_Polyhedron_t ps, ppl_Pointset_Powerset_C_Polyhedron_iterator_t psit)
                                                               ;


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_begin
(ppl_const_Pointset_Powerset_C_Polyhedron_t ps, ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t psit)
                                                                     ;


int
ppl_Pointset_Powerset_C_Polyhedron_iterator_end
(ppl_Pointset_Powerset_C_Polyhedron_t ps, ppl_Pointset_Powerset_C_Polyhedron_iterator_t psit)
                                                               ;


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_end
(ppl_const_Pointset_Powerset_C_Polyhedron_t ps, ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t psit)
                                                                     ;






int
ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test
(ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t x, ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t y)
                                                                  ;


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_equal_test
(ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t x, ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t y)
                                                                        ;





int
ppl_Pointset_Powerset_C_Polyhedron_iterator_increment
(ppl_Pointset_Powerset_C_Polyhedron_iterator_t psit);


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_increment
(ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t psit);


int
ppl_Pointset_Powerset_C_Polyhedron_iterator_decrement
(ppl_Pointset_Powerset_C_Polyhedron_iterator_t psit);


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_decrement
(ppl_Pointset_Powerset_C_Polyhedron_const_iterator_t psit);






int
ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference
(ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t ps, ppl_const_Polyhedron_t* d)
                                      ;


int
ppl_Pointset_Powerset_C_Polyhedron_const_iterator_dereference
(ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t ps, ppl_const_Polyhedron_t* d)
                                      ;






int
ppl_delete_Pointset_Powerset_C_Polyhedron_iterator
(ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t psit);


int
ppl_delete_Pointset_Powerset_C_Polyhedron_const_iterator
(ppl_const_Pointset_Powerset_C_Polyhedron_const_iterator_t psit);





int
ppl_Pointset_Powerset_C_Polyhedron_add_disjunct
(ppl_Pointset_Powerset_C_Polyhedron_t ps, ppl_const_Polyhedron_t d)
                                     ;






int
ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct
(ppl_Pointset_Powerset_C_Polyhedron_t ps, ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t cit, ppl_Pointset_Powerset_C_Polyhedron_iterator_t it)

                                                   ;





int
ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts
(ppl_Pointset_Powerset_C_Polyhedron_t ps, ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t first, ppl_const_Pointset_Powerset_C_Polyhedron_iterator_t last)

                                                           ;





int
ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;


int
ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y)
                                                         ;






int
ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y, int disjuncts)

                          ;


int
ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign
(ppl_Pointset_Powerset_C_Polyhedron_t x, ppl_const_Pointset_Powerset_C_Polyhedron_t y, int disjuncts)

                          ;






int ppl_io_print_Pointset_Powerset_C_Polyhedron (ppl_const_Pointset_Powerset_C_Polyhedron_t x); int ppl_io_fprint_Pointset_Powerset_C_Polyhedron (FILE* stream, ppl_const_Pointset_Powerset_C_Polyhedron_t x); int ppl_io_asprint_Pointset_Powerset_C_Polyhedron (char** strp, ppl_const_Pointset_Powerset_C_Polyhedron_t x); int ppl_Pointset_Powerset_C_Polyhedron_ascii_dump (ppl_const_Pointset_Powerset_C_Polyhedron_t x, FILE* stream); int ppl_Pointset_Powerset_C_Polyhedron_ascii_load (ppl_Pointset_Powerset_C_Polyhedron_t x, FILE* stream);
# 12311 "/usr/local/include/ppl_c.h"
typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_iterator_tag* ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t; typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_iterator_tag const* ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t;
typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_tag* ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t; typedef struct ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_tag const* ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t;


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator
(ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t* pit);


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_const_iterator
(ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t* pit);





int
ppl_delete_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);





int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Polyhedron_t ph);







int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, int complexity);


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Polyhedron_t ph, int complexity);







int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Constraint_System
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Congruence_System
(ppl_Pointset_Powerset_NNC_Polyhedron_t* pph, ppl_const_Congruence_System_t cs);
# 12392 "/usr/local/include/ppl_c.h"
int
ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type* m);


int
ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type* m);






int
ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_Constraint
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_Generator
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_Congruence
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_is_empty
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_is_universe
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);






int
ppl_Pointset_Powerset_NNC_Polyhedron_topological_closure_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph);


int
ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph);






int
ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_maximize
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_minimize
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_OK
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ph);





int
ppl_Pointset_Powerset_NNC_Polyhedron_add_constraint
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_add_congruence
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_add_constraints
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_add_congruences
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraint
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruence
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraints
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruences
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_intersection_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_difference_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_concatenate_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_time_elapse_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_constrains
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_affine_image
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_affine_preimage
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_image
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_preimage
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_embed
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_project
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_remove_higher_space_dimensions
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_expand_space_dimension
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_drop_some_non_integer_points
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, int complexity)
                           ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_drop_some_non_integer_points_2
(ppl_Pointset_Powerset_NNC_Polyhedron_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ps, size_t* sz)
                       ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ps, size_t* sz)
                       ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_size
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ps, size_t* sz)
                       ;






int
ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator
(ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t* px, ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t y)
                                                                    ;


int
ppl_new_Pointset_Powerset_NNC_Polyhedron_const_iterator_from_const_iterator
(ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t* px, ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t y)
                                                                          ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_begin
(ppl_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t psit)
                                                                 ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_begin
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t psit)
                                                                       ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_end
(ppl_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t psit)
                                                                 ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_end
(ppl_const_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t psit)
                                                                       ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equal_test
(ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t y)
                                                                    ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_equal_test
(ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t y)
                                                                          ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_increment
(ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t psit);


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_increment
(ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t psit);


int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_decrement
(ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t psit);


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_decrement
(ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_t psit);






int
ppl_Pointset_Powerset_NNC_Polyhedron_iterator_dereference
(ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t ps, ppl_const_Polyhedron_t* d)
                                      ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_const_iterator_dereference
(ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t ps, ppl_const_Polyhedron_t* d)
                                      ;






int
ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator
(ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t psit);


int
ppl_delete_Pointset_Powerset_NNC_Polyhedron_const_iterator
(ppl_const_Pointset_Powerset_NNC_Polyhedron_const_iterator_t psit);





int
ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct
(ppl_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_const_Polyhedron_t d)
                                     ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct
(ppl_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t cit, ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t it)

                                                     ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts
(ppl_Pointset_Powerset_NNC_Polyhedron_t ps, ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t first, ppl_const_Pointset_Powerset_NNC_Polyhedron_iterator_t last)

                                                             ;





int
ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y)
                                                           ;






int
ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y, int disjuncts)

                          ;


int
ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign
(ppl_Pointset_Powerset_NNC_Polyhedron_t x, ppl_const_Pointset_Powerset_NNC_Polyhedron_t y, int disjuncts)

                          ;






int ppl_io_print_Pointset_Powerset_NNC_Polyhedron (ppl_const_Pointset_Powerset_NNC_Polyhedron_t x); int ppl_io_fprint_Pointset_Powerset_NNC_Polyhedron (FILE* stream, ppl_const_Pointset_Powerset_NNC_Polyhedron_t x); int ppl_io_asprint_Pointset_Powerset_NNC_Polyhedron (char** strp, ppl_const_Pointset_Powerset_NNC_Polyhedron_t x); int ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump (ppl_const_Pointset_Powerset_NNC_Polyhedron_t x, FILE* stream); int ppl_Pointset_Powerset_NNC_Polyhedron_ascii_load (ppl_Pointset_Powerset_NNC_Polyhedron_t x, FILE* stream);
# 13117 "/usr/local/include/ppl_c.h"
int
ppl_delete_Double_Box
(ppl_const_Double_Box_t ph);





int
ppl_new_Double_Box_from_space_dimension
(ppl_Double_Box_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Double_Box_from_C_Polyhedron
(ppl_Double_Box_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Double_Box_from_NNC_Polyhedron
(ppl_Double_Box_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Double_Box_from_Grid
(ppl_Double_Box_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Double_Box_from_Rational_Box
(ppl_Double_Box_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Double_Box_from_BD_Shape_mpz_class
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Double_Box_from_BD_Shape_mpq_class
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Double_Box_from_Octagonal_Shape_mpz_class
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Double_Box_from_Octagonal_Shape_mpq_class
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Double_Box_from_Double_Box
(ppl_Double_Box_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Double_Box_from_BD_Shape_double
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Double_Box_from_Octagonal_Shape_double
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Double_Box_from_C_Polyhedron_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Double_Box_from_Grid_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Double_Box_from_Rational_Box_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Double_Box_from_Double_Box_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Double_Box_from_BD_Shape_double_with_complexity
(ppl_Double_Box_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity
(ppl_Double_Box_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Double_Box_from_Constraint_System
(ppl_Double_Box_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Double_Box_from_Congruence_System
(ppl_Double_Box_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Double_Box_from_Generator_System
(ppl_Double_Box_t* pph, ppl_const_Generator_System_t cs);
# 13276 "/usr/local/include/ppl_c.h"
int
ppl_Double_Box_space_dimension
(ppl_const_Double_Box_t ph, ppl_dimension_type* m);


int
ppl_Double_Box_affine_dimension
(ppl_const_Double_Box_t ph, ppl_dimension_type* m);






int
ppl_Double_Box_relation_with_Constraint
(ppl_const_Double_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Double_Box_relation_with_Generator
(ppl_const_Double_Box_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Double_Box_relation_with_Congruence
(ppl_const_Double_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Double_Box_get_constraints
(ppl_const_Double_Box_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Double_Box_get_congruences
(ppl_const_Double_Box_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Double_Box_get_minimized_constraints
(ppl_const_Double_Box_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Double_Box_get_minimized_congruences
(ppl_const_Double_Box_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Double_Box_is_empty
(ppl_const_Double_Box_t ph);


int
ppl_Double_Box_is_universe
(ppl_const_Double_Box_t ph);


int
ppl_Double_Box_is_bounded
(ppl_const_Double_Box_t ph);


int
ppl_Double_Box_contains_integer_point
(ppl_const_Double_Box_t ph);


int
ppl_Double_Box_is_topologically_closed
(ppl_const_Double_Box_t ph);


int
ppl_Double_Box_is_discrete
(ppl_const_Double_Box_t ph);






int
ppl_Double_Box_topological_closure_assign
(ppl_Double_Box_t ph);






int
ppl_Double_Box_bounds_from_above
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Double_Box_bounds_from_below
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Double_Box_get_upper_bound
(ppl_Double_Box_t ps, ppl_dimension_type var, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* pclosed)



                         ;


int
ppl_Double_Box_get_lower_bound
(ppl_Double_Box_t ps, ppl_dimension_type var, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* pclosed)



                         ;






int
ppl_Double_Box_maximize
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Double_Box_minimize
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Double_Box_maximize_with_point
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Double_Box_minimize_with_point
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Double_Box_frequency
(ppl_const_Double_Box_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Double_Box_contains_Double_Box
(ppl_const_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_strictly_contains_Double_Box
(ppl_const_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_is_disjoint_from_Double_Box
(ppl_const_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;






int
ppl_Double_Box_equals_Double_Box
(ppl_const_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;





int
ppl_Double_Box_OK
(ppl_const_Double_Box_t ph);





int
ppl_Double_Box_add_constraint
(ppl_Double_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Double_Box_add_congruence
(ppl_Double_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Double_Box_add_constraints
(ppl_Double_Box_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Double_Box_add_congruences
(ppl_Double_Box_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Double_Box_refine_with_constraint
(ppl_Double_Box_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Double_Box_refine_with_congruence
(ppl_Double_Box_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Double_Box_refine_with_constraints
(ppl_Double_Box_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Double_Box_refine_with_congruences
(ppl_Double_Box_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Double_Box_intersection_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_upper_bound_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_difference_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_concatenate_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;


int
ppl_Double_Box_time_elapse_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;






int
ppl_Double_Box_upper_bound_assign_if_exact
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;






int
ppl_Double_Box_simplify_using_context_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;





int
ppl_Double_Box_constrains
(ppl_Double_Box_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Double_Box_unconstrain_space_dimension
(ppl_Double_Box_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Double_Box_unconstrain_space_dimensions
(ppl_Double_Box_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Double_Box_affine_image
(ppl_Double_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Double_Box_affine_preimage
(ppl_Double_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Double_Box_bounded_affine_image
(ppl_Double_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Double_Box_bounded_affine_preimage
(ppl_Double_Box_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Double_Box_generalized_affine_image
(ppl_Double_Box_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Double_Box_generalized_affine_preimage
(ppl_Double_Box_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Double_Box_generalized_affine_image_lhs_rhs
(ppl_Double_Box_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Double_Box_generalized_affine_preimage_lhs_rhs
(ppl_Double_Box_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Double_Box_add_space_dimensions_and_embed
(ppl_Double_Box_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Double_Box_add_space_dimensions_and_project
(ppl_Double_Box_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Double_Box_remove_space_dimensions
(ppl_Double_Box_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Double_Box_remove_higher_space_dimensions
(ppl_Double_Box_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Double_Box_expand_space_dimension
(ppl_Double_Box_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Double_Box_fold_space_dimensions
(ppl_Double_Box_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Double_Box_map_space_dimensions
(ppl_Double_Box_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Double_Box_drop_some_non_integer_points
(ppl_Double_Box_t ph, int complexity)
                           ;





int
ppl_Double_Box_drop_some_non_integer_points_2
(ppl_Double_Box_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Double_Box_external_memory_in_bytes
(ppl_const_Double_Box_t ps, size_t* sz)
                       ;


int
ppl_Double_Box_total_memory_in_bytes
(ppl_const_Double_Box_t ps, size_t* sz)
                       ;






int
ppl_Double_Box_CC76_widening_assign_with_tokens
(ppl_Double_Box_t x, ppl_const_Double_Box_t y, unsigned* tp)

                         ;






int
ppl_Double_Box_CC76_widening_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;






int
ppl_Double_Box_widening_assign_with_tokens
(ppl_Double_Box_t x, ppl_const_Double_Box_t y, unsigned* tp)

                         ;





int
ppl_Double_Box_widening_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;





int
ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens
(ppl_Double_Box_t x, ppl_const_Double_Box_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 13910 "/usr/local/include/ppl_c.h"
int
ppl_Double_Box_limited_CC76_extrapolation_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 13923 "/usr/local/include/ppl_c.h"
int
ppl_Double_Box_CC76_narrowing_assign
(ppl_Double_Box_t x, ppl_const_Double_Box_t y)
                                     ;






int
ppl_Double_Box_linear_partition
(ppl_const_Double_Box_t x, ppl_const_Double_Box_t y, ppl_Double_Box_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Double_Box_wrap_assign
(ppl_Double_Box_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Double_Box_recycle_Constraint_System
(ppl_Double_Box_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Double_Box_recycle_Congruence_System
(ppl_Double_Box_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Double_Box_recycle_Generator_System
(ppl_Double_Box_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_Double_Box_from_Double_Box
(ppl_Double_Box_t dst, ppl_const_Double_Box_t src);






int
ppl_Double_Box_add_recycled_constraints
(ppl_Double_Box_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Double_Box_add_recycled_congruences
(ppl_Double_Box_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_Double_Box
(ppl_const_Double_Box_t pset);


int
ppl_termination_test_PR_Double_Box
(ppl_const_Double_Box_t pset);







int
ppl_one_affine_ranking_function_MS_Double_Box
(ppl_const_Double_Box_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Double_Box
(ppl_const_Double_Box_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Double_Box
(ppl_const_Double_Box_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Double_Box
(ppl_const_Double_Box_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after);


int
ppl_termination_test_PR_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after);







int
ppl_one_affine_ranking_function_MS_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Double_Box_2
(ppl_const_Double_Box_t pset_before, ppl_const_Double_Box_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Double_Box (ppl_const_Double_Box_t x); int ppl_io_fprint_Double_Box (FILE* stream, ppl_const_Double_Box_t x); int ppl_io_asprint_Double_Box (char** strp, ppl_const_Double_Box_t x); int ppl_Double_Box_ascii_dump (ppl_const_Double_Box_t x, FILE* stream); int ppl_Double_Box_ascii_load (ppl_Double_Box_t x, FILE* stream);
# 14116 "/usr/local/include/ppl_c.h"
int
ppl_delete_BD_Shape_double
(ppl_const_BD_Shape_double_t ph);





int
ppl_new_BD_Shape_double_from_space_dimension
(ppl_BD_Shape_double_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_BD_Shape_double_from_C_Polyhedron
(ppl_BD_Shape_double_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_double_from_NNC_Polyhedron
(ppl_BD_Shape_double_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_BD_Shape_double_from_Grid
(ppl_BD_Shape_double_t* pph, ppl_const_Grid_t ph);


int
ppl_new_BD_Shape_double_from_Rational_Box
(ppl_BD_Shape_double_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_BD_Shape_double_from_BD_Shape_mpz_class
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_double_from_BD_Shape_mpq_class
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_BD_Shape_double_from_Double_Box
(ppl_BD_Shape_double_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_BD_Shape_double_from_BD_Shape_double
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_double
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Grid_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Rational_Box_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Double_Box_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity
(ppl_BD_Shape_double_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_BD_Shape_double_from_Constraint_System
(ppl_BD_Shape_double_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_BD_Shape_double_from_Congruence_System
(ppl_BD_Shape_double_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_BD_Shape_double_from_Generator_System
(ppl_BD_Shape_double_t* pph, ppl_const_Generator_System_t cs);
# 14275 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_double_space_dimension
(ppl_const_BD_Shape_double_t ph, ppl_dimension_type* m);


int
ppl_BD_Shape_double_affine_dimension
(ppl_const_BD_Shape_double_t ph, ppl_dimension_type* m);






int
ppl_BD_Shape_double_relation_with_Constraint
(ppl_const_BD_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_double_relation_with_Generator
(ppl_const_BD_Shape_double_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_BD_Shape_double_relation_with_Congruence
(ppl_const_BD_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_double_get_constraints
(ppl_const_BD_Shape_double_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_double_get_congruences
(ppl_const_BD_Shape_double_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_double_get_minimized_constraints
(ppl_const_BD_Shape_double_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_BD_Shape_double_get_minimized_congruences
(ppl_const_BD_Shape_double_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_BD_Shape_double_is_empty
(ppl_const_BD_Shape_double_t ph);


int
ppl_BD_Shape_double_is_universe
(ppl_const_BD_Shape_double_t ph);


int
ppl_BD_Shape_double_is_bounded
(ppl_const_BD_Shape_double_t ph);


int
ppl_BD_Shape_double_contains_integer_point
(ppl_const_BD_Shape_double_t ph);


int
ppl_BD_Shape_double_is_topologically_closed
(ppl_const_BD_Shape_double_t ph);


int
ppl_BD_Shape_double_is_discrete
(ppl_const_BD_Shape_double_t ph);






int
ppl_BD_Shape_double_topological_closure_assign
(ppl_BD_Shape_double_t ph);






int
ppl_BD_Shape_double_bounds_from_above
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_BD_Shape_double_bounds_from_below
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_BD_Shape_double_maximize
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_BD_Shape_double_minimize
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_BD_Shape_double_maximize_with_point
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_BD_Shape_double_minimize_with_point
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_BD_Shape_double_frequency
(ppl_const_BD_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_BD_Shape_double_contains_BD_Shape_double
(ppl_const_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_strictly_contains_BD_Shape_double
(ppl_const_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double
(ppl_const_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_equals_BD_Shape_double
(ppl_const_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;





int
ppl_BD_Shape_double_OK
(ppl_const_BD_Shape_double_t ph);





int
ppl_BD_Shape_double_add_constraint
(ppl_BD_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_double_add_congruence
(ppl_BD_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_double_add_constraints
(ppl_BD_Shape_double_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_double_add_congruences
(ppl_BD_Shape_double_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_double_refine_with_constraint
(ppl_BD_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_BD_Shape_double_refine_with_congruence
(ppl_BD_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_BD_Shape_double_refine_with_constraints
(ppl_BD_Shape_double_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_BD_Shape_double_refine_with_congruences
(ppl_BD_Shape_double_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_BD_Shape_double_intersection_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_upper_bound_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_difference_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_concatenate_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_time_elapse_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_upper_bound_assign_if_exact
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_simplify_using_context_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;





int
ppl_BD_Shape_double_constrains
(ppl_BD_Shape_double_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_double_unconstrain_space_dimension
(ppl_BD_Shape_double_t ph, ppl_dimension_type var)
                                   ;





int
ppl_BD_Shape_double_unconstrain_space_dimensions
(ppl_BD_Shape_double_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_double_affine_image
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_BD_Shape_double_affine_preimage
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_BD_Shape_double_bounded_affine_image
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_double_bounded_affine_preimage
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_double_generalized_affine_image
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_BD_Shape_double_generalized_affine_preimage
(ppl_BD_Shape_double_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_BD_Shape_double_generalized_affine_image_lhs_rhs
(ppl_BD_Shape_double_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs
(ppl_BD_Shape_double_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_BD_Shape_double_add_space_dimensions_and_embed
(ppl_BD_Shape_double_t ph, ppl_dimension_type d)
                                 ;


int
ppl_BD_Shape_double_add_space_dimensions_and_project
(ppl_BD_Shape_double_t ph, ppl_dimension_type d)
                                 ;






int
ppl_BD_Shape_double_remove_space_dimensions
(ppl_BD_Shape_double_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_BD_Shape_double_remove_higher_space_dimensions
(ppl_BD_Shape_double_t ph, ppl_dimension_type d)
                                 ;





int
ppl_BD_Shape_double_expand_space_dimension
(ppl_BD_Shape_double_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_BD_Shape_double_fold_space_dimensions
(ppl_BD_Shape_double_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_BD_Shape_double_map_space_dimensions
(ppl_BD_Shape_double_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_BD_Shape_double_drop_some_non_integer_points
(ppl_BD_Shape_double_t ph, int complexity)
                           ;





int
ppl_BD_Shape_double_drop_some_non_integer_points_2
(ppl_BD_Shape_double_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_BD_Shape_double_external_memory_in_bytes
(ppl_const_BD_Shape_double_t ps, size_t* sz)
                       ;


int
ppl_BD_Shape_double_total_memory_in_bytes
(ppl_const_BD_Shape_double_t ps, size_t* sz)
                       ;






int
ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, unsigned* tp)

                         ;


int
ppl_BD_Shape_double_H79_widening_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_double_BHMZ05_widening_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;


int
ppl_BD_Shape_double_H79_widening_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_widening_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, unsigned* tp)

                         ;





int
ppl_BD_Shape_double_widening_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;





int
ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 14916 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_double_limited_H79_extrapolation_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_BD_Shape_double_limited_CC76_extrapolation_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 14943 "/usr/local/include/ppl_c.h"
int
ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, unsigned* tp)

                         ;






int
ppl_BD_Shape_double_CC76_extrapolation_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_CC76_narrowing_assign
(ppl_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y)
                                          ;






int
ppl_BD_Shape_double_linear_partition
(ppl_const_BD_Shape_double_t x, ppl_const_BD_Shape_double_t y, ppl_BD_Shape_double_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_BD_Shape_double_wrap_assign
(ppl_BD_Shape_double_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_BD_Shape_double_recycle_Constraint_System
(ppl_BD_Shape_double_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_BD_Shape_double_recycle_Congruence_System
(ppl_BD_Shape_double_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_BD_Shape_double_recycle_Generator_System
(ppl_BD_Shape_double_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_BD_Shape_double_from_BD_Shape_double
(ppl_BD_Shape_double_t dst, ppl_const_BD_Shape_double_t src);






int
ppl_BD_Shape_double_add_recycled_constraints
(ppl_BD_Shape_double_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_BD_Shape_double_add_recycled_congruences
(ppl_BD_Shape_double_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_BD_Shape_double
(ppl_const_BD_Shape_double_t pset);


int
ppl_termination_test_PR_BD_Shape_double
(ppl_const_BD_Shape_double_t pset);







int
ppl_one_affine_ranking_function_MS_BD_Shape_double
(ppl_const_BD_Shape_double_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_double
(ppl_const_BD_Shape_double_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_double
(ppl_const_BD_Shape_double_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_double
(ppl_const_BD_Shape_double_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after);


int
ppl_termination_test_PR_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after);







int
ppl_one_affine_ranking_function_MS_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_BD_Shape_double_2
(ppl_const_BD_Shape_double_t pset_before, ppl_const_BD_Shape_double_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_BD_Shape_double (ppl_const_BD_Shape_double_t x); int ppl_io_fprint_BD_Shape_double (FILE* stream, ppl_const_BD_Shape_double_t x); int ppl_io_asprint_BD_Shape_double (char** strp, ppl_const_BD_Shape_double_t x); int ppl_BD_Shape_double_ascii_dump (ppl_const_BD_Shape_double_t x, FILE* stream); int ppl_BD_Shape_double_ascii_load (ppl_BD_Shape_double_t x, FILE* stream);
# 15157 "/usr/local/include/ppl_c.h"
int
ppl_delete_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t ph);





int
ppl_new_Octagonal_Shape_double_from_space_dimension
(ppl_Octagonal_Shape_double_t* pph, ppl_dimension_type d, int empty);






int
ppl_new_Octagonal_Shape_double_from_C_Polyhedron
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Polyhedron_t ph);


int
ppl_new_Octagonal_Shape_double_from_Grid
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Grid_t ph);


int
ppl_new_Octagonal_Shape_double_from_Rational_Box
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Rational_Box_t ph);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph);


int
ppl_new_Octagonal_Shape_double_from_Double_Box
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Double_Box_t ph);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_double
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_double_t ph);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_double_t ph);







int
ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Polyhedron_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Grid_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Grid_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Rational_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpz_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_mpq_class_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Double_Box_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_BD_Shape_double_t ph, int complexity);


int
ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Octagonal_Shape_double_t ph, int complexity);







int
ppl_new_Octagonal_Shape_double_from_Constraint_System
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_double_from_Congruence_System
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_double_from_Generator_System
(ppl_Octagonal_Shape_double_t* pph, ppl_const_Generator_System_t cs);
# 15316 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_double_space_dimension
(ppl_const_Octagonal_Shape_double_t ph, ppl_dimension_type* m);


int
ppl_Octagonal_Shape_double_affine_dimension
(ppl_const_Octagonal_Shape_double_t ph, ppl_dimension_type* m);






int
ppl_Octagonal_Shape_double_relation_with_Constraint
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_double_relation_with_Generator
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Generator_t c)
                                    ;


int
ppl_Octagonal_Shape_double_relation_with_Congruence
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_double_get_constraints
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_double_get_congruences
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_double_get_minimized_constraints
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Constraint_System_t* pcs)
                                               ;


int
ppl_Octagonal_Shape_double_get_minimized_congruences
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Congruence_System_t* pcs)
                                               ;






int
ppl_Octagonal_Shape_double_is_empty
(ppl_const_Octagonal_Shape_double_t ph);


int
ppl_Octagonal_Shape_double_is_universe
(ppl_const_Octagonal_Shape_double_t ph);


int
ppl_Octagonal_Shape_double_is_bounded
(ppl_const_Octagonal_Shape_double_t ph);


int
ppl_Octagonal_Shape_double_contains_integer_point
(ppl_const_Octagonal_Shape_double_t ph);


int
ppl_Octagonal_Shape_double_is_topologically_closed
(ppl_const_Octagonal_Shape_double_t ph);


int
ppl_Octagonal_Shape_double_is_discrete
(ppl_const_Octagonal_Shape_double_t ph);






int
ppl_Octagonal_Shape_double_topological_closure_assign
(ppl_Octagonal_Shape_double_t ph);






int
ppl_Octagonal_Shape_double_bounds_from_above
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le)
                                             ;


int
ppl_Octagonal_Shape_double_bounds_from_below
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le)
                                             ;






int
ppl_Octagonal_Shape_double_maximize
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;


int
ppl_Octagonal_Shape_double_minimize
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum)



                          ;






int
ppl_Octagonal_Shape_double_maximize_with_point
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;


int
ppl_Octagonal_Shape_double_minimize_with_point
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_n, ppl_Coefficient_t ext_d, int* poptimum, ppl_Generator_t point)




                                  ;






int
ppl_Octagonal_Shape_double_frequency
(ppl_const_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t le, ppl_Coefficient_t ext_fn, ppl_Coefficient_t ext_fd, ppl_Coefficient_t ext_vn, ppl_Coefficient_t ext_vd)




                                     ;





int
ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;





int
ppl_Octagonal_Shape_double_OK
(ppl_const_Octagonal_Shape_double_t ph);





int
ppl_Octagonal_Shape_double_add_constraint
(ppl_Octagonal_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_double_add_congruence
(ppl_Octagonal_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_double_add_constraints
(ppl_Octagonal_Shape_double_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_double_add_congruences
(ppl_Octagonal_Shape_double_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_double_refine_with_constraint
(ppl_Octagonal_Shape_double_t ph, ppl_const_Constraint_t c)
                                     ;


int
ppl_Octagonal_Shape_double_refine_with_congruence
(ppl_Octagonal_Shape_double_t ph, ppl_const_Congruence_t c)
                                     ;






int
ppl_Octagonal_Shape_double_refine_with_constraints
(ppl_Octagonal_Shape_double_t ph, ppl_const_Constraint_System_t cs)
                                             ;


int
ppl_Octagonal_Shape_double_refine_with_congruences
(ppl_Octagonal_Shape_double_t ph, ppl_const_Congruence_System_t cs)
                                             ;






int
ppl_Octagonal_Shape_double_intersection_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_upper_bound_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_difference_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_concatenate_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;


int
ppl_Octagonal_Shape_double_time_elapse_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_upper_bound_assign_if_exact
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_simplify_using_context_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;





int
ppl_Octagonal_Shape_double_constrains
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_double_unconstrain_space_dimension
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var)
                                   ;





int
ppl_Octagonal_Shape_double_unconstrain_space_dimensions
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_double_affine_image
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;


int
ppl_Octagonal_Shape_double_affine_preimage
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)


                                      ;






int
ppl_Octagonal_Shape_double_bounded_affine_image
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_double_bounded_affine_preimage
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, ppl_const_Linear_Expression_t lb, ppl_const_Linear_Expression_t ub, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_double_generalized_affine_image
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;


int
ppl_Octagonal_Shape_double_generalized_affine_preimage
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type var, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t le, ppl_const_Coefficient_t d)



                                      ;






int
ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs
(ppl_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;


int
ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs
(ppl_Octagonal_Shape_double_t ph, ppl_const_Linear_Expression_t lhs, enum ppl_enum_Constraint_Type relsym, ppl_const_Linear_Expression_t rhs)


                                              ;






int
ppl_Octagonal_Shape_double_add_space_dimensions_and_embed
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type d)
                                 ;


int
ppl_Octagonal_Shape_double_add_space_dimensions_and_project
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type d)
                                 ;






int
ppl_Octagonal_Shape_double_remove_space_dimensions
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type ds[], size_t n)

                     ;





int
ppl_Octagonal_Shape_double_remove_higher_space_dimensions
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type d)
                                 ;





int
ppl_Octagonal_Shape_double_expand_space_dimension
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type d, ppl_dimension_type m)

                                 ;





int
ppl_Octagonal_Shape_double_fold_space_dimensions
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type ds[], size_t n, ppl_dimension_type d)


                                 ;





int
ppl_Octagonal_Shape_double_map_space_dimensions
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type maps[], size_t n)

                     ;





int
ppl_Octagonal_Shape_double_drop_some_non_integer_points
(ppl_Octagonal_Shape_double_t ph, int complexity)
                           ;





int
ppl_Octagonal_Shape_double_drop_some_non_integer_points_2
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type ds[], size_t n, int complexity)


                           ;







int
ppl_Octagonal_Shape_double_external_memory_in_bytes
(ppl_const_Octagonal_Shape_double_t ps, size_t* sz)
                       ;


int
ppl_Octagonal_Shape_double_total_memory_in_bytes
(ppl_const_Octagonal_Shape_double_t ps, size_t* sz)
                       ;






int
ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_double_BHMZ05_widening_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_widening_assign_with_tokens
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, unsigned* tp)

                         ;





int
ppl_Octagonal_Shape_double_widening_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;





int
ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;


int
ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, ppl_const_Constraint_System_t cs, unsigned* tp)


                         ;
# 15936 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, ppl_const_Constraint_System_t cs)

                                             ;


int
ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, ppl_const_Constraint_System_t cs)

                                             ;
# 15956 "/usr/local/include/ppl_c.h"
int
ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, unsigned* tp)

                         ;






int
ppl_Octagonal_Shape_double_CC76_extrapolation_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_CC76_narrowing_assign
(ppl_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y)
                                                 ;






int
ppl_Octagonal_Shape_double_linear_partition
(ppl_const_Octagonal_Shape_double_t x, ppl_const_Octagonal_Shape_double_t y, ppl_Octagonal_Shape_double_t* p_inters, ppl_Pointset_Powerset_NNC_Polyhedron_t* p_rest)


                                                           ;






int
ppl_Octagonal_Shape_double_wrap_assign
(ppl_Octagonal_Shape_double_t ph, ppl_dimension_type ds[], size_t n, enum ppl_enum_Bounded_Integer_Type_Width w, enum ppl_enum_Bounded_Integer_Type_Representation r, enum ppl_enum_Bounded_Integer_Type_Overflow o, const ppl_const_Constraint_System_t* pcs, unsigned complexity_threshold, int wrap_individually)







                                  ;




int
ppl_new_Octagonal_Shape_double_recycle_Constraint_System
(ppl_Octagonal_Shape_double_t* pph, ppl_Constraint_System_t cs);


int
ppl_new_Octagonal_Shape_double_recycle_Congruence_System
(ppl_Octagonal_Shape_double_t* pph, ppl_Congruence_System_t cs);


int
ppl_new_Octagonal_Shape_double_recycle_Generator_System
(ppl_Octagonal_Shape_double_t* pph, ppl_Generator_System_t cs);







int
ppl_assign_Octagonal_Shape_double_from_Octagonal_Shape_double
(ppl_Octagonal_Shape_double_t dst, ppl_const_Octagonal_Shape_double_t src);






int
ppl_Octagonal_Shape_double_add_recycled_constraints
(ppl_Octagonal_Shape_double_t ph, ppl_Constraint_System_t cs)
                                       ;


int
ppl_Octagonal_Shape_double_add_recycled_congruences
(ppl_Octagonal_Shape_double_t ph, ppl_Congruence_System_t cs)
                                       ;






int
ppl_termination_test_MS_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset);


int
ppl_termination_test_PR_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset, ppl_Generator_t point)
                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset, ppl_Generator_t point)
                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset, ppl_Polyhedron_t ph)
                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_double
(ppl_const_Octagonal_Shape_double_t pset, ppl_Polyhedron_t ph)
                                ;







int
ppl_termination_test_MS_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after);


int
ppl_termination_test_PR_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after);







int
ppl_one_affine_ranking_function_MS_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after, ppl_Generator_t point)

                                  ;


int
ppl_one_affine_ranking_function_PR_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after, ppl_Generator_t point)

                                  ;







int
ppl_all_affine_ranking_functions_MS_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after, ppl_Polyhedron_t ph)

                                ;


int
ppl_all_affine_ranking_functions_PR_Octagonal_Shape_double_2
(ppl_const_Octagonal_Shape_double_t pset_before, ppl_const_Octagonal_Shape_double_t pset_after, ppl_Polyhedron_t ph)

                                ;







int ppl_io_print_Octagonal_Shape_double (ppl_const_Octagonal_Shape_double_t x); int ppl_io_fprint_Octagonal_Shape_double (FILE* stream, ppl_const_Octagonal_Shape_double_t x); int ppl_io_asprint_Octagonal_Shape_double (char** strp, ppl_const_Octagonal_Shape_double_t x); int ppl_Octagonal_Shape_double_ascii_dump (ppl_const_Octagonal_Shape_double_t x, FILE* stream); int ppl_Octagonal_Shape_double_ascii_load (ppl_Octagonal_Shape_double_t x, FILE* stream);
