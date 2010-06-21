#ifndef R4dfp_object_h
#define R4dfp_object_h

#include <fcntl.h>

#include <Rinternals.h>

#include "ifh.h"

#define R4DFP_INTERNAL 0
#define R4DFP_FILE     1
#define R4DFP_DIM      2
#define R4DFP_SCALE    3
#define R4DFP_MMPPIX   4
#define R4DFP_CENTER   5


typedef struct
{
	float      *array;
	ssize_t     size, msize, psize;
	int         mtype, swap;
} R4dfp_data;


typedef struct
{
	IFH         header;
	R4dfp_data  image;
	char       *file;
} R4dfp_object;


void get_file_names(const char*, char**, char**);

void R4dfp_free(R4dfp_object*);
R4dfp_object *R4dfp_load(const char*, int);
int R4dfp_recycle(R4dfp_object**, int);
int R4dfp_save(R4dfp_object*);

SEXP R4dfp_to_R(R4dfp_object*);
void R4dfp_update_file(SEXP);
void R4dfp_clear(SEXP);
SEXP R4dfp_R_free(SEXP);


static inline int
R4dfp_get_index(int maxX, int maxY, int maxZ, int X, int Y, int Z, int t)
{ return ( X + maxX * ( Y + maxY * ( Z + maxZ * t ) ) ); }


static inline float
*R4dfp_get_voxel(R4dfp_object *hHeader, int X, int Y, int Z, int t)
{
	return (float*) hHeader->image.array +
	  R4dfp_get_index(
	     hHeader->header.matrix_size[0],
	     hHeader->header.matrix_size[1],
	     hHeader->header.matrix_size[2], X, Y, Z, t );
}


static inline const float
*R4dfp_get_voxel_const(const R4dfp_object *hHeader, int X, int Y, int Z, int t)
{
	return (const float*) hHeader->image.array +
	  R4dfp_get_index(
	     hHeader->header.matrix_size[0],
	     hHeader->header.matrix_size[1],
	     hHeader->header.matrix_size[2], X, Y, Z, t );
}


static inline float R4dfp_swap_endian(float vValue)
{
	int I;
	float result = 0.0;
	for (I = 0; I < (signed) sizeof(float); I++)
	*((unsigned char*) &result + I) = *((const unsigned char*) &vValue + sizeof(float) - I - 1);
	return result;
}

#endif /*R4dfp_object_h*/
