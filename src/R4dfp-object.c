/* Kevin P. Barry [ta0kira@users.berlios.de], BMCLAB, 23 Jun 2010 */

#include "R4dfp-object.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <R.h>
#include <Rdefines.h>

#include "Getifh.h"

#define CEILING_TO(val,denom) ( ((val) % (denom))? (((val) / (denom) + 1) * denom) : (val) )
#define HEADER_EXT ".4dfp.ifh"
#define IMAGE_EXT  ".4dfp.img"


void R4dfp_free(R4dfp_object *hHeader)
{
	if (!hHeader) return;
	if (hHeader->image.mtype && hHeader->image.array && hHeader->image.msize)
		munmap(hHeader->image.array, hHeader->image.msize);

	else
		free(hHeader->image.array);

	free(hHeader->file);

	hHeader->image.array = NULL;
	hHeader->image.size  = 0;
	hHeader->image.msize = 0;
	hHeader->image.mtype = 0;
	hHeader->file        = NULL;

	free((void*) hHeader);
}


R4dfp_object *R4dfp_load(const char *fFile, int oOptions)
{
	char *header_file, *image_file;
	get_file_names(fFile, &header_file, &image_file);


	IFH header;
	if (Getifh(header_file, &header) != 0)
	{
		free(header_file);
		free(image_file);
		return NULL;
	}


	R4dfp_object *new_image = calloc(1, sizeof(R4dfp_object));
	if (!new_image)
	{
		error("unable to allocate image object:");
//fprintf(stderr, "unable to allocate image object: %s\n", strerror(errno));
		free(header_file);
		free(image_file);
		return NULL;
	}
	new_image->header = header;
	new_image->file   = header_file; /*('new_image' now owns the pointer)*/

	long expected_size = new_image->header.matrix_size[0] * new_image->header.matrix_size[1] *
		new_image->header.matrix_size[2] * new_image->header.matrix_size[3];


	if (oOptions)
	{
		int open_flags = 0x00;
		switch(oOptions & (PROT_READ | PROT_WRITE))
		{
		case PROT_READ:  open_flags = O_RDONLY; break;
		case PROT_WRITE: open_flags = O_WRONLY; break;
		default:         open_flags = O_RDWR;   break;
		}

		int image = open(image_file, open_flags);
		if (image < 0)
		{
			error("unable to open image file: %s\n", image_file);
//fprintf(stderr, "unable to open image file: %s\n", strerror(errno));
			free(image_file);
			R4dfp_free(new_image);
			return NULL;
		}

		free(image_file);

		struct stat image_stat;
		if (fstat(image, &image_stat) != 0)
		{
			//fprintf(stderr, "unable to stat image file: %s\n", strerror(errno));
			error("Unable to stat image file\n");
			close(image);
			R4dfp_free(new_image);
			return NULL;
		}

		if (image_stat.st_size != expected_size * sizeof(float))
		{
			error("image data size doesn't match dimensions given in image header");
			//fprintf(stderr, "image data size (%i) doesn't match dimensions given in image header ([%i,%i,%i,%i]=%i)\n", (int) (image_stat.st_size / sizeof(float)), new_image->header.matrix_size[0], new_image->header.matrix_size[1],new_image->header.matrix_size[2], new_image->header.matrix_size[3], (int) expected_size);
			close(image);
			R4dfp_free(new_image);
			return NULL;
		}

		new_image->image.size  = image_stat.st_size;
		new_image->image.psize = sysconf(_SC_PAGE_SIZE);
		new_image->image.msize = CEILING_TO(new_image->image.size, new_image->image.psize);

		new_image->image.array = (float*) mmap(NULL, new_image->image.msize, oOptions, MAP_SHARED, image, 0);
		if (new_image->image.array == (void*) -1)
		{
			//fprintf(stderr, "unable to map image file: %s\n", strerror(errno));
			error("Unable to map image files\n");
			new_image->image.array = NULL;
			close(image);
			R4dfp_free(new_image);
			return NULL;
		}
		new_image->image.mtype = oOptions;

		close(image);
	}


	else
	{
		int image = open(image_file, O_RDONLY);
		if (image < 0)
		{
			error("Unable to open image file");
			//fprintf(stderr, "unable to open image file: %s\n", strerror(errno));
			free(image_file);
			R4dfp_free(new_image);
			return NULL;
		}

		free(image_file);

		new_image->image.array = malloc(expected_size * sizeof(float));
		if (!new_image->image.array)
		{
			//fprintf(stderr, "unable to allocate image: %s\n", strerror(errno));
			error("Unable to allocate image\n");
			close(image);
			R4dfp_free(new_image);
			return NULL;
		}

		if (read(image, new_image->image.array, expected_size * sizeof(float)) != expected_size * sizeof(float))
		{
			//fprintf(stderr, "unable to read image: %s\n", strerror(errno));
			error("Unable to read image\n");
			close(image);
			R4dfp_free(new_image);
			return NULL;
		}

		close(image);
		new_image->image.size = expected_size * sizeof(float);
	}

	union {
		unsigned char byte;
		short         word;
	} test;

	test.word = 01;

	new_image->image.swap = test.byte ^ (strcmp(new_image->header.imagedata_byte_order, "littleendian") == 0);


	return new_image;
}


int R4dfp_save(R4dfp_object *hHeader)
{
	if (!hHeader) return -1;

	if (hHeader->image.mtype)
	{
		error("can't save a directly-mapped image\n");
		return -1;
	}

	union {
		unsigned char byte;
		short         word;
	} test;

	test.word = 01;
	if (Writeifh("", hHeader->file, &hHeader->header, (test.byte ^ hHeader->image.swap)? 'l' : 'b') != 0)
	{
		return -1;
	}

	char *image_file;
	get_file_names(hHeader->file, NULL, &image_file);

	int image = open(image_file, O_WRONLY | O_CREAT | O_TRUNC, 0666);
	if (image < 0)
	{
		//fprintf(stderr, "unable to open image file: %s\n", strerror(errno));
		error("Unable to open image file\n");
		free(image_file);
		return -1;
	}

	if (write(image, hHeader->image.array, hHeader->image.size) != hHeader->image.size)
	{
		//fprintf(stderr, "unable to write image: %s\n", strerror(errno));
		error("Unable to write image\n");
		close(image);
		free(image_file);
		return -1;
	}

	close(image);
	free(image_file);
	return 0;
}


void get_file_names(const char *fFile, char **hHeader, char **iImage)
{
	int original_length = strlen(fFile);
	int extension_index = original_length;


	if (original_length > strlen(HEADER_EXT) &&
	    strcmp(HEADER_EXT, fFile + original_length - strlen(HEADER_EXT)) == 0)
		extension_index = original_length - strlen(HEADER_EXT);

	else if (original_length > strlen(IMAGE_EXT) &&
		 strcmp(IMAGE_EXT, fFile + original_length - strlen(IMAGE_EXT)) == 0)
		extension_index = original_length - strlen(IMAGE_EXT);

	if (hHeader)
	{
		*hHeader = calloc(1, extension_index + strlen(HEADER_EXT) + 1);
		strncpy(*hHeader, fFile, extension_index);
		strncpy(*hHeader + extension_index, HEADER_EXT, strlen(HEADER_EXT));
	}

	if (iImage)
	{
		*iImage = calloc(1, extension_index + strlen(IMAGE_EXT) + 1);
		strncpy(*iImage, fFile, extension_index);
		strncpy(*iImage + extension_index, IMAGE_EXT, strlen(IMAGE_EXT));
	}
}


SEXP R4dfp_to_R(R4dfp_object *hHeader)
{
	SEXP R4dfp_image;

	PROTECT(R4dfp_image = allocVector(VECSXP, 6));

	SET_VECTOR_ELT(R4dfp_image, R4DFP_INTERNAL, R_MakeExternalPtr(hHeader, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(VECTOR_ELT(R4dfp_image, R4DFP_INTERNAL), (R_CFinalizer_t) &R4dfp_R_free);

	SET_VECTOR_ELT(R4dfp_image, R4DFP_FILE, mkString(hHeader->file? hHeader->file : "")); /*file*/
	SET_VECTOR_ELT(R4dfp_image, R4DFP_DIM, allocVector(INTSXP, 4)); /*dim*/
	INTEGER(VECTOR_ELT(R4dfp_image, R4DFP_DIM))[0] = hHeader->header.matrix_size[0];
	INTEGER(VECTOR_ELT(R4dfp_image, R4DFP_DIM))[1] = hHeader->header.matrix_size[1];
	INTEGER(VECTOR_ELT(R4dfp_image, R4DFP_DIM))[2] = hHeader->header.matrix_size[2];
	INTEGER(VECTOR_ELT(R4dfp_image, R4DFP_DIM))[3] = hHeader->header.matrix_size[3];

	SET_VECTOR_ELT(R4dfp_image, R4DFP_SCALE, allocVector(REALSXP, 4)); /*scale*/
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_SCALE))[0] = hHeader->header.scaling_factor[0];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_SCALE))[1] = hHeader->header.scaling_factor[1];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_SCALE))[2] = hHeader->header.scaling_factor[2];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_SCALE))[3] = hHeader->header.scaling_factor[3];

	SET_VECTOR_ELT(R4dfp_image, R4DFP_MMPPIX, allocVector(REALSXP, 3)); /*mmpppix*/
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_MMPPIX))[0] = hHeader->header.mmppix[0];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_MMPPIX))[1] = hHeader->header.mmppix[1];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_MMPPIX))[2] = hHeader->header.mmppix[2];

	SET_VECTOR_ELT(R4dfp_image, R4DFP_CENTER, allocVector(REALSXP, 3)); /*center*/
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_CENTER))[0] = hHeader->header.center[0];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_CENTER))[1] = hHeader->header.center[1];
	REAL(VECTOR_ELT(R4dfp_image, R4DFP_CENTER))[2] = hHeader->header.center[2];

	UNPROTECT(1);
	return R4dfp_image;
}


void R4dfp_update_file(SEXP oObject)
{
	R4dfp_object *header = R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header) return;
	if (!header->image.mtype)
	{
		free(header->file);
		header->file = strdup(CHAR(CHARACTER_POINTER(VECTOR_ELT(oObject, R4DFP_FILE))[0]));
	}
	else
		SET_VECTOR_ELT(oObject, R4DFP_FILE, mkString(header->file? header->file : ""));
}


void R4dfp_clear(SEXP oObject)
{
	R_ClearExternalPtr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	SET_VECTOR_ELT(oObject, R4DFP_INTERNAL, R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
	SET_VECTOR_ELT(oObject, R4DFP_INTERNAL,   R_NilValue);
	SET_VECTOR_ELT(oObject, R4DFP_FILE,       R_NilValue); /*file*/
	SET_VECTOR_ELT(oObject, R4DFP_DIM,        R_NilValue); /*dim*/
	SET_VECTOR_ELT(oObject, R4DFP_SCALE,      R_NilValue); /*scale*/
	SET_VECTOR_ELT(oObject, R4DFP_MMPPIX,     R_NilValue); /*mmpppix*/
	SET_VECTOR_ELT(oObject, R4DFP_CENTER,     R_NilValue); /*center*/
}


SEXP R4dfp_R_free(SEXP hHeader)
{
	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(hHeader);
	R4dfp_free(header);
	return R_NilValue;
}
