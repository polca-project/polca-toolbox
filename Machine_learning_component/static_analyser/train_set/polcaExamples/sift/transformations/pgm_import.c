/*
 * Copyright (c) 2014, Recore Systems B.V., The Netherlands,
 * web: www.recoresystems.com, email: info@recoresystems.com
 *
 * Any reproduction in whole or in parts is prohibited
 * without the written consent of the copyright owner.
 *
 * All Rights Reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "pgm_import.h"

GraylevelImage DefaultPgmImage = {0, 0, 0, NULL};

int readPgm(const char *fileName, GraylevelImage *pGray)
{
  FILE *fp;
  uint8_t magicNumber[2];
  int byte, i, depth;
  int imageSize;
  int retVal = -1;

  assert(pGray->data == NULL);

  fp = fopen(fileName, "rb");
  if (fp == NULL) {
    fprintf(stderr, "PGM read: open file %s failed\n", fileName);
    return retVal;
  }

  if (fread(magicNumber, sizeof(magicNumber), 1, fp) != 1) {
    fprintf(stderr, "PGM read: read of magic number failed\n");
    goto cleanup_file;
  }
  /* Checking for magic numbers P2 or P5 */
  if (magicNumber[0] != 'P' || (magicNumber[1] != '5' && magicNumber[1] != '2')) {
    fprintf(stderr, "PGM read: wrong magic number \n");
    goto cleanup_file;
  }
  while ((byte = fgetc(fp)) != '\n');
  /* Skip comments */
  while ((byte = fgetc(fp)) == '#') {
    while ((byte = fgetc(fp)) != '\n');
  }
  ungetc(byte,fp);
  /* Read image dimensions */
  if (fscanf(fp, "%d %d", &(pGray->width),&(pGray->height)) != 2)  {
    fprintf(stderr, "PGM read: read of image size failed\n");
    goto cleanup_file;
  }
  /* Read the maximum pixel value */
  if (fscanf(fp, "%d", &byte) != 1) {
    fprintf(stderr, "PGM load: read of maximum value failed\n");
    goto cleanup_file;
  }

  if (byte < 256)
    depth = 1;
  else
    depth = 2;
  pGray->depth = depth;
  while ((byte=fgetc(fp)) != '\n');
  imageSize = pGray->width * pGray->height;
  pGray->data = (uint16_t *)malloc(imageSize * sizeof(uint16_t));
  if(pGray->data == NULL) {
    fprintf(stderr, "PGM read: allocation of data memory failed\n");
    goto cleanup_file;
  }
  /* Read pixels */
  for (i = 0; i < imageSize; i++) {
    if (fread(&byte, sizeof(uint8_t)*depth , 1, fp) != 1) {
      fprintf(stderr, "PGM read: read of data failed\n");
      goto cleanup_malloc;
    }
    pGray->data[i] = byte;
  }
  retVal = 0;
  goto cleanup_file;

  cleanup_malloc:
  free(pGray->data);
  pGray->data = NULL;
  cleanup_file:
  fclose(fp);
  return retVal;
}

int writePgm(const char *fileName, GraylevelImage *pGray)
{
  FILE *fp;
  int imageSize, i, byte;
  assert(pGray != NULL);
  assert(pGray->data != NULL);
  imageSize = pGray->width * pGray->height;
  fp = fopen(fileName,"wb");
  if (fp == NULL) {
    fprintf(stderr, "PGM write: open file  %s failed",fileName);
    return -1;
  }
  fprintf(fp, "P5\n"); // Write magic number
  fprintf(fp, "#\n"); // Write comments
  fprintf(fp, "%d %d\n", pGray->width, pGray->height); // Write image dimensions
  fprintf(fp, "%d\n", IMAGE_SCALING(pGray->depth)); // Write maximum pixel value
  /* Write pixels */
  for (i = 0; i < imageSize; i++) {
    byte = pGray->data[i];
    fwrite(&byte, sizeof(uint8_t)*pGray->depth, 1, fp);
  }
  fclose(fp);
  return 0;
}

void freePgm(GraylevelImage *pGray)
{
  assert(pGray->data != NULL);
  free(pGray->data);
  pGray->data = NULL;
}
