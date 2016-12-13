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
#include <math.h>
#include <assert.h>
#include "pgm_import.h"
#include "sift.h"


/* Function to print image */
void printImage(uint8_t width,
    uint8_t height,
    int32_t *pImage)
{
  uint8_t w, h;
  /* Loop over image height */
  for (h = 0; h < height; h++) {
    /* Loop over image width */
    for (w = 0; w < width; w++) {
      printf("%d ", pImage[h * width + w]);
    }
    printf("\n");
  }
  printf("\n");
}

int saveImage(char *fileName,
    uint8_t width,
    uint8_t height,
    uint8_t fracLen,
    int32_t *pImage)
{
  uint16_t i, imageSize;
  uint32_t half = (1u << (fracLen - 1));
  GraylevelImage grayOut = DefaultPgmImage;

  imageSize = width * height;
  grayOut.width = width;
  grayOut.height = height;
  grayOut.depth = 1;
  grayOut.data = (uint16_t *)malloc(imageSize * sizeof(uint16_t));

  /* Scale pixels to 8 bit and copy to grayOut.data */
  for (i = 0; i < imageSize; i++) {
    grayOut.data[i] = (uint16_t)(sadd32((pImage[i] * IMAGE_SCALING(grayOut.depth)),
        half) >> fracLen);
  }
	printf("Writing image to %s\n", fileName);
  if (writePgm(fileName, &grayOut)) {
    printf("ERROR: Could not write to %s\n", fileName);
    return -1;
  }
  freePgm(&grayOut);
  return 0;
}

void getGaussianKernels(InputParams *param)
{
  int32_t x, y, normCoef;
  int32_t scale, filterLen, r;
  double sigma, sigmaPre, temp, k;
  double sigmaSq, preFactor, norm;
  const double CONST_PI = 3.14159265359;
  int32_t maxFilterSize = ((MAX_FILTER_LEN << 1) + 1);
  maxFilterSize *= maxFilterSize;
  double coeffFloat[maxFilterSize];
  k = pow(2, 1./(param->noOfScales - 3));
  sigmaPre = sqrt(k*k - 1.)*param->sigma_init;
  sigma = param->sigma_init;
  /*Filter coefficients */
  for (scale = 0; scale < param->noOfScales; scale++) {
    filterLen = ceil(6*sigma);
    filterLen = (filterLen % 2) ? filterLen : filterLen + 1;
    assert(filterLen < MAX_FILTER_LEN);
    param->filterLen[scale] = filterLen;
    // generate Gaussian kernel
    r = filterLen>>1;
    sigmaSq = 2*sigma*sigma;
    preFactor = 1. / (sigmaSq * CONST_PI);
    norm = 0;
    // Since Gaussian coefficients are symmetric
    for(y = 0; y <= r; ++y) {
      for (x = 0; x <= r; ++x) {
        temp = (x * x) + (y * y);
        temp = temp / sigmaSq;
        temp = preFactor * exp(-temp);
        coeffFloat[y * (r + 1) + x] = temp;
        if(x && y) norm += (temp*4);
        else if(x || y) norm += (temp*2);
        else norm += temp;
      }
    }
    //normalize coefficients
    for(y = 0; y <= r; ++y) {
      for (x = 0; x <= r; ++x) {
        normCoef = double2fix(coeffFloat[y * (r + 1) + x] / norm, param->fracLen);
        param->filter[scale][(r + x) * filterLen + (r + y)] = normCoef;
        param->filter[scale][(r + x) * filterLen + (r - y)] = normCoef;
        param->filter[scale][(r - x) * filterLen + (r + y)] = normCoef;
        param->filter[scale][(r - x) * filterLen + (r - y)] = normCoef;
      }
    }
    sigma = sigmaPre * k;
    sigmaPre = sigma;
  }
}

/* Function to load input parameters */
int loadInputParams(InputParams *pParams, char *inputFileName)
{
  int32_t i;
  int32_t imSize;

  /* Parameters */
  pParams->noOfOctaves = 2;
  pParams->noOfScales = 6;
  pParams->fracLen = 20;
  pParams->sigma_init = 1.6;

  assert(pParams->noOfScales <= MAX_SCALES);
  assert(pParams->noOfOctaves <= MAX_OCTAVES);

  /*import image data*/
  GraylevelImage gray = DefaultPgmImage;
  if (readPgm(inputFileName, &gray)) {
    printf("ERROR: Could not read from %s\n", inputFileName);
    return -1;
  }
  imSize = gray.width * gray.height;
  assert(imSize <= MAX_IMAGE_SIZE);
  pParams->imageWidth = gray.width;
  pParams->imageHeight = gray.height;
  /* Scale pixels to fixed point fractional length and load into input image buffer */
  for (i = 0; i < imSize; i++) {
    pParams->image[i] = fmul32((gray.data[i] << pParams->fracLen),
        (double2fix((1./IMAGE_SCALING(gray.depth)), pParams->fracLen)),
        pParams->fracLen);
  }
  freePgm(&gray);
  //initialize Gaussian Filter Kernels
  getGaussianKernels(pParams); // Calculate Gaussian Filter Kernels

  return 0;
}

/* Function to perform 2D convolution */
void convolution2D(uint8_t width,
    uint8_t height,
    uint8_t filterLen,
    uint8_t fracLen,
    int32_t *pFilter,
    int32_t *pImage,
    int32_t *pGauss)
{
  uint8_t w, h, k, l;
  int16_t x, y, xstart, ystart;
  int32_t sum;
  uint8_t filterLenBy2 = filterLen >> 1;

  /* Loop over processed image height indices */
  for (h = 0; h < height; h++) 
  {
    /* Loop over processed image width indices */
    for (w = 0; w < width; w++) 
    {
      sum = 0; /* Initialize sum to 0 */
      /* Pixel row index to start with */
      ystart = h - filterLenBy2;
      /* Loop over filter length height-wise */
      for (l = 0; l < filterLen; l++) 
      {
        y = ystart;
        ystart++; /* Increment pixel row index */
        /* Pixel column index to start with */
        xstart = w - filterLenBy2;
        /* Loop over filter length width-wise */
        for (k = 0; k < filterLen; k++) 
        {
          x = xstart;
          xstart++; /* Increment pixel column index */
          /* Skip pixels that are outside the image boundary (Zero-padding) */
          if ((x < 0) || (y < 0) || (x > width - 1) || (y > height - 1)) 
            continue;
          /* Multiply image pixel with filter coefficient and add to sum */
          sum = sadd32(sum,
              fmul32(pImage[y * width + x],
                  pFilter[l * filterLen + k],
                  fracLen)
          );
        }
      }
      /* Copy sum to output gaussian image location */
      pGauss[h * width + w] = sum;
    }
  }
}

/* Function to downsample an image by a factor of 2 */
void downsample(uint8_t width,
    uint8_t height,
    int32_t *pInputImage,
    int32_t *pOutputImage)
{
  uint8_t w, h;
  /* Temporary pointers */
  int32_t *pIn;
  int32_t *pOut = pOutputImage;
  /* Loop over image height skipping every other index */
  for (h = 0; h < height; h += 2) 
  {
    /* Set pIn to point to current row */
    pIn = pInputImage + h * width;
    /* Loop over image width skipping every other index */
    for(w = 0; w < width; w += 2) 
    {
      /* Copy input image pixel value to output image */
      *pOut = *pIn;
      ++pOut; /* Increment output image pointer */
      pIn += 2; /* Increment input image pointer */
    }
  }
}

int main(int argc, char* argv[])
{
  if (argc < 2) {
    printf("USAGE: ./sift path/to/input/image\n");
    return -1;
  }
  uint8_t scale, octave;
  uint8_t width, height;
  uint16_t pixel, size;
  InputParams params;
  int32_t gauss[MAX_OCTAVES][MAX_SCALES][MAX_IMAGE_SIZE];
  int32_t dog[MAX_OCTAVES][MAX_SCALES][MAX_IMAGE_SIZE];
  int32_t *pImage;

  char outFileName[80];
  /* Load input parameters into structure */
  if (loadInputParams(&params, argv[1])) {
    printf("ERROR: Invalid Input\n");
    return -1;
  }
  width = params.imageWidth;
  height = params.imageHeight;
  size = width * height;
  /* Set pImage to point to input image */
  pImage = params.image;
  /* Set scale loop index to start from 0 for first octave */
  scale = 0;
  /* Loop over number of octaves */
  for (octave = 0; octave < params.noOfOctaves; octave++) 
  {
    if (octave > 0) {
      scale = 0;
      /* Downsample the (S + 1)th image from previous octave
       * Store the downsampled image as the first gaussian image
       * for current octave
       */
      downsample(width,
          height,
		 gauss[octave - 1][params.noOfScales - (params.noOfScales-1)],
          gauss[octave][scale]);
      /* Adjust image dimensions */
      width = (width % 2) ? (width + 1) >> 1 : width >> 1;
      height = (height % 2) ? (height + 1) >> 1 : height >> 1;
      size = width * height;
      /* First gaussian image is input image for next scale */
      pImage = gauss[octave][scale];
      /* Printing */
      sprintf(outFileName, "data/gauss_%d_%d.pgm", octave, scale);
      saveImage(outFileName, width, height, params.fracLen, gauss[octave][scale]);
      // printf("Gaussian image for octave %d scale 0\n", octave);
      // printImage(width, height, gauss[octave][0]);
      /* Set scale loop index to start from 1 */
      ++scale;
    }
    /* Construct Gaussian images */ 
    for ( ; scale < params.noOfScales; scale++) 
    {
      /* Perform 2D convolution to obtain a gaussian image */
      convolution2D(width,
          height,
          params.filterLen[scale],
          params.fracLen,
          params.filter[scale],
          pImage,
          gauss[octave][scale]);
      /* Current Gaussian image is input image for next scale */
      pImage = gauss[octave][scale];
      /* Printing */
      sprintf(outFileName, "data/gauss_%d_%d.pgm", octave, scale);
      saveImage(outFileName, width, height, params.fracLen, gauss[octave][scale]);
      // printf("Gaussian image for octave %d scale %d\n", octave, scale);
      // printImage(width, height, gauss[octave][scale]);
    }
    /* Construct DoG images */
    for (scale = 0; scale < params.noOfScales - 1; scale++) 
    {
      for (pixel = 0; pixel < size; pixel++) 
      {
        dog[octave][scale][pixel] = gauss[octave][scale + 1][pixel] -
            gauss[octave][scale][pixel];
      }
      /* Printing */
      sprintf(outFileName, "data/dog_%d_%d.pgm", octave, scale);
      saveImage(outFileName, width, height, params.fracLen, dog[octave][scale]);
      // printf("DoG image for octave %d scale %d\n", octave, scale);
      // printImage(width, height, dog[octave][scale]);
    }
  }
  return 0;
}
