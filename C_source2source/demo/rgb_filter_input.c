
#include "lodepng.h"
#include "ppmImage.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#pragma polca type_size image
#pragma polca type_size redImage
#pragma polca type_size blueImage
#pragma polca type_size greenImage
size_t typeSize;

#pragma polca total_size image
#pragma polca total_size redImage
#pragma polca total_size blueImage
#pragma polca total_size greenImage
int imageSize; 

void kernelRedFilter(char* image,int i,int rawWidth,char **outputImage){
  int j;

     for(j = 0; j < rawWidth; j+=3)
  	{
  	  char r = image[i*rawWidth+j];
  	  char g = image[i*rawWidth+j+1];
  	  char b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = r;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelGreenFilter(char* image,int i,int rawWidth,char **outputImage){
  int j;

     for(j = 0; j < rawWidth; j+=3)
  	{
  	  char r = image[i*rawWidth+j];
  	  char g = image[i*rawWidth+j+1];
  	  char b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = g;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelBlueFilter(char* image,int i,int rawWidth,char **outputImage){
  int j;
     for(j = 0; j < rawWidth; j+=3)
  	{
  	  char r = image[i*rawWidth+j];
  	  char g = image[i*rawWidth+j+1];
  	  char b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = b;
  	}
}


void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{

  unsigned rawWidth = width * 3;
  int i;

  printf("Applying red color filter...\n");

  #pragma polca kernel opencl
  #pragma polca kernel mpi
  #pragma polca kernel omp
  #pragma polca kernel maxj
  #pragma polca def redFilter
  #pragma polca map image redImage
  for (i = 0; i < height; i++)
    {
      kernelRedFilter(image,i,rawWidth,redImage);
    }
  printf("Applying green color filter...\n");

  #pragma polca kernel opencl
  #pragma polca kernel mpi
  #pragma polca kernel omp
  #pragma polca kernel maxj
  #pragma polca def greenFilter
  #pragma polca map image greenImage
  for (i = 0; i < height; i++)
    {
      kernelGreenFilter(image,i,rawWidth,greenImage);
    }

  printf("Applying blue color filter...\n");

  #pragma polca kernel opencl
  #pragma polca kernel mpi
  #pragma polca kernel omp
  #pragma polca kernel maxj
  #pragma polca def blueFilter
  #pragma polca map image blueImage
  for (i = 0; i < height; i++)
    {
      kernelBlueFilter(image,i,rawWidth,blueImage);
    }

}

int main(int argc, char *argv[]){

  const char* filename = argc > 1 ? argv[1] : "../input/test.ppm";
  char* inputImage;
  int width,height;

  #pragma polca io 
  #pragma polca init width
  #pragma polca init height
  #pragma polca init image
  LoadPPMImageArray(&inputImage,&width,&height,filename);

  typeSize = sizeof(char);
  #pragma polca init imageSize
  imageSize = width * height * 3;  
  char *redImage = malloc( typeSize * imageSize);
  char *blueImage = malloc( typeSize * imageSize);
  char *greenImage = malloc( typeSize * imageSize);

  rgbImageFilter(inputImage,width,height,&redImage,&greenImage,&blueImage);
  /* redcolorFilterImage(filename,"../output/img_redcolor.png"); */
  /* testFilterImage(filename,"../output/img_test.png"); */
  
  /* testFilterImage(inputImage,width,height,&blueImage); */
  #pragma polca io 
  {
    SavePPMImageArray(redImage,width,height,"../output/test_red.ppm");
    SavePPMImageArray(greenImage,width,height,"../output/test_green.ppm");
    SavePPMImageArray(blueImage,width,height,"../output/test_blue.ppm");
  }

  return 0;
}

