
#include "ppmImage.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

size_t typeSize;

int imageSize;

void kernelRedFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = r;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelGreenFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = g;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelBlueFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = b;
  	}
}


void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{

  unsigned rawWidth = width * 3;

  printf("Applying red color filter...\n");
  printf("Applying green color filter...\n");
  printf("Applying blue color filter...\n");


  #pragma polca kernel maxj
  #pragma polca def filterrgb
  #pragma polca map image redImage
  #pragma polca map image greenImage
  #pragma polca map image blueImage
  for (int j = 0; j < height * rawWidth; j++)
    {
      char r,g,b;
      char r1,g1,b1;
      char r2,g2,b2;

      int i;

      i = j % 3;
      
      r = image[j];

      (*redImage)[j] = i==0?r:0;

      r1 = image[j];
      (*greenImage)[j] = i==1?r1:0;

      r2 = image[j];
      (*blueImage)[j] = i==2?r2:0;
    }

}

int main(int argc, char *argv[]){

  const char* filename = argc > 1 ? argv[1] : "../../input/test.ppm";
  char* inputImage;
  int width,height;

  LoadPPMImageArray(&inputImage,&width,&height,filename);

  typeSize = sizeof(char);
  imageSize = width * height * 3;  
  char *redImage = malloc( typeSize * imageSize);
  char *blueImage = malloc( typeSize * imageSize);
  char *greenImage = malloc( typeSize * imageSize);

  rgbImageFilter(inputImage,width,height,&redImage,&greenImage,&blueImage);

  SavePPMImageArray(redImage,width,height,"../../output/test_red.ppm");
  SavePPMImageArray(greenImage,width,height,"../../output/test_green.ppm");
  SavePPMImageArray(blueImage,width,height,"../../output/test_blue.ppm");

  return 0;
}
