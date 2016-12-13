// TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 0]
// TEST_LABEL: 0 (CPU)

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
  int i,j;

  printf("Applying red color filter...\n");
  printf("Applying green color filter...\n");
  printf("Applying blue color filter...\n");

  for (i = 0; i < height; i++)
    {
      char r,g,b;
      char r1,g1,b1;
      char r2,g2,b2;

      for(j = 0; j < (rawWidth/3); j++)
      {
	  r = image[i*rawWidth+(j*3)];
  	  g = image[i*rawWidth+(j*3)+1];
  	  b = image[i*rawWidth+(j*3)+2];

  	  (*redImage)[i*rawWidth+(j*3)] = r;
  	  (*redImage)[i*rawWidth+(j*3)+1] = 0;
  	  (*redImage)[i*rawWidth+(j*3)+2] = 0;

  	  r1 = image[i*rawWidth+(j*3)];
  	  g1 = image[i*rawWidth+(j*3)+1];
  	  b1 = image[i*rawWidth+(j*3)+2];

  	  (*greenImage)[i*rawWidth+(j*3)] = 0;
  	  (*greenImage)[i*rawWidth+(j*3)+1] = g1;
  	  (*greenImage)[i*rawWidth+(j*3)+2] = 0;

  	  r2 = image[i*rawWidth+(j*3)];
  	  g2 = image[i*rawWidth+(j*3)+1];
  	  b2 = image[i*rawWidth+(j*3)+2];

  	  (*blueImage)[i*rawWidth+(j*3)] = 0;
  	  (*blueImage)[i*rawWidth+(j*3)+1] = 0;
  	  (*blueImage)[i*rawWidth+(j*3)+2] = b2;

      }      

    }

}
